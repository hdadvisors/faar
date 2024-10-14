## HUD AMI limits for FAAR region -------------------------

# 1. Setup ------------------------------------------------

library(tidyverse)
library(readxl)
library(tigris)

# Load HUD FY 2024 Income Limits and filter to 3 FMR areas in FAAR region

hud_ami <- read_excel("data/raw/hud_ami_fy24.xlsx", sheet = "Section8-FY24") |> 
  filter(
    hud_area_name %in% c(
      "Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area",
      "Caroline County, VA",
      "Culpeper County, VA HUD Metro FMR Area",
      "King George County, VA")
  )

# Save AMI area assignments

hud_ami_areas <- hud_ami |> 
  mutate(
    geoid = str_c(state, county),
    .before = 1
  ) |> 
  select(GEOID = geoid, state, county, County_Name, hud_area_name)

write_rds(hud_ami_areas, "data/hud_ami_areas.rds")

# 2. Recode income limits ---------------------------------

# Note: DC-area 80% limits "capped" to USA MFI per HUD methods

hud_ami_cap <- hud_ami |> 
  select(area = 6, 12:35) |> 
  distinct() |> 
  pivot_longer(
    cols = starts_with("ELI_") | starts_with("l50_") | starts_with("l80_"),
    names_to = c("level", "hh_size"),
    names_pattern = "(.*)_(.*)",
    values_to = "income"
  ) |> 
  mutate(hh_size = paste(hh_size, "person", sep = "-"), .after = 1) |> 
  mutate(level = recode(level, ELI = "ami30", l50 = "ami50", l80 = "ami80"), .after = 3) |> 
  mutate(
    cap = case_when(
      area == "Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area" & level == "ami80" ~ "Capped",
      area == "King George County, VA" & level == "ami80" ~ "Capped",
      .default = "Uncapped"
    )
  ) |> 
  select(1, 5, 3, 2, 4) |> 
  (\(x) bind_rows(
    x,
    x |>
      filter(cap == "Capped") |>
      mutate(level = "ami100")
  ))() |> 
  (\(x) bind_rows(
    x,
    x |>
      filter(cap == "Capped") |>
      mutate(level = "ami120")
  ))() |> 
  distinct()

write_csv(hud_ami_cap, "hud_ami_cap.csv")


# 3. Create function to calculate higher AMIs -------------

# Create function to calculate income limits based on MFI
calc_ami <- function(mfi, area_name, levels = c(30, 50, 80, 100, 120)) {
  
  # Calculate 4-person Very Low-Income Limit (VLIL)
  vlil <- mfi * 0.5
  
  # Create Family Size Adjustment factors for 1 to 8-person households
  fsa <- c(0.70, 0.80, 0.90, 1, 1.08, 1.16, 1.24, 1.32)
  
  # Calculate AMI levels
  ami_levels <- sapply(levels, function(level) {
    ami <- round(vlil * (level / 50) / 50) * 50
    round(ami * fsa / 50) * 50
  })
  
  # Create tibble with results
  ami <- tibble(
    area = area_name,
    hh_size = paste0(1:8, "-person")
  )
  
  # Add columns for each AMI level
  for (i in seq_along(levels)) {
    ami[[paste0("ami", levels[i])]] <- ami_levels[, i]
  }
  
  # Pivot longer
  ami <- ami |> 
    pivot_longer(-(1:2), names_to = "level", values_to = "income")
  
  return(ami)
}


# 4. Calculate "uncapped" DC-area income limits -----------

dc_ami <- calc_ami(
  154700,
  "Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area",
  c(80, 100, 120)
  ) |> 
  mutate(cap = "Uncapped", .after = 1)


# 5. Calculate Culpeper 100% and 120% income limits -------

culp_ami <- calc_ami(
  110400,
  "Culpeper County, VA HUD Metro FMR Area",
  c(100, 120)
) |> 
  mutate(cap = "Uncapped", .after = 1)


# 6. Calculate Caroline 100% and 120% income limits -------

caro_ami <- calc_ami(
  102800,
  "Caroline County, VA",
  c(100, 120)
) |> 
  mutate(cap = "Uncapped", .after = 1)


# 7. Calculate "uncapped" King George income limits -------

kg_ami <- calc_ami(
  124000,
  "King George County, VA",
  c(80, 100, 120)
) |> 
  mutate(cap = "Uncapped", .after = 1)


# 8. Combine all AMI values -------------------------------

faar_ami_pums <- read_rds("data/pums/faar_ami_pums.rds")

faar_ami_all <- hud_ami_cap |> 
  bind_rows(dc_ami, culp_ami, caro_ami, kg_ami, faar_ami_pums)

write_rds(faar_ami_all, "data/ami/faar_ami_all.rds")


# 9. Get geographies for mapping AMI areas ----------------

hud_ami_counties <- counties(state = c("DC", "MD", "VA"), year = 2021, progress_bar = FALSE) |> 
  select(GEOID, geometry) |> 
  filter(GEOID %in% hud_ami_areas$GEOID) |> 
  left_join(hud_ami_areas) |> 
  mutate(
    faar = case_when(
      GEOID %in% faar_fips ~ "Y",
      .default = "N"
    ), .before = 5
  ) |> 
  st_transform("EPSG:4326")

write_rds(hud_ami_counties, "data/ami/hud_ami_counties.rds")

dc_msa <- core_based_statistical_areas(year = 2021, , progress_bar = FALSE) |> 
  filter(GEOID == "47900") |> 
  st_cast("MULTILINESTRING") |> 
  st_transform("EPSG:4326")

write_rds(dc_msa, "data/ami/dc_msa.rds")
