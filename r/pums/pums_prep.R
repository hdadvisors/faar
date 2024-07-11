## Prepare PUMS Data for Housing Spectrum -----------------


## 1. Setup -----------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(survey)

pums_raw <- read_rds("data/pums/pums_raw.rds")
pums_wgt <- read_rds("data/pums/pums_wgt.rds")
pums_labels <- read_rds("data/pums/pums_labels.rds")
faar_ami <- read_rds("data/pums/faar_ami_pums.rds")


## 2. Join AMI limits to PUMS data ------------------------

# Prepare AMI limits to join
faar_ami_join <- faar_ami |> 
  mutate(NP = as.numeric(str_sub(hh_size, end = 1))) |> 
  select(6, 4, 5) |> 
  pivot_wider(names_from = level, values_from = income)

# Create list of AMI levels
ami_levels <- c(
  "Zero or negative income", "Below 30% AMI", "30-50% AMI",
  "50-80% AMI", "80-100% AMI", "100-120% AMI", "Above 120% AMI"
  )

# Join AMI limits and categorize
faar_pums_ami <- pums_raw |>
  filter(!TEN == "b") |> 
  mutate(NP = pmin(NP, 8)) |> 
  left_join(faar_ami_join, by = "NP") |>
  mutate(
    ami_faar = factor(
      case_when(
        HINCP < 1 ~ ami_levels[1],
        HINCP <= ami30 ~ ami_levels[2],
        HINCP <= ami50 ~ ami_levels[3],
        HINCP <= ami80 ~ ami_levels[4],
        HINCP <= ami100 ~ ami_levels[5],
        HINCP <= ami120 ~ ami_levels[6],
        TRUE ~ ami_levels[7]
      ),
      levels = ami_levels,
      ordered = TRUE
    )
  ) |> 
  select(-ami30, -ami50, -ami80, -ami100, -ami120)


## 3. Calculate number of earners in household ------------

# Identify persons with wages or salary at least $5000 in last 12 months
faar_pums_earners <- faar_pums_ami |> 
  group_by(SERIALNO) |> 
  mutate(
    hh_earners = sum(WAGP >= 5000)
  ) |> 
  ungroup()


## 4. Add place of work labels ----------------------------

powpuma_lookup <- read_rds("data/pums/powpuma_lookup.rds") |> 
  select(5:6)

faar_pums_pow <- faar_pums_earners |> 
  # Clean state POW column
  mutate(
    POWSP = case_when(
      POWSP == "00N" ~ NA,
      str_starts(POWSP, "0") ~ str_sub(POWSP, start = 2, end = 3),
      .default = POWSP
    )
  ) |> 
  # Create POW GEOID column
  mutate(
    pow_geoid = case_when(
      is.na(POWSP) ~ NA,
      POWPUMA10 != "-0009" ~ paste0(POWSP, POWPUMA10),
      POWPUMA20 != "-0009" ~ paste0(POWSP, POWPUMA20)
      
    )
  ) |>
  # Join POW lookup data
  left_join(powpuma_lookup, relationship = "many-to-many") |> 
  # Retain Virginia region labels and add DC/MD labels
  mutate(
    pow_label = case_when(
      POWSP == "51" ~ pow_label,
      POWSP == "11" ~ "Washington, DC",
      POWSP == "24" ~ "Maryland",
      .default = NA
    )
  ) |> 
  # Add 'core_workforce' field to flag HHs with at least 1 worker with POW in FAAR region
  group_by(SERIALNO) |> 
  mutate(
    core_workforce = if_else(
      SPORDER == 1,
      any(pow_label == "Fredericksburg area", na.rm = TRUE),
      NA
    )
  ) |> 
  ungroup()
  

## 5. Recode PUMS data as needed --------------------------

# Use custom function from pums_labels.R
pums_recode <- function(data, vars) {
  
  recoded_data <- data
  
  for (var_name in vars) {
    recoded_data <- recoded_data %>%
      left_join(
        pums_labels %>% 
          filter(var_code == var_name) %>% 
          select(val, val_label),
        by = setNames("val", var_name)
      ) %>%
      mutate(!!var_name := coalesce(val_label, as.character(.data[[var_name]]))) %>%
      select(-val_label)
  }
  
  return(recoded_data)
  
}

# Create list of variables to recode
vars_recode <- c(
  "BLD", "FS", "TEN", "YRBLT", "HHLDRHISP", "HHLDRRAC1P", "HHT2", "MV",
  "WIF", "COW", "HINS3", "HINS4", "DIS", "NAICSP", "SOCP"
)

faar_pums_recode <- pums_recode(faar_pums_pow, vars_recode)


## 6. Rename PUMS variables as needed ---------------------

pums_rename <- function(data) {
  
  data |> 
    rename(
      hh_size = NP,
      bedrooms = BDSP,
      fam_income = FINCP,
      cost_rent = GRNTP,
      hh_income = HINCP,
      children = NOC,
      cost_own = SMOCP,
      age = AGEP,
      ssi = SSIP,
      ss = SSP,
      wages = WAGP,
      str_type = BLD,
      snap = FS,
      tenure = TEN,
      str_yrblt = YRBLT,
      ethnicity = HHLDRHISP,
      race = HHLDRRAC1P,
      hh_type = HHT2,
      moved = MV,
      workers = WIF,
      wkr_class = COW,
      medicare = HINS3,
      medicaid = HINS4,
      disability = DIS,
      naics = NAICSP,
      soc = SOCP
    ) 
  
}

faar_pums_rename <- pums_rename(faar_pums_recode)


## 7. Clean up and reorder columns ------------------------

faar_pums_clean <- faar_pums_rename |> 
  mutate(
    puma = case_when(
      PUMA10 == "-0009" ~ PUMA20,
      .default = PUMA10
    )
  ) |> 
  select(
    1:5,
    # Household info
    "puma",
    "hh_size",
    "hh_type",
    "age",
    "children",
    "race",
    "ethnicity",
    # Income and wages
    "ami_faar",
    "hh_income",
    "fam_income",
    "wages",
    # Housing
    "tenure",
    "str_type",
    "bedrooms",
    "str_yrblt",
    "moved",
    "cost_own",
    "cost_rent",
    # Disability and assistance
    "disability",
    "ssi",
    "ss",
    "snap",
    "medicare",
    "medicaid",
    # Economic
    "workers",
    "hh_earners",
    "wkr_class",
    "pow_label",
    "core_workforce",
    "naics",
    "soc"
  )


## 8. Simplify variable labels ----------------------------

faar_pums_simple <- faar_pums_clean |> 
  mutate(
    hh_type = case_when(
      str_detect(hh_type, "Married|Cohabiting") ~ "Couple",
      str_detect(hh_type, "no spouse/partner present, with children") ~ "Single parent",
      str_detect(hh_type, "living alone") ~ "Individual",
      str_detect(hh_type, "with relatives") ~ "Relatives",
      str_detect(hh_type, "nonrelatives") ~ "Roommates",
      .default = hh_type
    )
  ) |> 
  mutate(
    race = case_when(
      ethnicity != "Not Spanish/Hispanic/Latino" ~ "Hispanic or Latino",
      ethnicity == "Not Spanish/Hispanic/Latino" ~ race
    ),
    race = case_when(
      race == "Black or African American alone" ~ "Black",
      race == "Two or More Races" ~ "Multiracial",
      race == "Some Other Race" ~ "Another race",
      str_detect(race, "American Indian") ~ "American Indian",
      .default = str_remove_all(race, " alone")
    )
  ) |> 
  mutate(
    tenure_detail = case_when(
      tenure == "Rented" ~ "Renter",
      tenure == "Owned with mortgage or loan (include home equity loans)" ~ "Homeowner",
      tenure == "Owned free and clear" ~ "Homeowner (no mortgage)",
      tenure == "Occupied without payment of rent" ~ "Renter (no rent)"
    ), .after = tenure
  ) |> 
  mutate(
    tenure = case_when(
      str_detect(tenure, "(?i)rent") ~ "Renter",
      str_detect(tenure, "Owned") ~ "Homeowner"
    )
  ) |> 
  mutate(
    str_type = case_when(
      str_type == "One-family house detached" ~ "Single-family detached",
      str_type == "One-family house attached" ~ "Townhome",
      str_type == "2 Apartments" ~ "Small multifamily (2-9 units)",
      str_type == "3-4 Apartments" ~ "Small multifamily (2-9 units)",
      str_type == "5-9 Apartments" ~ "Small multifamily (2-9 units)",
      str_type == "10-19 Apartments" ~ "Medium multifamily (10-19 units)",
      str_type == "20-49 Apartments" ~ "Large multifamily (20+ units)",
      str_type == "50 or more apartments" ~ "Large multifamily (20+ units)",
      .default = "Mobile home"
    )
  ) |> 
  mutate(
    str_yrblt = case_when(
      str_detect(str_yrblt, "202") ~ "2020 or after",
      str_detect(str_yrblt, "195|196") ~ "1950-1969",
      str_detect(str_yrblt, "193|194") ~ "1949 or earlier",
      .default = str_yrblt
    )
  ) |> 
  mutate(
    moved = case_when(
      moved == "12 months or less" ~ "Within last year",
      moved == "13 to 23 months" ~ "1-4 years ago",
      moved == "2 to 4 years" ~ "1-4 years ago",
      moved == "5 to 9 years" ~ "5-9 years ago",
      moved == "10 to 19 years" ~ "10-19 years ago",
      moved == "20 to 29 years" ~ "20-29 years ago",
      moved == "30 years or more" ~ "30+ years ago"
    )
  ) |> 
  mutate(
    wkr_class = case_when(
      str_detect(wkr_class, "last worked") ~ "Non-earner",
      str_detect(wkr_class, "without pay") ~ "Non-earner",
      str_detect(wkr_class, "private for-profit") ~ "For-profit",
      str_detect(wkr_class, "not-for-profit") ~ "Non-profit",
      str_detect(wkr_class, "Local|State") ~ "Local or state government",
      str_detect(wkr_class, "Federal") ~ "Federal government",
      str_detect(wkr_class, "Self-employed") ~ "Self-employed"
    )
  )
  
# Possible household typologies...
# 
# Working family
# Single working parent
# DINK
# Active retirees
# Seniors
# Roommates


## 9. Save final PUMS data --------------------------------

# Join with replicate weights
pums_faar <- faar_pums_simple |> 
  left_join(pums_wgt)

# Save RDS file
write_rds(pums_faar, "data/pums/pums_faar.rds")
# read_rds("data/pums/pums_faar.rds")

## EXAMPLE CROSSTAB ---------------------------------------

# Median gross rent for renters who moved into home within less than 5 years ago by AMI
pums_faar |>
  filter(
    SPORDER == 1,
    moved %in% c("Within last year", "1-4 years"),
    tenure == "Renter"
  ) |>
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar) |>
  summarise(
    median = survey_median(cost_rent, vartype = "cv")
  )
  #arrange(desc(median))





  


