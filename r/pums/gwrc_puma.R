## Setup --------------------------------------------------

library(tidyverse)
library(tigris)
library(sf)
library(mapview)

# Define FAAR region with two PUMAs
faar_2010 <- c("51115", "51120") # 2010 PUMA codes
faar_2020 <- c("17700", "17900") # 2020 PUMA codes

# State FIPS codes for DC, Maryland, and Virginia
dmv <- c("11", "24", "51")

puma_pow <- read_csv("data/pums/puma_pow.csv")


## Download geographies -----------------------------------

# 2010 PUMA boundaries
puma_2010 <- pumas(cb = TRUE, year = 2019) 
  
# 2020 PUMA boundaries
puma_2020 <- pumas(cb = TRUE, year = 2020)

# Virginia locality boundaries
va_local <- counties(state = "VA", cb = TRUE, year = 2020)


## Get PUMAs for FAAR region ------------------------------

# Select 2 PUMAs in Planning District 16 (GWRC)
gwrc <- puma_2020 |> 
  filter(str_detect(NAMELSAD20, "George Washington Regional Commission"))

# View GWRC PUMAs and localities
mapview(va_local) + mapview(gwrc)


## Make PUMA lookup file for place-of-work ----------------

# Filter 2010 PUMAs to DMV and recode
puma_2010_dmv <- puma_2010 |> 
  st_drop_geometry() |> 
  filter(STATEFP10 %in% dmv) |> 
  select(1, 2, 4) |> 
  left_join(
    filter(puma_pow, version == "PUMACE10"),
    by = join_by(PUMACE10 == puma)
    ) |> 
  mutate(
    pow_label = case_when(
      STATEFP10 == "11" ~ "Washington, DC",
      STATEFP10 == "24" ~ "Maryland",
      STATEFP10 == "51" & is.na(version) ~ "Somewhere else in Virginia",
      .default = pow_label
    ),
    version = "PUMACE10"
  ) |> 
  select(4, st = 1, puma = 2, geoid = 3, 5:6)
  
# Filter 2020 PUMAs to DMV and recode
puma_2020_dmv <- puma_2020 |> 
  st_drop_geometry() |> 
  filter(STATEFP20 %in% dmv) |> 
  select(1, 2, 4) |> 
  add_row(STATEFP20 = "51", PUMACE20 = "76000") |> 
  left_join(
    filter(puma_pow, version == "PUMACE20"),
    by = join_by(PUMACE20 == puma)
    ) |> 
  mutate(
    pow_label = case_when(
      STATEFP20 == "11" ~ "Washington, DC",
      STATEFP20 == "24" ~ "Maryland",
      STATEFP20 == "51" & is.na(version) ~ "Somewhere else in Virginia",
      .default = pow_label
    ),
    version = "PUMACE20"
  ) |> 
  select(4, st = 1, puma = 2, geoid = 3, 5:6)

# Combine 2010 and 2020 PUMA POW lookup files
puma_pow_lookup <- bind_rows(puma_2010_dmv, puma_2020_dmv)

# Save file
write_rds(puma_pow_lookup, "data/pums/puma_pow_lookup.rds")

