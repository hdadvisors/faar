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


## Download geographies -----------------------------------

# 2010 PUMA boundaries
puma_2010 <- pumas(cb = TRUE, year = 2019) 
  
# 2020 PUMA boundaries
puma_2020 <- pumas(cb = TRUE, year = 2020)

# Virginia locality boundaries
va_local <- counties(state = "VA", cb = TRUE, year = 2020)


## Save data for mapping ----------------------------------

puma_2020 |> 
  filter(STATEFP20 == "51") |> 
  select(4, 5) |> 
  mutate(
    gwrc = case_when(
      str_detect(NAMELSAD20, "George Washington Regional Commission") ~ "GWRC",
      .default = "Other"
    ),
    .after = 2
  ) |> 
  st_transform("EPSG:4326") |> 
  write_rds("data/ami/puma_va.rds")

va_local |> 
  select(5, 7) |> 
  st_transform("EPSG:4326") |> 
  write_rds("data/ami/local_va.rds")


## Get PUMAs for FAAR region ------------------------------

# Select 2 PUMAs in Planning District 16 (GWRC)
gwrc <- puma_2020 |> 
  filter(str_detect(NAMELSAD20, "George Washington Regional Commission"))

# View GWRC PUMAs and localities
mapview(va_local) + mapview(gwrc)


## Make PUMA lookup file for place-of-work ----------------

powpuma_lookup <- read_csv("data/pums/puma_pow.csv") |> 
  mutate(pow_geoid = paste0(st, pow_puma)) |> 
  select(1:2, 5:6, 8, 7) |> 
  distinct()

# Save file
write_rds(powpuma_lookup, "data/pums/powpuma_lookup.rds")

