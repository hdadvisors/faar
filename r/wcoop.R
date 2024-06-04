library(tidyverse)
library(readxl)
library(tidycensus)

local_fips <- c(51177, 51179, 51630, 
                51099, 51033, 51137)

wecoop <- read_excel("data/raw/wecoop_30_50.xlsx", skip = 3) |> 
  rename(fips = 1, name = 2)


va_county_data <- get_decennial(
  geography = "county",
  state = "VA",
  year = 2020,
  variables = "P1_001N" # Total population
)

va_2020 <- va_county_data |> 
  select(fips = GEOID, name = NAME, value) |> 
  mutate(name = str_remove(name, ", Virginia"),
         fips = as.numeric(fips)) |> 
  add_row(fips = 51000, name = "Virginia, Statewide", value = 8631393)

# total_2020 <- sum(va_2020$value, na.rm = TRUE)

wecoop_20_50 <- wecoop |> 
  left_join(va_2020, by = "fips") |> 
  select(fips, name = name.x, `2030`, `2040`, `2050`, `2020` = value) |> 
  pivot_longer(cols = 3:6,
               names_to = "year") |> 
  filter(fips %in% local_fips)

write_rds(wecoop_20_50, "data/wecoop.rds")


