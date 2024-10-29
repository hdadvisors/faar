library(tidyverse)
library(tigris)

faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


bps_years <- 2000:2023

header_rows <- read_csv("https://www2.census.gov/econ/bps/County/co2020a.txt", 
                        col_names = FALSE,
                        n_max = 2)

column_names <- header_rows |>
  select(X1:X18) |>
  t() |>
  as_tibble() |>
  mutate(group = rep(1:6, each = 3)) |>
  group_by(group) |>
  fill(V1, .direction = "updown") |>
  mutate(names = paste0(V1, ": ", V2)) |>
  pull(names)

library(glue)

cbps_raw <- map_df(bps_years, ~{
  raw <- read_csv(glue("https://www2.census.gov/econ/bps/County/co{.x}a.txt"), skip = 2,
                  col_names = FALSE) |>
    select(X1:X18) |>
    set_names(column_names)
  
  raw
  
})


# Read in latest 2024 cumulative data (thru July)

cbps_ytd <- read_csv("https://www2.census.gov/econ/bps/County/co2408y.txt", 
                     col_names = FALSE,
                     skip = 2) |> 
  select(X1:X18) |> 
  set_names(column_names) |> 
  mutate(`Survey: Date` = 2024)


cbps_data <- cbps_raw |> 
  bind_rows(cbps_ytd) |> 
  mutate(year = `Survey: Date`,
         GEOID = paste0(`FIPS: State`, `FIPS: County`)) |>
  select(`1-unit: Bldgs`:GEOID) |>
  filter(GEOID %in% faar) |>
  pivot_longer(`1-unit: Bldgs`:`5+ units: Value`,
               names_to = "type",
               values_to = "value") |>
  separate(type, into = c("Type", "col"), sep = ": ") |>
  pivot_wider(names_from = col,
              values_from = value) |>
  rename_with(tolower, Type:Value) |> 
  select(GEOID, year, type:units)

faar_cbps <- cbps_data |> 
  left_join(counties("VA", year = 2021), by = "GEOID") |> 
  select(GEOID, NAMELSAD, year, type, units) |> 
  group_by(GEOID, NAMELSAD, year, type) |> 
  summarise(units = sum(units)) |> 
  ungroup()


write_rds(faar_cbps, "data/faar_cbps.rds")


faar_cbps |> 
  filter(year > 2015) |> 
  group_by(year) |> 
  summarize(
    total = sum(units)
  )
