library(tidycensus)
library(tidyverse)

# Table B25010: Household size of occupied housing by tenure

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2019:2022

b25010_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25009")

b25010_raw <- map_dfr(years, function(yr){
  b25010_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25010_raw <- b25010_raw |> 
  subset(GEOID %in% cv)

b25010_vars_cleaned <- b25010_vars |> 
  separate(label, into = c("est", "total", "tenure", "size"), sep = "!!") |> 
  select(variable = name, tenure, size)  |> 
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  ))

b25010_data <- b25010_raw |> 
  right_join(b25009_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, size, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  mutate(size = case_when(
    size == "1-person household" ~ "1-person",
    size == "2-person household" ~ "2-person",
    size == "3-person household" ~ "3-person",
    TRUE ~ "4 or more persons"
  )) 
