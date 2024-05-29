library(tidyverse)
library(tidycensus)

# Table B25003: Households by Tenure

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


years <- 2019:2022

b25003_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25003") |> 
  filter(str_length(name) < 11)

b25003_raw <- map_dfr(years, function(yr){
  b25003_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25003",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25003_raw <- b25003_raw |> 
  subset(GEOID %in% cv)


b25003_vars_cleaned <- b25003_vars |> 
  separate(label, into = c("est", "total", "tenure"), sep = "!!") |> 
  select(variable = name, tenure) |>
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

FAAR_hhtenure_data <- b25003_raw |> 
  right_join(b25003_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, estimate, moe) 
