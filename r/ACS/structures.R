library(tidyverse)
library(tidycensus)
library(hdatools)

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

# Table B25127: Tenure by year structure built by units in structure

years <- 2019:2022

b25127_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25127")

b25127_raw <- map_dfr(years, function(yr){
  b25127_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25127",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25127_raw <- b25127_raw |> 
  subset(GEOID %in% cv) 

b25127_vars_cleaned <- b25127_vars |> 
  separate(label, into = c("est", "total", "tenure", "yrbuilt", "structure"), sep = "!!") |> 
  select(variable = name, tenure, yrbuilt, structure) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25127_raw <- b25127_raw |> 
  right_join(b25127_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate, moe)

b25127_data <- b25127_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         yrbuilt = str_remove_all(yrbuilt, "Built ")) |>
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate) |> 
  mutate(structure = case_when(
    structure == "1, detached  or attached" ~ "Single-family",
    structure == "2 to 4" ~ "2 to 4 units",
    structure == "5 to 19" ~ "5 to 19 units",
    structure == "20 to 49" ~ "20 or more units",
    structure == "50 or more" ~ "20 or more units",
    TRUE ~ "Other"
  )) 


write_rds(b25127_data, "data/b25127.rds")

# Table B25032: Tenure by units in structure

b25032_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25032")

b25032_raw <- map_dfr(years, function(yr){
  b25032_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25032",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25032_raw <- b25032_raw |> 
  subset(GEOID %in% cv) 

b25032_vars_cleaned <- b25032_vars |> 
  filter(str_detect(name, "B25032_")) |> 
  separate(label, into = c("est", "total", "tenure", "structure"), sep = "!!") |> 
  select(variable = name, tenure, structure) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner-occupied housing units" ~ "Homeowner",
    tenure == "Renter-occupied housing units" ~ "Renter"
  )) |> 
  mutate(
    structure = fct_case_when(
      structure == "1, detached" ~ "Single-family detached",
      structure == "1, attached" ~ "Single-family attached",
      structure %in% c("2", "3 or 4") ~ "2 to 4 units",
      structure %in% c("5 to 9", "10 to 19") ~ "5 to 19 units",
      structure %in% c("20 to 49", "50 or more") ~ "20 units or more",
      structure == "Mobile home" ~ "Manufactured home",
      structure == "Boat, RV, van, etc." ~ "Other"
    )
  )

b25032_raw <- b25032_raw |> 
  right_join(b25032_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, structure, estimate, moe)

b25032_data <- b25032_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>
  select(NAME, GEOID, year, tenure, structure, estimate) |> 
  group_by(NAME, year, tenure, structure) |> 
  summarize(estimate = sum(estimate)) |> 
  ungroup()

write_rds(b25032_data, "data/b25032.rds")

# Table B25096: Tenure by units in structure

b25096_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25096")

b25096_raw <- map_dfr(years, function(yr){
  b25096_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25096",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25096_raw <- b25096_raw |> 
  subset(GEOID %in% cv) 

b25096_vars_cleaned <- b25096_vars |> 
  separate(label, into = c("est", "total", "mortgage", "value"), sep = "!!") |> 
  select(variable = name, mortgage, value) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(
    value = fct_case_when(
      value %in% c("Less than $50,000", "$50,000 to $99,999") ~ "Less than $100,000",
      value %in% c("$100,000 to $149,999", "$150,000 to $199,999") ~ "$100,000 to $199,999",
      value == "$200,000 to $299,999" ~ "$200,000 to $299,999",
      value == "$300,000 to $499,999" ~ "$300,000 to $499,999",
      value == "$500,000 to $749,999" ~ "$500,000 to $749,999",
      value == "$750,000 to $999,999" ~ "$750,000 to $999,999",
      value == "$1,000,000 or more" ~ "$1,000,000 or more"
    )
  )

b25096_raw <- b25096_raw |> 
  right_join(b25096_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, mortgage, value, estimate, moe)

b25096_data <- b25096_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>
  select(NAME, GEOID, year, mortgage, value, estimate) |> 
  group_by(NAME, year, mortgage, value) |> 
  summarize(estimate = sum(estimate)) |> 
  ungroup()

write_rds(b25096_data, "data/b25096.rds")

# Table B25004: Vacancy status

b25004_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25004")

b25004_raw <- map_dfr(years, function(yr){
  b25004_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25004",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25004_raw <- b25004_raw |> 
  subset(GEOID %in% cv) 

b25004_vars_cleaned <- b25004_vars |> 
  separate(label, into = c("est", "total", "status"), sep = "!!") |> 
  select(variable = name, status) |> 
  drop_na()

b25004_raw <- b25004_raw |> 
  right_join(b25004_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, status, estimate, moe)

b25004_data <- b25004_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>
  select(NAME, GEOID, year, status, estimate)

write_rds(b25004_data, "data/b25004.rds")
