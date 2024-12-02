library(tidycensus)
library(tidyverse)

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2019:2022

b25042_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25042")

b25042_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25042",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25042_raw <- b25042_raw |> 
  subset(GEOID %in% cv)

b25042_vars_cleaned <- b25042_vars |> 
  separate(label, into = c("est", "total", "tenure", "br"), sep = "!!") |> 
  select(variable = name, tenure, br) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25042_raw <- b25042_raw |> 
  right_join(b25042_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, br, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"))

write_rds(b25042_raw, "data/b25042.rds")





# Table B25063: Gross Rent

years <- 2022

b25063_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25063")

b25063_raw <- map_dfr(years, function(yr) {
  b25063_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25063",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25063_raw <- b25063_raw |> 
  subset(GEOID %in% cv)

b25063_vars_cleaned <- b25063_vars |> 
  separate(label, into = c("est", "total", "type", "rent"), sep = "!!") |> 
  mutate(
    rent = case_when(
      type == "No cash rent" ~ type,
      .default = rent
    )
  ) |> 
  select(variable = name, rent) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":")))

b25063_raw <- b25063_raw |> 
  right_join(b25063_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, rent, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"))

b25063_fct <- b25063_raw |> 
  mutate(
    rent = fct_case_when(
      rent %in% b25063_vars_cleaned$rent[1:14] ~ "Less than $749",
      rent %in% b25063_vars_cleaned$rent[14:17] ~ "$750 to $999",
      rent %in% b25063_vars_cleaned$rent[18:19] ~ "$1,000 to $1,499",
      rent %in% b25063_vars_cleaned$rent[20] ~ "$1,500 to $1,999",
      rent %in% b25063_vars_cleaned$rent[21] ~ "$2,000 to $2,499",
      rent %in% b25063_vars_cleaned$rent[22:24] ~ "$2,500 or more",
      rent %in% b25063_vars_cleaned$rent[25] ~ "No cash rent"
    )
  ) |> 
  summarise(
    estimate = sum(estimate),
    .by = c(NAME, year, rent)
  )

write_rds(b25063_fct, "data/b25063.rds")
