library(tidyverse)
library(tidycensus)

# Table B25003: Households by Tenure

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


years <- 2010:2022

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

b25003_data <- b25003_raw |> 
  right_join(b25003_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, estimate, moe) 

b25003_trend <- b25003_data |> 
  filter(year >= 2016) |> 
  group_by(year, tenure) |> 
  summarise(estimate = sum(estimate)) |>
  group_by(tenure) |> 
  mutate(change = (estimate - lag(estimate)),
         pct_change = change/lag(estimate)) |> 
  group_by(tenure) |> 
  summarise(avg_rate = mean(pct_change, na.rm = TRUE))

b25003_project <- b25003_data  |> 
  group_by(year, tenure) |> 
  summarise(estimate = sum(estimate)) |>
  mutate(rate = case_when(
    tenure == "Renter" ~ 0.01668576,
    tenure == "Homeowner" ~ 0.01549630
  )) |> 
  mutate(projection_2030 = case_when(
    tenure == "Renter" ~ estimate * (1 + rate) ^ 8,
    tenure == "Homeowner" ~ estimate * (1 + rate) ^ 8
  )) |> 
  mutate(projection_2035 = case_when(
    tenure == "Renter" ~ estimate * (1 + rate) ^ 13,
    tenure == "Homeowner" ~ estimate * (1 + rate) ^ 13
  ))  |> 
  mutate(projection_2040 = case_when(
    tenure == "Renter" ~ estimate * (1 + rate) ^ 18,
    tenure == "Homeowner" ~ estimate * (1 + rate) ^ 18
  )) 

write_rds(b25003_project, "data/b25003_project.rds")
  
write_rds(b25003_data, "data/b25003.rds")


# Table B25081: Vacancy status

b25081_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25081")

b25081_raw <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25081",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE
  )

b25081_raw <- b25081_raw |> 
  subset(GEOID %in% cv) 

b25081_vars_cleaned <- b25081_vars |> 
  separate(label, into = c("est", "total", "mortgage", "multiple", "type"), sep = "!!") |> 
  select(variable = name, mortgage, multiple, type) |> 
  drop_na(mortgage) |> 
  mutate(
    mortgage = case_when(
      variable %in% c("B25081_008", "B25081_009") ~ "Without a mortgage",
      .default = "With a mortgage"
    )
  ) |> 
  mutate(
    multiple = case_when(
      variable == "B25081_003" ~ "Primary mortgage only",
      variable == "B25081_005" ~ "Second mortgage and/or home equity loan",
      variable == "B25081_006" ~ "Second mortgage and/or home equity loan",
      variable == "B25081_007" ~ "Second mortgage and/or home equity loan",
      variable == "B25081_008" ~ "Home equity loan only",
      variable == "B25081_009" ~ "No debt",
      .default = NA
    )
  ) |> 
  drop_na(multiple) |> 
  select(-type)

b25081_raw <- b25081_raw |> 
  right_join(b25081_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, mortgage, multiple, estimate, moe)

b25081_data <- b25081_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |>
  select(NAME, GEOID, mortgage, multiple, estimate) |> 
  summarise(estimate = sum(estimate), .by = c(NAME, mortgage, multiple)) |> 
  bind_rows(
    b25081_raw |> 
      summarise(
        estimate = sum(estimate),
        .by = c(mortgage, multiple)
      ) |> 
      mutate(NAME = "Region", .before = 1)
  ) |> 
  mutate(pct1 = estimate/sum(estimate), .by = c(NAME)) |> 
  mutate(pct2 = estimate/sum(estimate), .by = c(NAME, mortgage))

write_rds(b25081_data, "data/b25081.rds")
