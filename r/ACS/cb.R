library(tidyverse)
library(tidycensus)

faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


years <- 2010:2022

b25106_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25106")

b25106_raw <- map_dfr(years, function(yr) {
  b25042_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25106",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

# b25106_raw <- b25106_raw |> 
#   subset(GEOID %in% faar)

b25106_vars_cleaned <- b25106_vars |> 
  separate(label, into = c("est", "total", "tenure", "income", "cb"), sep = "!!") |> 
  select(variable = name, tenure, income, cb) |> 
  drop_na() |> 
  mutate(across(2:3, .fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner-occupied housing units" ~ "Homeowner",
    tenure == "Renter-occupied housing units" ~ "Renter"),
    cb = case_when(
      cb == "30 percent or more" ~ "Cost-burdened",
      TRUE ~ "Not cost-burdened"))

b25106_raw <- b25106_raw |> 
  right_join(b25106_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, income, cb, estimate)

b25106_data <- b25106_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         NAME = case_when(
           NAME == "Bedford city" ~ "Bedford County", # Aggregate Bedford City and Bedford County
           TRUE ~ NAME)) |>
  select(NAME, GEOID, year, tenure, income, cb, estimate) |> 
  group_by(NAME, GEOID, year, tenure, income, cb) |> 
  summarise(estimate = sum(estimate))


va_cb <- b25106_data |> 
  group_by(year, tenure, cb) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(year, tenure) |> 
  mutate(pct = round(estimate/sum(estimate), 3)) |> 
  mutate(year = as.character(year)) |> 
  filter(tenure == "Renter")

write_csv(va_cb, "data/va_cb.csv")

ggplot(va_cb,
       aes(x = year,
           y = pct,
           fill = cb)) +
  geom_col(position = "stack") +
  facet_wrap(~tenure) +
  scale_y_continuous(labels = scales::percent_format())


write_rds(b25106_data, "data/b25106.rds")

```