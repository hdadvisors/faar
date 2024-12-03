
library(tidyverse)
library(glue)
library(httr)
library(janitor)
library(readxl)

raw <- read_csv("data/chas/050/Table18C.csv", col_types = cols())

cleaned <- raw |>
  clean_names() |>
  mutate(fips = substr(geoid, 10, 14)) |>
  filter(fips %in% faar) |> 
  mutate(
    name = str_remove_all(name, " County, Virginia| city, Virginia")
  ) |> 
  select(209, 4, 7:209) |> 
  pivot_longer(
    starts_with("T"),
    names_to = "code",
    values_to = "value"
  ) |> 
  mutate(type = str_extract(code, "est|moe"), .after = 4) |> 
  mutate(
    code = str_replace_all(code, "moe", "est"),
    code = str_to_title(code)
  ) |> 
  pivot_wider(names_from = type, values_from = value)


dict <- read_excel("data/chas/CHAS data dictionary 17-21.xlsx", sheet = "Table 18C") |> 
  clean_names() |> 
  filter(line_type == "Detail") |> 
  select(code = 1, 6, 5) |> 
  mutate(code = str_to_title(code)) |> 
  mutate(
    rent = fct_case_when(
      rent == "less than or equal to RHUD30" ~ "30% AMI or below",
      rent == "greater than RHUD30 and less than or equal to RHUD50" ~ "31–50% AMI",
      rent == "greater than RHUD50 and less than or equal to RHUD80" ~ "51–80% AMI",
      rent == "greater than RHUD80" ~ "80% AMI or greater"
    )
  ) |> 
  mutate(
    household_income = fct_case_when(
      household_income == "less than or equal to 30% of HAMFI" ~ "30% AMI or below",
      household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31–50% AMI",
      household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51–80% AMI",
      str_detect(household_income, "100") ~ "80% AMI or greater"
    )
  ) |> 
  mutate(
    match = case_when(
      rent == household_income ~ "Affordable",
      rent == "30% AMI or below" & household_income == "31–50% AMI" ~ "Very affordable",
      rent == "30% AMI or below" & household_income == "51–80% AMI" ~ "Very affordable",
      rent == "30% AMI or below" & household_income == "80% AMI or greater" ~ "Very affordable",
      rent == "31–50% AMI" & household_income == "31–50% AMI" ~ "Affordable",
      rent == "31–50% AMI" & household_income == "51–80% AMI" ~ "Very affordable",
      rent == "31–50% AMI" & household_income == "80% AMI or greater" ~ "Very affordable",
      rent == "31–50% AMI" & household_income == "30% AMI or below" ~ "Unaffordable",
      rent == "51–80% AMI" & household_income == "30% AMI or below" ~ "Unaffordable",
      rent == "51–80% AMI" & household_income == "31–50% AMI" ~ "Unaffordable",
      rent == "51–80% AMI" & household_income == "80% AMI or greater" ~ "Very affordable",
      rent == "80% AMI or greater" & household_income == "30% AMI or below" ~ "Unaffordable",
      rent == "80% AMI or greater" & household_income == "31–50% AMI" ~ "Unaffordable",
      rent == "80% AMI or greater" & household_income == "51–80% AMI" ~ "Unaffordable"
    )
  ) |> 
  mutate(
    gapcode = case_when(
      match == "Unaffordable" ~ "Gap",
      TRUE ~ "Matches or less than income"
    )
  )

cleaned_with_dict <- cleaned |>
  right_join(dict) |> 
  select(1:2, 6:9, 4) |> 
  summarise(
    est = sum(est),
    .by = c(fips, name, household_income, rent, match, gapcode)
  )

write_rds(cleaned_with_dict, "data/chas_t18c.rds")
