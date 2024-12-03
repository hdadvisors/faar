
library(tidyverse)
library(glue)
library(httr)
library(janitor)
library(readxl)
library(hdatools)

faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2021

sumlev <- "050"

url <- "https://www.huduser.gov/PORTAL/datasets/cp/2017thru2021-050-csv.zip"

file <- basename(url)

path <- file.path("data/chas", file)

GET(url, write_disk(path, overwrite = TRUE), progress(type = "down"))

unzip(path, exdir = "data/chas")

raw <- read_csv("data/chas/050/Table7.csv", col_types = cols())

cleaned <- raw |>
  clean_names() |>
  mutate(fips = substr(geoid, 10, 14)) |>
  filter(fips %in% faar) |> 
  mutate(
    name = str_remove_all(name, " County, Virginia| city, Virginia")
  ) |> 
  select(433, 4, 7:422) |> 
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

dict <- read_excel("data/chas/CHAS data dictionary 17-21.xlsx", sheet = "Table 7") |> 
  clean_names() |> 
  rename(code = 1) |> 
  filter(line_type == "Detail") |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  )) |> 
  mutate(household_income = fct_case_when(
    household_income == "household income is less than or equal to 30% of HAMFI" ~ "30% AMI or below",
    household_income == "household income is greater than 30% but less than or equal to 50% of HAMFI" ~ "31–50% AMI",
    household_income == "household income is greater than 50% but less than or equal to 80% of HAMFI" ~ "51–80% AMI",
    household_income == "household income is greater than 80% but less than or equal to 100% of HAMFI" ~ "81–100% AMI",
    household_income == "household income is greater than 100% of HAMFI" ~ "Above 100% AMI"
  )) |> 
  mutate(household_type = case_when(
    household_type == "household type is elderly family (2 persons, with either or both age 62 or over)" ~ "Elderly family",
    household_type == "household type is small family (2 persons, neither person 62 years or over, or 3 or 4 persons)" ~ "Small family",
    household_type == "household type is large family (5 or more persons)" ~ "Large family",
    household_type == "household type is elderly non-family" ~ "Elderly non-family",
    household_type == "other household type (non-elderly non-family)" ~ "Non-elderly non-family"
  ))  |> 
  mutate(cost_burden = fct_case_when(
    cost_burden == "housing cost burden is less than or equal to 30%" ~ "Not cost-burdened",
    cost_burden == "housing cost burden is greater than 30% but less than or equal to 50%" ~ "Cost-burdened",
    cost_burden == "housing cost burden is greater than 50%" ~ "Severely cost-burdened"
  )) |> 
  mutate(cb_group = fct_case_when(
    cost_burden == "Cost-burdened" ~ "Cost-burdened",
    cost_burden == "Severely cost-burdened" ~ "Cost-burdened",
    cost_burden == "Not cost-burdened" ~ "Not cost-burdened"
  )) 

cleaned_with_dict <- cleaned |>
  right_join(dict) |> 
  select(1:2, 7, 8, 11, 10, 4) |> 
  summarise(
    est = sum(est),
    .by = c(fips, name, tenure, household_income, cb_group, cost_burden)
  )

write_rds(cleaned_with_dict, "data/chas_t7.rds")
