
library(tidyverse)
library(glue)
library(httr)
library(janitor)
library(readxl)

faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2021

sumlev <- "050"

dir.create(glue("data/{sumlev}"))

walk(years, ~{
  url <- glue("https://www.huduser.gov/PORTAL/datasets/cp/{.x - 4}thru{.x}-{sumlev}-csv.zip")
  file <- basename(url)
  path <- file.path("data", sumlev, file)
  if (!file.exists(path)) {
    GET(url, write_disk(path, overwrite = TRUE), progress(type = "down"))
  }
  print(glue("Unzipping {.x}..."))
  unzip(path, exdir = file.path("data", sumlev, .x))
})

# Tables to get
tables <- "18C"
table <- "18C"

raw <- read_csv("data/050/2021/050/Table18C.csv", col_types = cols())

table <- "18C"

cleaned <- raw |>
  clean_names() |>
  mutate(fips = substr(geoid, 10, 14)) |>
  separate(name, into = c("county", "state"), sep = ",") |>
  filter(st == "51") |>
  pivot_longer(starts_with("T"),
               names_to = "code",
               values_to = "value") |>
  mutate(id = str_extract(code, "\\d+$"),
         type = str_extract(code, "est|moe")) |>
  select(-code) |>
  pivot_wider(names_from = type, values_from = value) |>
  rename(Estimate = est, MOE = moe) |>
  mutate(Code := glue("T{table}_est{id}"),
         Year = 2021) |>
  select(Code, Year, Estimate, MOE, everything(), -id) |>
  mutate(fips = case_when(
    fips == "51515" ~ "51019",
    TRUE ~ fips
  )) |>
  mutate(county = case_when(
    county == "Bedford city" ~ "Bedford County",
    TRUE ~ county
  )) |>
  subset(fips %in% faar)

dict <- read_excel("data/050/2021/CHAS data dictionary 17-21.xlsx",
                   sheet = "Table 18C")

cleaned_with_dict <- cleaned |>
  left_join(dict, by = c("Code" = "Column Name"))

write_csv(cleaned_with_dict, "data/CHAS/Table18C_2021.csv")


t18c_2021 <- cleaned_with_dict |> 
  clean_names() |> 
  select(fips, county, line_type, units_in_structure, rent, household_income, code, estimate, moe) |> 
  filter(line_type == "Detail") |> 
  summarise(
    estimate = sum(estimate),
    .by = c(fips, county, rent, household_income)
  ) |> 
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

write_rds(t18c_2021, "data/tb_18_match.rds")















source("package_setup.R")

faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


## Rental housing gap


'{r}

#| label: gap-data
#| eval: false

tb_18c <- Table18C |>
  select(st = 51)

colnames(tb_18c)[16] <- "Cost"

renter <- tb_18c |>
  clean_names() |>
  filter(line_type == "Detail") |>
  select(county, fips, year, estimate, tenure, cost, household_income) |>
  group_by(county, fips, year, tenure, cost, household_income) |>
  summarise(estimate = sum(estimate)) |>
  filter(fips %in% faar)

tb_18_match <- renter |>
  filter(tenure == "Renter occupied") |>
  mutate(cost = case_when(
    cost == "greater than RHUD30 and less than or equal to RHUD50" ~ "31 to 50% AMI",
    cost == "greater than RHUD50 and less than or equal to RHUD80" ~ "51 to 80% AMI",
    cost == "greater than RHUD80" ~ "80% AMI or greater",
    cost == "less than or equal to RHUD30" ~ "30% AMI or below"
  )) |>
  mutate(household_income = case_when(
    household_income == "greater than 100% of HAMFI" ~ "80% AMI or greater",
    household_income == "greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80% AMI or greater",
    household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51 to 80% AMI",
    household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31 to 50% AMI",
    household_income == "less than or equal to 30% of HAMFI" ~ "30% AMI or below"
  )) |>
  group_by(county, fips, year, cost, household_income) |>
  summarise(estimate = sum(estimate))

tb_18_match <- tb_18_match |>
  mutate(match = case_when(
    cost == household_income ~ "Affordable",
    cost == "30% AMI or below" & household_income == "31 to 50% AMI" ~ "Very affordable",
    cost == "30% AMI or below" & household_income == "51 to 80% AMI" ~ "Very affordable",
    cost == "30% AMI or below" & household_income == "80% AMI or greater" ~ "Very affordable",
    cost == "31 to 50% AMI" & household_income == "31 to 50% AMI" ~ "Affordable",
    cost == "31 to 50% AMI" & household_income == "51 to 80% AMI" ~ "Very affordable",
    cost == "31 to 50% AMI" & household_income == "80% AMI or greater" ~ "Very affordable",
    cost == "31 to 50% AMI" & household_income == "30% AMI or below" ~ "Unaffordable",
    cost == "51 to 80% AMI" & household_income == "30% AMI or below" ~ "Unaffordable",
    cost == "51 to 80% AMI" & household_income == "31 to 50% AMI" ~ "Unaffordable",
    cost == "51 to 80% AMI" & household_income == "80% AMI or greater" ~ "Very affordable",
    cost == "80% AMI or greater" & household_income == "30% AMI or below" ~ "Unaffordable",
    cost == "80% AMI or greater" & household_income == "31 to 50% AMI" ~ "Unaffordable",
    cost == "80% AMI or greater" & household_income == "51 to 80% AMI" ~ "Unaffordable"
  )) |>
  mutate(gapcode = case_when(
    match == "Unaffordable" ~ "Gap",
    TRUE ~ "Matches or less than income"
  ))

write_rds(tb_18_match, "data/tb_18_match.rds")

tb_18_match <- read_rds("data/tb_18_match.rds")

gap <- tb_18_match |>
  filter(fips %in% as.numeric(pha)) |> 
  group_by(year, household_income, gapcode) |>
  summarise(estimate = sum(estimate)) |>
  mutate(estimate = case_when(
    gapcode == "Gap" ~ estimate * -1,
    gapcode == "Matches or less than income" ~ estimate
  )) |>
  filter(household_income != "80% AMI or greater")

write_rds(gap, "data/gap.rds")

'
