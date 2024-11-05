
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
  filter(fips %in% rr)

tb_18_match <- renter |>
  filter(tenure == "Renter occupied") |>
  mutate(cost = case_when(
    cost == "greater than RHUD30 and less than or equal to RHUD50" ~ "31 to 50 percent AMI",
    cost == "greater than RHUD50 and less than or equal to RHUD80" ~ "51 to 80 percent AMI",
    cost == "greater than RHUD80" ~ "80 percent AMI or greater",
    cost == "less than or equal to RHUD30" ~ "30 percent AMI or below"
  )) |>
  mutate(household_income = case_when(
    household_income == "greater than 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80 percent AMI or greater",
    household_income == "greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "51 to 80 percent AMI",
    household_income == "greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "31 to 50 percent AMI",
    household_income == "less than or equal to 30% of HAMFI" ~ "30 percent AMI or below"
  )) |>
  group_by(county, fips, year, cost, household_income) |>
  summarise(estimate = sum(estimate))

tb_18_match <- tb_18_match |>
  mutate(match = case_when(
    cost == household_income ~ "Affordable",
    cost == "30 percent AMI or below" & household_income == "31 to 50 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "30 percent AMI or below" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Affordable",
    cost == "31 to 50 percent AMI" & household_income == "51 to 80 percent AMI" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "31 to 50 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "51 to 80 percent AMI" & household_income == "80 percent AMI or greater" ~ "Very affordable",
    cost == "80 percent AMI or greater" & household_income == "30 percent AMI or below" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "31 to 50 percent AMI" ~ "Unaffordable",
    cost == "80 percent AMI or greater" & household_income == "51 to 80 percent AMI" ~ "Unaffordable"
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
  filter(household_income != "80 percent AMI or greater")

write_rds(gap, "data/gap.rds")

'
