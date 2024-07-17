## Use PUMS Data for Housing Spectrum Analysis

## Setup -----------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(hdatools)

# Load clean, labels PUMS data with variables and weights
pums_faar <- read_rds("data/pums/pums_faar.rds")


## Household typologies

faar_hh_typologies <- pums_faar |> 
  group_by(SERIALNO) |> 
  mutate(
    hh_age = case_when(
      # Calculate mean adult age and immediately use it for categorization
      mean(age[age >= 18], na.rm = TRUE) < 35 ~ "Young",
      mean(age[age >= 18], na.rm = TRUE) < 55 ~ "Middle-age",
      mean(age[age >= 18], na.rm = TRUE) < 75 ~ "Senior",
      TRUE ~ "Elderly"
    ), .after = age
  ) |> 
  mutate(
    hh_disability = if_else(
      SPORDER == 1,
      any(disability == "With a disability", na.rm = TRUE),
      NA
    ), .after = disability
  ) |> 
  ungroup() |> 
  filter(SPORDER == 1) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(tenure, cb_bin) |>

## Regional housing spectrum ------------------------------




## 3. Stats for all households ----------------------------

# AMI by tenure
pums_faar_fct |> 
  filter(SPORDER == 1, cost_hsg_pct > 0 & cost_hsg_pct < 1) |> 
  mutate(cb_bin = cut(cost_hsg_pct, breaks = 50)) |> 
  #select(cost_hsg_pct, cb_bin)
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(tenure, cb_bin) |> 
  summarise(
    n = survey_prop(vartype = "cv")
  ) |> 
  mutate(bin_midpoint = as.numeric(str_extract(cb_bin, "(?<=,).*(?=\\])"))) |> 
  #add_reliability()
  ggplot(aes(x = bin_midpoint, y = n, fill = tenure)) +
    geom_col(position = "dodge")



## 4. Stats for households with at least 

