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
  mutate(
    children = case_when(
      children == 0 ~ "No",
      children > 0 ~ "Yes"
    )
  ) |> 
  mutate(
    hh_earners = case_when(
      hh_earners == 0 ~ "No",
      hh_earners > 0 ~ "Yes"
    )
  ) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(hh_type, hh_age, children, hh_earners) |>
  summarise(
    n = survey_total(vartype = "cv")
  )

# 1. Working family (middle-age): Couple > Middle-age > Children > Working
# 2. SINKs/DINKs: Couple > Middle-age > No children > Working
# 3. Working family (young): Couple > Young > Children > Working
# 4. Working single-parent: Single parent > Middle-age > Working
# 5. 

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

