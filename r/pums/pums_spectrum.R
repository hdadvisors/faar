## Use PUMS Data for Housing Spectrum Analysis

## 1. Setup -----------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(survey)

# Load clean, labels PUMS data with variables and weights
pums_faar <- read_rds("data/pums/pums_faar.rds")


## TEST fct_case_when() -----------------------------------

# https://stackoverflow.com/questions/49572416/r-convert-to-factor-with-order-of-levels-same-with-case-when

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

pums_faar_fct <- pums_faar |> 
  mutate(
    cost_hsg = case_when(
      cost_own > cost_rent ~ cost_own,
      cost_rent > cost_own ~ cost_rent
    ), .after = cost_rent
  ) |> 
  mutate(
    cost_hsg_pct = cost_hsg/(hh_income/12),
    .after = cost_hsg
  ) |> 
  mutate(
    cb = fct_case_when(
      cost_hsg_pct < 0.30 ~ "Not cost-burdened",
      cost_hsg_pct < 0.50 ~ "Cost-burdened",
      cost_hsg_pct >= 0.50 ~ "Severely cost-burdened"
    ), .after = cost_hsg_pct
  )

## 2. Function to label reliability of estimates ----------

add_reliability <- function(data) {
  
  # Find the column name ending with "_cv"
  cv_col <- names(data)[grep("_cv$", names(data))]
  
  # Check if a CV column was found
  if (length(cv_col) == 0) {
    stop("No column ending with '_cv' found in the data.")
  } else if (length(cv_col) > 1) {
    warning("Multiple columns ending with '_cv' found. Using the first one.")
    cv_col <- cv_col[1]
  }
  
  # Add the reliability column based on the CV values
  data %>%
    mutate(reliability = case_when(
      .data[[cv_col]] < 0.15 ~ "High",
      .data[[cv_col]] >= 0.15 & .data[[cv_col]] < 0.30 ~ "Medium",
      .data[[cv_col]] >= 0.30 ~ "Low",
      TRUE ~ NA_character_  # For any other case (e.g., NA values)
    ))
  
}

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
