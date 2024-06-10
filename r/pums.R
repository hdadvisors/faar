library(tidycensus)
library(tidyverse)
library(spatstat)

pums_vars_2021 <- pums_variables %>% 
  filter(year == 2021, survey == "acs5")


fxburg <- c("51115", "51120")

# Variables
# Household Type = "HHT2"
# Age of Householder =  "HHLDRAGEP"
# Number of Persons Associated w/ Housing Record = "NP"
# Workers in Family = "WIF"
# Household Income = "HINCP"
# Adjustment Factor for Household Income = "ADJINC"
# NAICS Code = "NAICSP"
# Number of Own Children in Household = "NOC"

pums_data <- get_pums(
  variables = c("PUMA", "HHT2", "HHLDRAGEP", "NP", "HINCP", "ADJINC", 
                "WIF", "NAICSP", "NOC"),
  year = 2021,
  state = "VA",
  puma = fxburg,
  survey = "acs5",
  recode = TRUE
)



pums_fxburg <- pums_data |> 
  mutate(pct_ami = HINCP/129000) |> # Median Family Income in 2021 for DC Metro
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.15 ~ "81-115% AMI",
    pct_ami > 1.15 & pct_ami <= 1.50 ~ "115-150% AMI",
    pct_ami > 1.50 ~ "151% AMI and greater")) |> 
  mutate(WIF = as.numeric(WIF)) |> 
  mutate(NOC = as.numeric(NOC)) |>
  distinct(SERIALNO, .keep_all = TRUE) |> 
  group_by(ami, HHT2_label, WIF_label, NAICSP_label) |> 
  summarise(
    mean_hhage = weighted.mean(HHLDRAGEP, WGTP),
    med_inc = weighted.mean(HINCP, WGTP),
    mode = sum(WGTP)
  )
