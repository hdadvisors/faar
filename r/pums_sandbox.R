library(tidycensus)
library(tidyverse)
library(spatstat)

pums_vars_2022 <- pums_variables %>% 
  filter(year == 2022, survey == "acs5")


fxburg <- c("51115", "51120")
faar <- c("17700", "17900")

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
  variables = c("PUMA20", "HHT2", "HHLDRAGEP", "NP", "HINCP", "ADJINC", 
                "WIF", "NAICSP", "NOC", "TEN", "HHLDRHISP", "HHLDRRAC1P"),
  year = 2022,
  state = "VA",
  survey = "acs5",
  recode = TRUE
) |> 
  filter(PUMA20 %in% faar)


hud_ami <- read_csv("data/raw/hud_ami.csv") |> 
  pivot_wider(names_from = "ami") |> 
  janitor::clean_names() |> 
  select(NP = size, 2:6)

# HUD 2022 Income Limits

pums_join <- pums_data |> 
  left_join(hud_ami, by = "NP") |> 
  mutate(ami = case_when(
    HINCP <= x30 ~ "30% AMI or less",
    HINCP > x30 & HINCP <= x50 ~ "31 to 50% AMI",
    HINCP > x50 & HINCP <= x80 ~ "51 to 80% AMI",
    HINCP > x80 & HINCP <= x100 ~ "81 to 100% AMI",
    HINCP > x100 & HINCP <= x120 ~ "101 to 120% AMI",
    TRUE ~ "121% AMI or more"
  ))

