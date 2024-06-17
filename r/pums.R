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

faar_ami <- pums_data |> 
  # filter(SPORDER == 1,
  #        TEN_label != "N/A (GQ/vacant)")|> 
  summarise(ami = weighted.median(HINCP, WGTP, na.rm = TRUE))


pums_fxburg <- pums_data |> 
  filter(TEN_label != "N/A (GQ/vacant)") |> 
  filter(SPORDER == 1) |> 
  mutate(pct_ami = HINCP/98800) |> # Median Household Income for FAAR region
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.20 ~ "101-120% AMI",
    pct_ami > 1.20 ~ "120% AMI and greater")) |> 
  mutate(WIF = as.numeric(WIF)) |> 
  mutate(NOC = as.numeric(NOC)) |>
  group_by(ami) |> 
  summarise(
    mean_hhage = weighted.mean(HHLDRAGEP, WGTP),
    med_inc = weighted.mean(HINCP, WGTP),
    mean_wif = weighted.mean(WIF, WGTP, na.rm = TRUE))



pums_fxburg <- pums_data |> 
  filter(TEN_label != "N/A (GQ/vacant)") |> 
  filter(SPORDER == 1) |> 
  mutate(pct_ami = HINCP/98800) |> # Median Household Income for FAAR region
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.20 ~ "101-120% AMI",
    pct_ami > 1.20 ~ "120% AMI and greater")) |> 
  mutate(race = case_when(
    HHLDRHISP_label != "Not Spanish/Hispanic/Latino" ~ "Hispanic, or Latino",
    HHLDRHISP_label == "Not Spanish/Hispanic/Latino" ~ HHLDRRAC1P_label
  ))

# Top tenure by AMI

pums_tenure <- pums_data |> 
  mutate(pct_ami = HINCP/129000) |> # Median Family Income in 2021 for DC Metro
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.15 ~ "101-115% AMI",
    pct_ami > 1.15 & pct_ami <= 1.50 ~ "115-150% AMI",
    pct_ami > 1.50 ~ "151% AMI and greater")) |> 
  group_by(ami, TEN_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami) |> 
  arrange(ami, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Top NAICS Occupation by AMI

pums_occ <- pums_data |> 
  mutate(pct_ami = HINCP/129000) |> # Median Family Income in 2021 for DC Metro
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.15 ~ "101-115% AMI",
    pct_ami > 1.15 & pct_ami <= 1.50 ~ "115-150% AMI",
    pct_ami > 1.50 ~ "151% AMI and greater")) |> 
  filter(NAICSP != "N") |> 
  group_by(ami, NAICSP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami) |> 
  arrange(ami, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()



