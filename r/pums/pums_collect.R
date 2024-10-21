## Collect PUMS Data for Housing Spectrum -----------------

# 1. Setup ------------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)

# Load PUMS variables to explore
pums_vars_2022 <- pums_variables |>  
  filter(year == 2022, survey == "acs5")

# Define FAAR region with two PUMAs
faar_2010 <- c("51115", "51120") # 2010 PUMA codes
faar_2020 <- c("17700", "17900") # 2020 PUMA codes


# 2. Variables --------------------------------------------

make_pums_vars <- function() {
  
  # HOUSING RECORD - BASIC VARIABLES
  vars_hr_basic <- c(
    "PUMA10",      # PUMA (2010 definition)
    "PUMA20",      # PUMA (2020 definition)
    "NP"           # Number of persons in household
  )
  
  # HOUSING RECORD - HOUSING UNIT VARIABLES
  vars_hr_hu <- c(
    "BDSP",        # Number of bedrooms
    "BLD",         # Units in structure
    "FS",          # SNAP status
    "TEN",         # Tenure
    "YRBLT"        # When structure first built
  )
  
  # HOUSING RECORD - HOUSEHOLD VARIABLES
  vars_hr_hh <- c(
    "FINCP",       # Family income
    "GRNTP",       # Gross rent
    "HHLDRHISP",   # Hispanic origin
    "HHLDRRAC1P",  # Race
    "HHT2",        # Household/family type (includes cohabiting)
    "HINCP",       # Household income
    "MULTG",       # Multigenerational household
    "MV",          # When moved into home
    "NOC",         # Number of own children
    "SMOCP",       # Selected monthly owner costs
    "WIF"          # Workers in family
  )
  
  # PERSON RECORD - PERSON VARIABLES
  vars_pr_p <- c(
    "AGEP",        # Age
    "COW",         # Class of worker
    "HINS3",       # Medicare
    "HINS4",       # Medicaid 
    "INTP",        # Interest, dividends, and net rental income
    "JWMNP",       # Travel time to work
    "JWRIP",       # Carpool status
    "JWTRNS",      # Means of transportation to work
    "OIP",         # All other income
    "PAP",         # Public assistance income
    "RELSHIPP",    # Relationship (to householder)
    "RETP",        # Retirement income
    "SCHL",        # Educational attainment
    "SEMP",        # Self-employment income
    "SEX",         # Sex
    "SSIP",        # Supplemental Security Income amount
    "SSP",         # Social Security amount
    "WAGP",        # Wages or salary income
    "WKL"          # When last worked
  )
  
  # PERSON RECORD - RECODED PERSON VARIABLES
  vars_pr_rcp <- c(
    "DIS",         # Disability
    "ESR",         # Employment status recode
    "NAICSP",      # NAICS recode (based on 2017 NAICS codes)
    "POWPUMA10",   # Place of work PUMA (2010 definition)
    "POWPUMA20",   # Place of work PUMA (2020 definition)
    "POWSP",       # Place of work state
    "SOCP"         # SOC codes (based on 2018 SOC codes)
  )
  
  vars_spectrum <- c(
    vars_hr_basic, vars_hr_hu, vars_hr_hh, vars_pr_p, vars_pr_rcp
  )
  
  vars_spectrum
  
}

vars_spectrum <- make_pums_vars()

# write_rds(vars_spectrum, "data/pums/pums_vars.rds")

# 3. Collect PUMS variables -------------------------------

# Get PUMS variables without weights
pums_raw <- get_pums(
  variables = vars_spectrum,
  year = 2022,
  state = "VA",
  survey = "acs5"
  ) |> 
  filter(PUMA10 %in% faar_2010 | PUMA20 %in% faar_2020)

# Save or load as needed
write_rds(pums_raw, "data/pums/pums_raw.rds")
# pums_raw <- read_rds("data/pums/pums_raw.rds")


# 4. Collect PUMS weights ---------------------------------

# Get PUMS with weights only
pums_wgt <- get_pums(
  variables = c("PUMA10", "PUMA20"),
  year = 2022,
  state = "VA",
  survey = "acs5",
  rep_weights = "both"
  ) |> 
  filter(PUMA10 %in% faar_2010 | PUMA20 %in% faar_2020)

# Save or load as needed
# write_rds(pums_wgt, "data/pums/pums_wgt.rds")
# pums_wgt <- read_rds("data/pums/pums_wgt.rds")

