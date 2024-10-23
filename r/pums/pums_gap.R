## Setup --------------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(scales)
library(hdatools)
library(ggridges)

# Load clean, labels PUMS data with variables and weights
pums_faar <- read_rds("data/pums/pums_faar.rds")

# Household records only
pums_faar_hh <- pums_faar |> filter(SPORDER == 1)



faar_ami_pums <- read_rds("data/pums/faar_ami_pums.rds") |> 
  mutate(
    ami_faar =
      case_match(
        level,
        "ami30" ~ "Below 30% AMI",
        "ami50" ~ "30-50% AMI",
        "ami80" ~ "50-80% AMI",
        "ami100" ~ "80-100% AMI",
        "ami120" ~ "100-120% AMI"
      )
  )

  
  
  ami_levels <- c(
    "Zero or negative income", "Below 30% AMI", "30-50% AMI",
    "50-80% AMI", "80-100% AMI", "100-120% AMI", "Above 120% AMI"
  )

  pums_faar_gap <- pums_faar_hh |> 