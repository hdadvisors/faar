## Calculate FAAR AMI with PUMS Data ----------------------


# 1. Setup ------------------------------------------------

library(tidyverse)
library(srvyr)

# Load PUMS data
pums_raw <- read_rds("data/pums/pums_raw.rds")
pums_wgt <- read_rds("data/pums/pums_wgt.rds")

  
# 2. Prepare PUMS data -----------------------------------

# Filter only household records with valid FINCP value
pums_data <- pums_raw |> 
  select(1:4, FINCP) |> 
  filter(SPORDER == 1, FINCP >= 0) |> 
  left_join(pums_wgt)
  

# 3. Calculate FAAR Median Family Income ------------------

# HUD inflation adjustment for 2022 ACS -> FY 2024
# (See "Methodology for Calculating FY 2024 Medians")
hud_adj <- 1.062

# Calculate Median Family Income (MFI) for FAAR region
faar_mfi <- pums_data |>
  to_survey(type = "housing", design = "rep_weights") |> 
  summarise(
    mfi = survey_median(FINCP, na.rm = TRUE)
  )

# Adjust MFI from PUMS to FY 2024 dollars
faar_mfi_adj <- round(faar_mfi$mfi * hud_adj, -2) # $116,300
  
  
# 4. Calculate FAAR regional income limits ------------------

# Create function to calculate income limits based on MFI
calc_ami <- function(mfi, area_name, levels = c(30, 50, 80, 100, 120)) {
  
  # Calculate 4-person Very Low-Income Limit (VLIL)
  vlil <- mfi * 0.5
  
  # Create Family Size Adjustment factors for 1 to 8-person households
  fsa <- c(0.70, 0.80, 0.90, 1, 1.08, 1.16, 1.24, 1.32)
  
  # Calculate AMI levels
  ami_levels <- sapply(levels, function(level) {
    ami <- round(vlil * (level / 50) / 50) * 50
    round(ami * fsa / 50) * 50
  })
  
  # Create tibble with results
  ami <- tibble(
    area = area_name,
    hh_size = paste0(1:8, "-person")
  )
  
  # Add columns for each AMI level
  for (i in seq_along(levels)) {
    ami[[paste0("ami", levels[i])]] <- ami_levels[, i]
  }
  
  # Pivot longer
  ami <- ami |> 
    pivot_longer(-(1:2), names_to = "level", values_to = "income")
  
  return(ami)
}

faar_ami <- calc_ami(faar_mfi_adj, "Planning District 16") |> 
  mutate(cap = "Uncapped", .after = 1)

# Save or load FAAR AMI data as needed
# write_rds(faar_ami, "data/pums/faar_ami_pums.rds")
faar_ami <- read_rds("data/pums/faar_ami_pums.rds")
