## Prepare PUMS Data for Housing Spectrum -----------------


## 1. Setup -----------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(hdatools)

pums_raw <- read_rds("data/pums/pums_raw.rds")
pums_wgt <- read_rds("data/pums/pums_wgt.rds")
pums_labels <- read_rds("data/pums/pums_labels.rds")
faar_ami <- read_rds("data/pums/faar_ami_pums.rds")


## 2. Join AMI limits to PUMS data ------------------------

# Prepare AMI limits to join
faar_ami_join <- faar_ami |> 
  mutate(NP = as.numeric(str_sub(hh_size, end = 1))) |> 
  select(6, 4, 5) |> 
  pivot_wider(names_from = level, values_from = income)

# Create list of AMI levels
ami_levels <- c(
  "Zero or negative income", "Below 30% AMI", "30-50% AMI",
  "50-80% AMI", "80-100% AMI", "100-120% AMI", "Above 120% AMI"
  )

# Join AMI limits and categorize
faar_pums_ami <- pums_raw |>
  filter(!TEN == "b") |> 
  mutate(NP = pmin(NP, 8)) |> 
  left_join(faar_ami_join, by = "NP") |>
  mutate(
    ami_faar = factor(
      case_when(
        HINCP < 1 ~ ami_levels[1],
        HINCP <= ami30 ~ ami_levels[2],
        HINCP <= ami50 ~ ami_levels[3],
        HINCP <= ami80 ~ ami_levels[4],
        HINCP <= ami100 ~ ami_levels[5],
        HINCP <= ami120 ~ ami_levels[6],
        TRUE ~ ami_levels[7]
      ),
      levels = ami_levels,
      ordered = TRUE
    )
  ) |> 
  select(-ami30, -ami50, -ami80, -ami100, -ami120)


## 3. Calculate number of earners in household ------------

# Identify persons with wages or salary at least $5000 in last 12 months
faar_pums_earners <- faar_pums_ami |> 
  group_by(SERIALNO) |> 
  mutate(
    hh_earners = sum((WAGP + SEMP) >= 5000) # Include self-employment income
  ) |> 
  ungroup()


## 4. Add place of work labels ----------------------------

powpuma_lookup <- read_rds("data/pums/powpuma_lookup.rds") |> 
  select(5:6)

faar_pums_pow <- faar_pums_earners |> 
  # Clean state POW column
  mutate(
    POWSP = case_when(
      POWSP == "00N" ~ NA,
      str_starts(POWSP, "0") ~ str_sub(POWSP, start = 2, end = 3),
      .default = POWSP
    )
  ) |> 
  # Create POW GEOID column
  mutate(
    pow_geoid = case_when(
      is.na(POWSP) ~ NA,
      POWPUMA10 != "-0009" ~ paste0(POWSP, POWPUMA10),
      POWPUMA20 != "-0009" ~ paste0(POWSP, POWPUMA20)
      
    )
  ) |>
  # Join POW lookup data
  left_join(powpuma_lookup, relationship = "many-to-many") |> 
  # Retain Virginia region labels and add DC/MD labels
  mutate(
    pow_label = case_when(
      POWSP == "51" ~ pow_label,
      POWSP == "11" ~ "Washington, DC",
      POWSP == "24" ~ "Maryland",
      .default = NA
    )
  ) |> 
  # Add 'core_workforce' field to flag HHs with at least 1 worker with POW in FAAR region
  group_by(SERIALNO) |> 
  mutate(
    core_workforce = if_else(
      SPORDER == 1,
      any(pow_label == "Fredericksburg area", na.rm = TRUE),
      NA
    )
  ) |> 
  ungroup()
  

## 5. Add cost burden data and labels ---------------------

faar_pums_cb <- faar_pums_pow |> 
  mutate(
    cb = case_when(
      HINCP == 0 ~ NA,
      SMOCP > GRNTP ~ SMOCP/(HINCP/12),
      GRNTP > SMOCP ~ GRNTP/(HINCP/12)
    ),
    cb = case_when(
      cb > 1.0 ~ 1.0,
      .default = cb
    )
  ) |> 
  mutate(
    cb_label = fct_case_when(
      cb < 0.30 ~ "Not cost-burdened",
      cb < 0.50 ~ "Cost-burdened",
      cb >= 0.50 ~ "Severely cost-burdened"
    )
  )


## 6. Add household-level age and disability labels -------

faar_pums_agedis <- faar_pums_cb |> 
  group_by(SERIALNO) |> 
  mutate(
    hh_age = fct_case_when(
      mean(AGEP[RELSHIPP %in% c("20", "21", "22", "23", "24", "34")], na.rm = TRUE) < 35 ~ "Young",
      mean(AGEP[RELSHIPP %in% c("20", "21", "22", "23", "24", "34")], na.rm = TRUE) < 65 ~ "Middle-age",
      mean(AGEP[RELSHIPP %in% c("20", "21", "22", "23", "24", "34")], na.rm = TRUE) < 80 ~ "Senior",
      TRUE ~ "Elderly"
    ),
  ) |>  
  mutate(
    hh_disability = if_else(
      SPORDER == 1,
      any(DIS == 2, na.rm = TRUE),
      NA
    )
  ) |> 
  ungroup()

## 7. Add number of minors --------------------------------

# Children under 18 in household

faar_pums_minors <- faar_pums_agedis |> 
  group_by(SERIALNO) |>
  mutate(
    minors = sum(str_detect(RELSHIPP, "25|26|27|30|35") & AGEP < 18)
  ) |> 
  ungroup()


## 8. Recode PUMS data as needed --------------------------

# Use custom function from pums_labels.R
pums_recode <- function(data, vars) {
  
  recoded_data <- data
  
  for (var_name in vars) {
    recoded_data <- recoded_data %>%
      left_join(
        pums_labels %>% 
          filter(var_code == var_name) %>% 
          select(val, val_label),
        by = setNames("val", var_name)
      ) %>%
      mutate(!!var_name := coalesce(val_label, as.character(.data[[var_name]]))) %>%
      select(-val_label)
  }
  
  return(recoded_data)
  
}

# Create list of variables to recode
vars_recode <- c(
  "BLD", "FS", "TEN", "YRBLT", "HHLDRHISP", "HHLDRRAC1P", "HHT2", 
  "MV", "WIF", "COW", "HINS3", "HINS4", "RELSHIPP", "SCHL", "SEX",
  "DIS", "ESR", "NAICSP", "SOCP"
)

faar_pums_recode <- pums_recode(faar_pums_minors, vars_recode)


## 8. Rename PUMS variables as needed ---------------------

pums_rename <- function(data) {
  
  data |> 
    rename(
      hh_size = NP,
      bedrooms = BDSP,
      fam_income = FINCP,
      cost_rent = GRNTP,
      hh_income = HINCP,
      children = NOC,
      cost_own = SMOCP,
      age = AGEP,
      inc_int = INTP,
      inc_other = OIP,
      inc_public = PAP,
      inc_ret = RETP,
      inc_selfemp = SEMP,
      inc_ssi = SSIP,
      inc_ss = SSP,
      inc_wages = WAGP,
      str_type = BLD,
      snap = FS,
      tenure = TEN,
      str_yrblt = YRBLT,
      ethnicity = HHLDRHISP,
      race = HHLDRRAC1P,
      hh_type = HHT2,
      multigen = MULTG,
      moved = MV,
      workers = WIF,
      wkr_class = COW,
      medicare = HINS3,
      medicaid = HINS4,
      relationship = RELSHIPP,
      edu = SCHL,
      sex = SEX,
      disability = DIS,
      emp_status = ESR,
      naics = NAICSP,
      soc = SOCP
    ) 
  
}

faar_pums_rename <- pums_rename(faar_pums_recode)


## 8. Group NAICS and SOC codes into categories -----------

# Load lookup tables created in naics_soc_lookup.rds

naics_recode <- read_rds("data/pums/naics_recode.rds") |> 
  select(naics = val_label, naics_group)

soc_recode <- read_rds("data/pums/soc_recode.rds")

faar_pums_occ <- faar_pums_rename |> 
  left_join(naics_recode) |> 
  left_join(soc_recode)

## 9. Clean up and reorder columns ------------------------

faar_pums_clean <- faar_pums_occ |> 
  mutate(
    puma = case_when(
      PUMA10 == "-0009" ~ PUMA20,
      .default = PUMA10
    )
  ) |> 
  select(
    1:4,
    # Household info
    "puma",
    "hh_size",
    "hh_type",
    "hh_age",
    "multigen",
    "children",
    "minors",
    "race",
    "ethnicity",
    # Person info
    "relationship",
    "age",
    "sex",
    "edu",
    # Income and wages
    "ami_faar",
    "hh_income",
    "fam_income",
    "inc_wages",
    "inc_selfemp",
    "inc_ret",
    "inc_int",
    "inc_ss",
    "inc_ssi",
    "inc_public",
    "inc_other",
    # Housing
    "tenure",
    "str_type",
    "bedrooms",
    "str_yrblt",
    "moved",
    "cost_own",
    "cost_rent",
    "cb",
    "cb_label",
    # Disability and assistance
    "hh_disability",
    "disability",
    "snap",
    "medicare",
    "medicaid",
    # Economic
    "workers",
    "hh_earners",
    "emp_status",
    "wkr_class",
    "pow_label",
    "core_workforce",
    "naics",
    "soc",
    "naics_group",
    "soc_group"
  )


## 10. Simplify variable labels ---------------------------

faar_pums_simple <- faar_pums_clean |> 
  mutate(
    hh_type = fct_case_when(
      str_detect(hh_type, "Married|Cohabiting") ~ "Couple",
      str_detect(hh_type, "no spouse/partner present, with children") ~ "Single parent",
      str_detect(hh_type, "living alone") ~ "Individual",
      str_detect(hh_type, "with relatives") ~ "Relatives",
      str_detect(hh_type, "nonrelatives") ~ "Roommates"
    )
  ) |> 
  mutate(
    race = case_when(
      ethnicity != "Not Spanish/Hispanic/Latino" ~ "Hispanic or Latino",
      ethnicity == "Not Spanish/Hispanic/Latino" ~ str_remove_all(race, " alone")
    ) ,
    race = case_when(
      race == "Black or African American" ~ "Black",
      race == "Two or More Races" ~ "Multiracial",
      race == "Some Other Race" ~ "Another race",
      str_detect(race, "American Indian") ~ "American Indian",
      .default = race
    )
  ) |> 
  mutate(
    relationship = case_when(
      str_detect(relationship, "spouse") ~ "Spouse",
      str_detect(relationship, "partner") ~ "Partner",
      str_detect(relationship, "Roommate") ~ "Roommate",
      str_detect(relationship, "Biological|Adopted|Foster|Stepson|Stepdaughter") ~ "Child",
      str_detect(relationship, "Father or mother") ~ "Parent",
      str_detect(relationship, "Brother or sister") ~ "Sibling",
      str_detect(relationship, "in-law") ~ "In-law",
      .default = relationship
    )
  ) |> 
  mutate(
    relationship = case_when(
      relationship == "Child" & age > 18 ~ "Adult child",
      relationship == "Grandchild" & age > 18 ~ "Adult grandchild",
      .default = relationship
    )
  ) |> 
  mutate(
    edu = case_when(
      #is.na(edu) ~ "Not reported",
      str_detect(edu, "less than 3|No schooling") ~ "None",
      str_detect(edu, "preschool") ~ "Pre-K",
      str_detect(edu, paste("Grade", 1:5, collapse = "|")) ~ "Elementary school",
      str_detect(edu, paste("Grade", 6:8, collapse = "|")) ~ "Middle school",
      str_detect(edu, paste("Grade", 9:12, collapse = "|")) ~ "High school",
      str_detect(edu, "12th grade") ~ "High school",
      str_detect(edu, "high|GED") ~ "High school diploma or GED",
      str_detect(edu, "Some college|no degree") ~ "Some college",
      str_detect(edu, "Professional|Doctorate") ~ "Professional or doctorate degree",
      .default = edu
    )
  ) |> 
  mutate(
    tenure_detail = fct_case_when(
      tenure == "Owned with mortgage or loan (include home equity loans)" ~ "Homeowner",
      tenure == "Owned free and clear" ~ "Homeowner (no mortgage)",
      tenure == "Rented" ~ "Renter",
      tenure == "Occupied without payment of rent" ~ "Renter (no rent)"
    ), .after = tenure
  ) |> 
  mutate(
    tenure = fct_case_when(
      str_detect(tenure, "Owned") ~ "Homeowner",
      str_detect(tenure, "(?i)rent") ~ "Renter"
    )
  ) |> 
  mutate(
    str_type = fct_case_when(
      str_type == "One-family house detached" ~ "Single-family detached",
      str_type == "One-family house attached" ~ "Townhome",
      str_detect(str_type, "2 Apartments|3-4 Apartments|5-9 Apartments") ~ "Small multifamily (2-9 units)",
      str_type == "10-19 Apartments" ~ "Medium multifamily (10-19 units)",
      str_detect(str_type, "20-49 Apartments|50 or more apartments") ~ "Large multifamily (20+ units)",
      TRUE ~ "Mobile home"
    )
  ) |> 
  mutate(
    str_yrblt = case_when(
      str_detect(str_yrblt, "202") ~ "2020 or after",
      str_detect(str_yrblt, "195|196") ~ "1950-1969",
      str_detect(str_yrblt, "193|194") ~ "1949 or before",
      .default = str_yrblt
    ),
    str_yrblt = fct_relevel(str_yrblt, sort)
  ) |> 
  mutate(
    moved = fct_case_when(
      moved == "12 months or less" ~ "Within last year",
      moved == "13 to 23 months" ~ "1-4 years ago",
      moved == "2 to 4 years" ~ "1-4 years ago",
      moved == "5 to 9 years" ~ "5-9 years ago",
      moved == "10 to 19 years" ~ "10-19 years ago",
      moved == "20 to 29 years" ~ "20-29 years ago",
      moved == "30 years or more" ~ "30+ years ago"
    )
  ) |> 
  mutate(
    workers = fct_case_when(
      workers == "No workers" ~ "None",
      workers == "1 worker" ~ "1",
      workers == "2 workers" ~ "2",
      workers == "3 or more workers" ~ "3 or more",
      TRUE ~ NA
    )
  ) |> 
  mutate(
    emp_status = case_when(
      str_detect(emp_status, "Civilian") ~ "Employed", 
      str_detect(emp_status, "Armed") ~ "Military",
      str_detect(emp_status, "Unemployed") ~ "Unemployed",
      str_detect(emp_status, "Not in") ~ "Not in labor force",
      .default = NA
    )
  ) |> 
  mutate(
    wkr_class = case_when(
      str_detect(wkr_class, "last worked") ~ "Non-earner",
      str_detect(wkr_class, "without pay") ~ "Non-earner",
      str_detect(wkr_class, "private for-profit") ~ "For-profit",
      str_detect(wkr_class, "not-for-profit") ~ "Non-profit",
      str_detect(wkr_class, "Local|State") ~ "Local or state government",
      str_detect(wkr_class, "Federal") ~ "Federal government",
      str_detect(wkr_class, "Self-employed") ~ "Self-employed"
    )
  ) |> 
  mutate(
    across(c(naics, soc), ~ replace(., . %in% c("N", "00000N"), NA))
  ) |> 
  mutate(
    across(
      fam_income:inc_other, ~ replace(., . %in% c(-1, -10001), NA)
    )
  ) |> 
  select(-ethnicity)
  

## 11. Save final PUMS data -------------------------------

# Join with replicate weights
pums_faar <- faar_pums_simple |> 
  left_join(pums_wgt)

# Save RDS file
write_rds(pums_faar, "data/pums/pums_faar.rds")
# read_rds("data/pums/pums_faar.rds")

## EXAMPLE CROSSTAB ---------------------------------------

# Median gross rent for renters who moved into home within less than 5 years ago by AMI
pums_faar |>
  filter(
    SPORDER == 1,
    moved %in% c("Within last year", "1-4 years"),
    tenure == "Renter"
  ) |>
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar) |>
  summarise(
    median = survey_median(cost_rent, vartype = "cv")
  )





  


