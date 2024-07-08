## Prepare PUMS Data for Housing Spectrum -----------------


# 1. Setup ------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)

pums_raw <- read_rds("data/pums/pums_raw.rds")
pums_wgt <- read_rds("data/pums/pums_wgt.rds")
faar_ami <- read_rds("data/pums/faar_ami_pums.rds")


# 2. Join AMI limits to PUMS data -------------------------

# Prepare AMI limits to join
faar_ami_join <- faar_ami |> 
  mutate(NP = as.numeric(str_sub(hh_size, end = 1))) |> 
  select(6, 4, 5) |> 
  pivot_wider(names_from = level, values_from = income)

# Join AMI limits and categorize
faar_pums_ami <- pums_raw |>
  filter(!TEN == "b") |> 
  mutate(NP = pmin(NP, 8)) |> 
  left_join(faar_ami_join, by = "NP") |>
  mutate(
    ami_faar = case_when(
      HINCP < 1 ~ "Zero or negative income",
      HINCP <= ami30 ~ "Below 30% AMI",
      HINCP <= ami50 ~ "30-50% AMI",
      HINCP <= ami80 ~ "50-80% AMI",
      HINCP <= ami100 ~ "80-100% AMI",
      HINCP <= ami120 ~ "100-120% AMI",
      TRUE ~ "Above 120% AMI"
    )
  ) |> 
  select(-ami30, -ami50, -ami80, -ami100, -ami120)


# 3. Calculate number of earners in household -------------

# Identify persons with wages or salary at least $5000 in last 12 months
faar_pums_earners <- faar_pums_ami |> 
  group_by(SERIALNO) |> 
  mutate(
    hh_earners = sum(WAGP >= 5000)
  ) |> 
  ungroup()


# 4. Add place of work labels -----------------------------

pow_lookup <- read_rds("data/pums/puma_pow_lookup.rds")

pow_lookup_2010 <- pow_lookup |> 
  filter(version == "PUMACE10") |> 
  select(-1, POWPUMA10 = puma)

pow_lookup_2020 <- pow_lookup |> 
  filter(version == "PUMACE20") |> 
  select(-1, POWPUMA20 = puma)

faar_pums_pow <- faar_pums_earners |> 
  mutate(
    POWSP = case_when(
      POWSP == "00N" ~ NA,
      str_starts(POWSP, "0") ~ str_sub(POWSP, start = 2, end = 3),
      .default = POWSP
    )
  )


pums_fuse_pumas <- function(data, lookup) {
  
  pow_lookup_2010 <- lookup |> 
    filter(version == "PUMACE10") |> 
    
  
  data_pow <- data |> 
    left_join()
  
  data |> 
    mutate(
      puma = case_when(
        PUMA10 == "-0009" ~ PUMA20,
        PUMA20 == "-0009" ~ PUMA10
      ),
      puma_pow = case_when(
        POWPUMA10 == "-0009" ~ PUMA20,
        POWPUMA20 == "-0009" ~ PUMA10,
        
      )
      
    )
  
}

faar_pums_fuse <- faar_pums_earners |> 
  left_join(pow_lookup_2010) |> 
  left_join(pow_lookup_2020, by = "POWPUMA20")

  
# 3. Rename PUMS variables as needed

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
      ssi = SSIP,
      ss = SSP,
      wages = WAGP,
      str_type = BLD,
      snap = FS,
      tenure = TEN,
      str_yrblt = YRBLT,
      ethnicity = HHLDRHISP,
      race = HHLDRRAC,
      hh_type = HHT2,
      moved = MV,
      workers = WIF,
      wkr_class = COW,
      medicare = HINS3,
      medicaid = HINS4,
      disability = DIS,
      naics = NAICSP,
      soc = SOCP
    ) |> 
    
    select(-ST, )
    rename_with(to_lower)
  
}

faar_pums_recode <- pums_rename(faar_pums_earners)

  # 3. Recode PUMS data as needed
  

# 4. ---------------------------------


# 5.  ------------------



 
  

  
  
# 7. Verify number of earners in household ----------------

pums_earners <- pums_ami |> 
  group_by(SERIALNO) |> 
  mutate(
    earner = PINCP > 1000,
    hh_earners = sum(earner),
    .after = 1
  )
  
  
  
  select(-ST, -ADJINC, -NAICSP, -OCCP, -ST_label, -ADJINC_label) |> 
  rename(
    hh_size = NP,
    hh_age = HHLDRAGEP,
    hh_income = HINCP,
    fam_income = FINCP,
    children = NOC,
    cost_own = SMOCP,
    cost_rent = GRNTP,
    bedrooms = BDSP,
    structure = BLD_label,
    workers = WIF_label
    ) |> 
  mutate(
    tenure = case_when(
      str_detect(TEN_label, "(?i)rent") ~ "Renter",
      str_detect(TEN_label, "Owned") ~ "Homeowner"
    ),
    tenure_detail = case_when(
      TEN_label == "Rented" ~ "Renter",
      TEN_label == "Owned with mortgage or loan (include home equity loans)" ~ "Homeowner",
      TEN_label == "Owned free and clear" ~ "Homeowner (no mortgage)",
      TEN_label == "Occupied without payment of rent" ~ "Renter (no rent)"
    ),
    race = case_when(
      HHLDRHISP_label != "Not Spanish/Hispanic/Latino" ~ "Hispanic or Latino",
      HHLDRHISP_label == "Not Spanish/Hispanic/Latino" ~ HHLDRRAC1P_label
    ),
    hh_type = case_when(
      HHT2_label 
    )
    .after = 4
  ) |> 
  mutate(
    workers = case_when(
      hh_size == 1 & OCCP_label != "NA" ~ "1 worker",
      .default = workers
    ),
    .after = bedrooms
  ) |> 
  select(-TEN_label, -HHLDRHISP_label, -HHLDRRAC1P_label, -HHT2_label)




# 8. Survey

# Convert to survey object
pums_svy_h <- pums_ami |> 
  to_survey(type = "housing", design = "rep_weights")

pums_ami <- pums_svy_h |> 
  group_by(ami_category) |> 
  summarise(
    estimate = survey_total()
  )

  
  

  
fxburg_units <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami_unit, tenure) |> 
  summarise(estimate = sum(WGTP)) |> 
  select(ami = ami_unit, tenure, estimate)

fxburg_hh <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami, tenure) |> 
  summarise(estimate = sum(WGTP)) 

pums_race <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami, race) |> 
  summarise(estimate = sum(WGTP)) |> 
  ungroup() |> 
  group_by(ami) |> 
  mutate(pct = estimate/(sum(estimate)))

fxburg_supply <- fxburg_units |> 
  full_join(fxburg_hh, by = c("ami", "tenure")) |> 
  select(ami, tenure, hh = estimate.y, units = estimate.x) |> 
  pivot_longer(cols = 3:4,
               names_to = "value",
               values_to = "estimate") |> 
  mutate(value = case_when(
    value == "hh" ~ "Demand",
    value == "units" ~ "Supply"
  ))

ggplot(pums_race,
       aes(x = ami,
           y = pct,
           fill = race)) +
  geom_col(position = "stack") +
  coord_flip()


ami_order <- factor(fxburg_supply$ami, levels = c("30% AMI or less", "31 to 50% AMI",
                                                  "51 to 80% AMI", "81 to 100% AMI", "101 to 120% AMI",
                                                  "121% AMI or more"))


ggplot(fxburg_supply,
       aes(x = ami_order,
           y = estimate,
           fill = value)) +
  geom_col(position = "dodge") +
  facet_wrap(~tenure) +
  theme_hda(base_size = 10) +
  scale_fill_hda() +
  theme(legend.position = "right")

# Top Household Type

top_hht <- pums_join |> 
  group_by(ami, tenure, HHT2_label) |> 
  summarise(count = sum(WGTP)) |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup() |> 
  select(ami, tenure, HHT2_label)


# TOP INDUSTRIES BY AMI

top_occ <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> # Nesting based on top household type
  mutate(OCCP_label = as.character(OCCP_label)) |> 
  filter(OCCP_label != "NA") |> 
  group_by(ami, tenure, OCCP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()

top_jobs <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> 
  filter(NAICSP != "N") |> 
  group_by(ami, tenure, NAICSP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()

top_bld <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> 
  group_by(ami, tenure, BLD_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()


# Summary Stats
# Average household age
# Median Household Income
# Average number of workers
# Average number of children

stat <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> # Nesting based on top household type
  mutate(WIF = as.numeric(WIF)) |> 
  mutate(NOC = as.numeric(NOC)) |>
  mutate(BDSP = as.numeric(BDSP)) |> 
  group_by(tenure, ami) |> 
  summarise(
    mean_hhage = weighted.mean(HHLDRAGEP, WGTP),
    med_inc = weighted.median(HINCP, WGTP, na.rm = TRUE),
    mean_wif = weighted.mean(WIF, WGTP, na.rm = TRUE),
    mean_noc = round(weighted.mean(NOC, WGTP, na.rm = TRUE), 1),
    mean_bed = weighted.mean(BDSP, WGTP, na.rm = TRUE))

# Top Household Type by AMI



# Create a data frame that breaks out
# 

new_order <- c("30% AMI or less", "31 to 50% AMI", "51 to 80% AMI", "81 to 100% AMI",
               "101 to 120% AMI", "121% AMI or more")

profile <- top_jobs |> 
  left_join(stat, by = c("ami", "tenure")) |> 
  left_join(top_hht, by = c("ami", "tenure")) |> 
  left_join(top_occ, by = c("ami", "tenure")) |> 
  left_join(top_bld, by = c("ami", "tenure")) |> 
  mutate(Industry = str_replace(NAICSP_label, ".*-", "")) |> 
  mutate(Industry = str_replace_all(Industry, "\\s*\\([^\\)]+\\)", "")) |> 
  mutate(Occupation = str_replace(OCCP_label, ".*-", "")) 

|> 
  mutate(hht = case_when(
    HHT2_label == "Married couple household with children of the householder less than 18" ~ "Married couple with children",
    HHT2_label == "Married couple household, no children of the householder less than 18" ~ "Married couple",
    HHT2_label == "Female householder, no spouse/partner present, living alone" ~ "Single female",
    HHT2_label == "Male householder, no spouse/partner present, living alone" ~ "Single male"
  )) |> 
  mutate(structure = case_when(
    BLD_label == "One-family house detached" ~ "Single-family detached home",
    BLD_label == "10-19 Apartments" ~ "Small-sized multifamily (10-19 units)",
    BLD_label == "20-49 Apartments" ~ "Medium-sized multifamily (20 to 49 units)" 
  )) |> 
  select(ami, tenure, Industry, hht, structure, mean_hhage, mean_wif, med_inc, mean_noc)


profile$ami <- factor(profile$ami, levels = new_order) 
  

profile_table <- profile |> 
  arrange(ami) 

