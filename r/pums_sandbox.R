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
                "WIF", "NAICSP", "NOC", "TEN", "HHLDRHISP", "HHLDRRAC1P", "SMOCP", "GRNTP"),
  year = 2022,
  state = "VA",
  survey = "acs5",
  recode = TRUE
) |> 
  filter(PUMA20 %in% faar)

# HUD 2022 Income Limits

hud_ami <- read_csv("data/raw/hud_ami.csv") |> 
  pivot_wider(names_from = "ami") |> 
  janitor::clean_names() |> 
  select(NP = size, 2:6)


pums_join <- pums_data |> 
  left_join(hud_ami, by = "NP") |> 
  mutate(ami = case_when(
    HINCP <= x30 ~ "30% AMI or less",
    HINCP > x30 & HINCP <= x50 ~ "31 to 50% AMI",
    HINCP > x50 & HINCP <= x80 ~ "51 to 80% AMI",
    HINCP > x80 & HINCP <= x100 ~ "81 to 100% AMI",
    HINCP > x100 & HINCP <= x120 ~ "101 to 120% AMI",
    TRUE ~ "121% AMI or more"
  )) |> 
  filter(TEN_label != "N/A (GQ/vacant)") |> 
  filter(SPORDER == 1) |> 
  mutate(ADJINC = as.numeric(ADJINC)) |> 
  mutate(race = case_when(
    HHLDRHISP_label != "Not Spanish/Hispanic/Latino" ~ "Hispanic, or Latino",
    HHLDRHISP_label == "Not Spanish/Hispanic/Latino" ~ HHLDRRAC1P_label
  )) |>  
  mutate(tenure = case_when(
    TEN_label == "Rented" ~ "Renter",
    TEN_label == "Owned with mortgage or loan (include home equity loans)" ~ "Homeowner",
    TEN_label == "Owned free and clear" ~ "Homeowner",
    TEN_label == "Occupied without payment of rent" ~ "Renter"
  )) |> 
  mutate(cost = case_when(
    tenure == "Renter" ~ GRNTP,
    tenure == "Homeowner" ~ SMOCP
  )) |> 
  mutate(cost = ADJINC * cost,
         annual_cost = 12*cost,
         income = (annual_cost*10)/3) |> 
  mutate(pct_ami_unit = income/142300) |>
  mutate(ami_unit = case_when(
    pct_ami_unit <= 0.3 ~ "30% AMI or less",
    pct_ami_unit > 0.3 & pct_ami_unit  <= 0.5 ~ "31 to 50% AMI",
    pct_ami_unit > 0.5 & pct_ami_unit  <= 0.8 ~ "51 to 80% AMI",
    pct_ami_unit > 0.8 & pct_ami_unit  <= 1 ~ "81 to 100% AMI",
    pct_ami_unit > 1 & pct_ami_unit  <= 1.20 ~ "101 to 120% AMI",
    pct_ami_unit > 1.20 ~ "121% AMI or more"))
  
fxburg_units <- pums_join |> 
  group_by(ami_unit, tenure) |> 
  summarise(estimate = sum(WGTP)) |> 
  select(ami = ami_unit, tenure, estimate)

fxburg_hh <- pums_join |> 
  group_by(ami, tenure) |> 
  summarise(estimate = sum(WGTP)) 

pums_race <- pums_join |> 
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


# TOP INDUSTRIES BY AMI

top_jobs <- pums_join |> 
filter(NAICSP != "N") |> 
group_by(ami, tenure, NAICSP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()


# Summary Stats
# Average household age
# Median Household Income
# Average number of workers

stat <- pums_join |> 
  mutate(WIF = as.numeric(WIF)) |> 
  mutate(NOC = as.numeric(NOC)) |>
  group_by(tenure, ami) |> 
  summarise(
    mean_hhage = weighted.mean(HHLDRAGEP, WGTP),
    med_inc = weighted.median(HINCP, WGTP, na.rm = TRUE),
    mean_wif = weighted.mean(WIF, WGTP, na.rm = TRUE))

# Top Household Type by AMI

top_hht <- pums_join |> 
  group_by(ami, tenure, HHT2_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami) |> 
  arrange(ami, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()
