library(tidycensus)
library(tidyverse)
library(hdatools)


faar <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes


# Estimating supply


# TABLE B25094: SELECTED MONTHLY OWNER COSTS


b25094_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25094",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE) |> 
  subset(GEOID %in% faar) 


b25094_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25094") |> 
  separate(label, into = c("est", "total", "monthly"), sep = "!!") |> 
  select(variable = name, monthly) |> 
  drop_na()
  

b25094 <- b25094_pull |> 
  filter(GEOID != "51137") |> 
  right_join(b25094_vars, by = "variable") |> 
  mutate(monthly = case_when(
    monthly == "Less than $200" ~ "Less than $599",
    monthly == "$200 to $299" ~  "Less than $599",
    monthly == "$300 to $399" ~  "Less than $599",
    monthly == "$400 to $499" ~  "Less than $599",
    monthly == "$500 to $599" ~  "Less than $599",
    monthly == "$600 to $699" ~  "$600 to $999",
    monthly == "$700 to $799" ~  "$600 to $999",
    monthly == "$800 to $899" ~  "$600 to $999",
    monthly == "$900 to $999" ~  "$600 to $999",
    monthly == "$1,000 to $1,249" ~  "$1,000 to $1,499",
    monthly == "$1,250 to $1,499" ~  "$1,000 to $1,499",
    monthly == "$1,500 to $1,999" ~  "$1,500 to $1,999",
    TRUE ~ "More than $2,000"
  ))

b25094_region <- b25094 |> 
  group_by(monthly) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(total = sum(estimate)) |> 
  mutate(pct = estimate/total) |> 
  mutate(vacant = pct*1049) |> 
  mutate(adj_estimate = estimate + vacant)


write_rds(b25094_region, "data/b25094_region.rds")
         
# The data visualization below shows the distribution of owner-occupied housing
# based on monthly housing costs. This viz is focused on the entire region.

custom_order <- factor(b25094_region$monthly, levels = c("Less than $599", "$600 to $999", "$1,000 to $1,499",
                                                         "$1,500 to $1,999", "More than $2,000"))

ggplot(b25094_region,
       aes(x = custom_order,
           y = estimate)) +
  geom_col(stat = "identity", fill = "#445ca9") +
  theme_hda(base_size = 10)
  

# TABLE B25063: GROSS RENT

b25063_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25063",
    year = 2022,
    survey = "acs5",
    cache_table = TRUE) |> 
  subset(GEOID %in% faar) 


b25063_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25063")|> 
  separate(label, into = c("est", "total", "cash", "rent"), sep = "!!") |> 
  select(variable = name, cash, rent) |> 
  mutate(cash = str_remove(cash, ":")) |> 
  mutate(rent = case_when(
    cash == "No cash rent" ~ "No cash rent",
    TRUE ~ rent
  ))

b25063 <- b25063_pull |> 
  filter(GEOID != "51137") |> 
  right_join(b25063_vars, by = "variable") |> 
  drop_na() |> 
  mutate(rent = case_when(
    rent == "No cash rent" ~ "Less than $599",
    rent == "Less than $100" ~ "Less than $599",
    rent == "$100 to $149" ~ "Less than $599",
    rent == "$150 to $199" ~ "Less than $599",
    rent == "$200 to $249" ~ "Less than $599",
    rent == "$250 to $299" ~ "Less than $599",
    rent == "$300 to $349" ~ "Less than $599",
    rent == "$350 to $399" ~ "Less than $599",
    rent == "$400 to $449" ~ "Less than $599",
    rent == "$450 to $499" ~ "Less than $599",
    rent == "$500 to $549" ~ "Less than $599",
    rent == "$550 to $599" ~ "Less than $599",
    rent == "$600 to $649" ~ "$600 to $999",
    rent == "$650 to $699" ~ "$600 to $999",
    rent == "$700 to $749" ~ "$600 to $999",
    rent == "$750 to $799" ~ "$600 to $999",
    rent == "$800 to $899" ~ "$600 to $999",
    rent == "$900 to $999" ~ "$600 to $999",
    rent == "$1,000 to $1,249" ~ "$1,000 to $1,499",
    rent == "$1,250 to $1,499" ~ "$1,000 to $1,499",
    rent == "$1,500 to $1,999" ~ "$1,500 to $1,999",
    TRUE ~ "More than $2,000"
  )) 


b25063_region <- b25063 |> 
  group_by(rent) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(total = sum(estimate)) |> 
  mutate(pct = estimate/total) |> 
  mutate(vacant = pct*2954) |> 
  mutate(adj_estmate = estimate + vacant)

write_rds(b25063_region, "data/b25063_region.rds")

# The data visualization below shows the distribution of renter-occupied housing
# based on monthly housing costs. This viz is focused on the entire region.

custom_order <- factor(b25063_region$rent, levels = c("Less than $599", "$600 to $999", "$1,000 to $1,499",
                                                         "$1,500 to $1,999", "More than $2,000"))

ggplot(b25063_region,
       aes(x = custom_order,
           y = estimate)) +
  geom_col(stat = "identity", fill = "#445ca9") +
  theme_hda(base_size = 10)


# TABLE B25042: TENURE BY BEDROOMS

b25042_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25042",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE) |> 
  subset(GEOID %in% faar) 


b25042_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25042") |> 
  separate(label, into = c("est", "total", "tenure", "br"), sep = "!!") |> 
  select(variable = name, tenure, br) |> 
  drop_na() |> 
  mutate(tenure = str_remove(tenure, ":")) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))
  
b25042 <- b25042_pull |> 
  filter(GEOID != "51137") |> 
  right_join(b25042_vars, by = "variable")

b25042_region <- b25042 |> 
  group_by(tenure, br) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  group_by(tenure) |> 
  mutate(total = sum(estimate)) |> 
  mutate(pct = estimate/total) |> 
  mutate(vacant = case_when(
    tenure == "Renter" ~ pct * 2954,
    tenure == "Homeowner" ~ pct * 1049
  )) |> 
  mutate(adj_estimate = estimate + vacant)


write_rds(b25042_region, "data/b25042_region.rds")

custom_order <- factor(b25042_region$br, levels = c("No bedroom", "1 bedroom", "2 bedrooms", 
                                                    "3 bedrooms", "4 bedrooms", "5 or more bedrooms"))

ggplot(b25042_region,
       aes(x = custom_order,
           y = estimate, 
           fill = tenure)) +
  geom_col(position = "dodge") +
  theme_hda(base_size = 10) +
  scale_fill_hda()
  
  
  
# TABLE B25068: BEDROOMS BY GROSS RENT

b25068_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25068",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE) |> 
  subset(GEOID %in% faar) 

b25068_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25068") |> 
  separate(label, into = c("est", "total", "br", "cash", "rent"), sep = "!!") |> 
  mutate(rent = case_when(
    cash == "No cash rent" ~ "No cash rent",
    TRUE ~ rent
  )) |> 
  mutate(rent = case_when(
    rent == "Less than $300" ~ "Less than $500",
    rent == "$300 to $499" ~ "Less than $500",
    rent == "$500 to $749" ~ "$500 to $999",
    rent == "$750 to $999" ~ "$500 to $999",
    rent == "No cash rent" ~ "Less than $500",
    TRUE ~ rent
  )) |> 
  mutate(br = str_remove(br, ":")) |> 
  drop_na() |> 
  select(variable = name, br, rent)
  
b25068 <- b25068_pull |> 
  filter(GEOID != "51137") |> 
  right_join(b25068_vars, by = "variable")

b25068_region <- b25068 |> 
  group_by(br, rent) |> 
  summarise(estimate = sum(estimate)) |> 
  ungroup() |> 
  mutate(total = sum(estimate)) |> 
  mutate(pct = estimate/total) |> 
  mutate(vacant = pct*2954) |> 
  mutate(adj_estimate = estimate + vacant)

write_rds(b25068_region, "data/b25068_region.rds")

library(gt)


tb <- b25068_region |> 
  select(br, rent, estimate) |> 
  pivot_wider(names_from = "br",
              values_from = "estimate") 


# ggplot(b25068_region,
#        aes(x = br,
#            y = estimate,
#            fill = br)) +
#   geom_col() +
#   facet_grid(~rent)


# Table B25004: VACANT UNITS

b25004_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25004",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE) |> 
  subset(GEOID %in% faar) 

b25004_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25004") |> 
  separate(label, into = c("est", "total", "vacancy"), sep = "!!") |> 
  drop_na() |> 
  select(variable = name, vacancy)

b25004 <- b25004_pull |> 
  right_join(b25004_vars, by = "variable")

b25004_region <- b25004 |> 
  filter(GEOID != "51137") |> 
  group_by(vacancy) |> 
  summarise(estimate = sum(estimate)) 

# 2,954 vacant rental units
# 1,049 vacant for-sale units
  
# TABLE B25127: TENURE BY YEAR BUILT AND STRUCTURE TYPE

years <- 2019:2022

b25127_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25127")

b25127_raw <- map_dfr(years, function(yr){
  b25127_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25127",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |> 
    mutate(year = yr)
})

b25127_raw <- b25127_raw |> 
  subset(GEOID %in% faar) 

b25127_vars_cleaned <- b25127_vars |> 
  separate(label, into = c("est", "total", "tenure", "yrbuilt", "structure"), sep = "!!") |> 
  select(variable = name, tenure, yrbuilt, structure) |> 
  drop_na() |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  mutate(tenure = case_when(
    tenure == "Owner occupied" ~ "Homeowner",
    tenure == "Renter occupied" ~ "Renter"
  ))

b25127_raw <- b25127_raw |> 
  right_join(b25127_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate, moe)

b25127_data <- b25127_raw |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia"),
         yrbuilt = str_remove_all(yrbuilt, "Built ")) |>
  select(NAME, GEOID, year, tenure, yrbuilt, structure, estimate) |> 
  mutate(structure = case_when(
    structure == "1, detached or attached" ~ "Single-family",
    structure == "2 to 4" ~ "2 to 4 units",
    structure == "5 to 19" ~ "5 to 19 units",
    structure == "20 to 49" ~ "20 or more units",
    structure == "50 or more" ~ "20 or more units",
    TRUE ~ "Other"
  )) 

b25127_region <- b25127_data |> 
  filter(GEOID != "51137") |> 
  group_by(year, yrbuilt, structure, tenure) |> 
  summarise(estimate = sum(estimate)) |> 
  filter(year == 2022) |> 
  ungroup() |> 
  group_by(tenure) |> 
  mutate(total = sum(estimate)) |> 
  mutate(pct = estimate/total) |> 
  mutate(vacant = case_when(
    tenure == "Renter" ~ pct * 2954,
    tenure == "Homeowner" ~ pct * 1049
  )) |> 
  mutate(adj_estimate = estimate + vacant)

write_rds(b25127_region, "data/b25127_region.rds")


ggplot(b25127_region,
       aes(x = yrbuilt,
           y = estimate,
           fill = structure)) +
  geom_col() +
  theme_hda(base_size = 10) +
  scale_fill_hda(direction = -1) +
  facet_wrap(~tenure)


# Estimating housing demand


# To estimate future housing demand over 10 or 20 years consider the following:
# Average growth rate of renter and owner occupied households.
# Average growth rate of average household size


# TABLE B25118: TENURE BY HOUSEHOLD INCOME


# Define AMI categories (e.g., 30%, 50%, 80%, 100%, and 120% of AMI)
ami_levels <- c(0.3, 0.5, 0.8, 1.0, 1.2)

pums_data <- get_pums(
  variables = c("PUMA20", "TEN", "GRNTP", "SMOCP", "HINCP", "ADJINC", "ADJHSG"),
  year = 2022,
  state = "VA",
  survey = "acs5",
  recode = TRUE
)

pums_fxburg <- pums_data |> 
  filter(PUMA20 %in% c("17700", "17900")) |> 
  filter(TEN_label != "N/A (GQ/vacant)") |> 
  mutate(tenure = case_when(
    TEN_label == "Rented" ~ "Renter",
    TEN_label == "Owned with mortgage or loan (include home equity loans)" ~ "Homeowner",
    TEN_label == "Owned free and clear" ~ "Homeowner",
    TEN_label == "Occupied without payment of rent" ~ "Renter"
  )) |> 
  mutate(pct_ami = HINCP/142300) |>
  mutate(ami = case_when(
    pct_ami <= 0.3 ~ "30% AMI or below",
    pct_ami > 0.3 & pct_ami <= 0.5 ~ "31-50% AMI",
    pct_ami > 0.5 & pct_ami <= 0.8 ~ "51-80% AMI",
    pct_ami > 0.8 & pct_ami <= 1 ~ "81-100% AMI",
    pct_ami > 1 & pct_ami <= 1.20 ~ "101-120% AMI",
    pct_ami > 1.20 ~ "121% AMI and greater")) 



supply <- pums_fxburg |> 
  select(SPORDER, tenure, ami, pct_ami, SMOCP, GRNTP, ADJHSG, ADJINC, WGTP) |> 
  mutate(ADJINC = as.numeric(ADJINC)) |> 
  filter(SPORDER ==1) |> 
  mutate(cost = case_when(
    tenure == "Renter" ~ GRNTP,
    tenure == "Homeowner" ~ SMOCP
  )) |> 
  mutate(cost = ADJINC * cost,
         annual_cost = 12*cost,
         income = (annual_cost*10)/3) |> 
  mutate(pct_ami_unit = income/142300) |>
  mutate(ami_unit = case_when(
    pct_ami_unit <= 0.3 ~ "30% AMI or below",
    pct_ami_unit > 0.3 & pct_ami_unit  <= 0.5 ~ "31-50% AMI",
    pct_ami_unit > 0.5 & pct_ami_unit  <= 0.8 ~ "51-80% AMI",
    pct_ami_unit > 0.8 & pct_ami_unit  <= 1 ~ "81-100% AMI",
    pct_ami_unit > 1 & pct_ami_unit  <= 1.20 ~ "101-120% AMI",
    pct_ami_unit > 1.20 ~ "121% AMI and greater")) |> 
  select(WGTP, tenure, ami, ami_unit)



fxburg_units <- supply |> 
  group_by(ami_unit, tenure) |> 
  summarise(estimate = sum(WGTP)) |> 
  select(ami = ami_unit, tenure, estimate)
  
fxburg_hh <- supply |> 
  group_by(ami, tenure) |> 
  summarise(estimate = sum(WGTP)) 

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

write_rds(fxburg_supply, "data/fxburg_supply_demand.rds")


ami_order <- factor(fxburg_supply$ami, levels = c("30% AMI or below", "31-50% AMI",
                                           "51-80% AMI", "81-100% AMI", "101-120% AMI",
                                           "121% AMI and greater"))

library(ggtext)

title_text <- "<span style = 'color:#8baeaa'><b>Supply</b></span> and <span style = 'color:#445ca9'><b>Demand</span>"




ggplot(fxburg_supply,
       aes(x = ami_order,
           y = estimate,
           fill = value)) +
  geom_col(position = "dodge") +
  facet_wrap(~tenure) +
  theme_hda(base_size = 10) +
  scale_fill_hda() +
  theme(legend.position = "right")







|> 
  summarise(med_inc = weighted.median(HINCP, WGTP, na.rm = TRUE))

pum


median_income <- 114850 # Weighted median household income 
income_thresholds <- ami_levels * median_income



