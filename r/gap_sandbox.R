library(tidycensus)
library(tidyverse)


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
  filter(str_sub(name, end = 6) %in% "B25068") 




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
  mutate(TEN_label = case_when(
    TEN_label == "Rented"
  ))



|> 
  summarise(med_inc = weighted.median(HINCP, WGTP, na.rm = TRUE))

pum


median_income <- 114850 # Weighted median household income 
income_thresholds <- ami_levels * median_income



