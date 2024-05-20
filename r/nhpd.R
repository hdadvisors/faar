library(tidyverse)
library(readxl)
library(janitor)

faar <- c("Fredericksburg City", "Stafford", "Caroline",
           "Orange", "Spotsylvania", "King George")

properties <- read_excel("data/raw/nhpd_va_pull_05.20.24.xlsx") |>
  clean_names() |> 
  filter(county %in% faar) |>
  select(nhpd_property_id, property_name, property_address, city, state, zip_code, total_units,
         subsidies = total_active_subsidies, property_status, earliest_start_date,
         latest_end_date, earliest_end_date, lat = latitude, long = longitude, county, 
         county_code, manager_name, manager_type, owner_type, 
         br1 = x0_1_bedroom_units, br2 = two_bedroom_units, br3 =three_bedroom_units, 
         fmr = fair_market_rent, target = target_tenant_type) |> 
  filter(property_status == c("Active", "Inconclusive"))

subsidies <- read_excel("data/raw/nhpd_va_pull_sub_05.20.24.xlsx") |> 
  clean_names() |> 
  left_join(properties, by = "nhpd_property_id") |> 
  select(nhpd_property_id, subsidy_status, subsidy_name, subsidy_subname,
         start_date, end_date, assisted_units, property_name.x) |> 
  filter(subsidy_status == c("Active", "Inconclusive"))


write_rds(properties, "data/nhpd_properties.rds")
write_rds(subsidies, "data/nhpd_subsidies.rds")