## Setup ---------------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(fredr)


## Import CoStar data --------------------------------

# To obtain data from CoStar, go to the Properties tab and then choose Multi-Family
# subtab. Filter by county and download data for each locality.

# Download data from the Data tab. Rent will be provided in current dollars.
# Note the date of data download: 6.04.2024

staff <- read_excel("data/raw/stafford_mf_grid.xlsx") |> 
  mutate(locality = "Stafford County")
fxburg <- read_excel("data/raw/fxburg_mf_grid.xlsx") |> 
  mutate(locality = "Fredericksburg")
caroline <- read_excel("data/raw/caroline_mf_grid.xlsx") |> 
  mutate(locality = "Caroline County")
orange <- read_excel("data/raw/orange_mf_grid.xlsx")|> 
  mutate(locality = "Orange County")
kg <- read_excel("data/raw/kg_mf_grid.xlsx") |> 
  mutate(locality = "King George County")
spotsy <- read_excel("data/raw/spotsy_mf_grid.xlsx") |> 
  mutate(locality = "Spotsylvania County")
region <- read_excel("data/raw/faar_costar.xlsx") |> 
  mutate(locality = "Region")


## Bind all locality data ----------------------------

costar <- rbind(staff, fxburg, caroline, orange, kg, spotsy, region) |> 
  janitor::clean_names() |> 
  filter(period != "2024 Q2 QTD") |> 
  mutate(
    year = as.integer(str_sub(period, 1, 4)),
    qtr = as.integer(str_sub(period, -1))
  ) |> 
  filter(year > 2014) |> 
  select(name = locality, year, qtr, 2:5, 12:13, 15:16, 20:21, 23:24)


## Get CPI Rent of Primary Residence -----------------

# https://fred.stlouisfed.org/series/CUUR0000SEHA

# Get quarterly CPI
cpi_q <- fredr(
  series_id = "CUUR0000SEHA",
  observation_start = as.Date("2015-01-01"),
  observation_end = as.Date("2024-03-31"),
  frequency = "q",
  aggregation_method = "avg"
) |> 
  mutate(
    year = year(date),
    qtr = quarter(date)
  ) |> 
  select(date, year, qtr, cpi = value)

# Pull latest CPI as benchmark for adjustment
cpi_latest <- fredr(
  series_id = "CUUR0000SEHA",
  observation_start = as.Date("2024-01-01")
) |> 
  slice_max(date, n = 1) |> 
  pull(3)

# As of 11/12/2024
# Most recent release: Sept 2024
# CUUR0000SEHA value: 423.821


## Prepare data for analysis -------------------------

costar_adj <- costar |> 
  left_join(cpi_q) |> 
  mutate(
    adj_asking_rent = (cpi_latest/cpi)*asking_rent_per_unit,
    .after = asking_rent_per_unit
  ) |> 
  mutate(name = str_remove_all(name, " County")) |> 
  select(1:16)


## Save CoStar data ----------------------------------

write_rds(costar_adj, "data/faar_costar.rds")




######################################################
# ERIC CODE

## Clean columns for join in needs qmd 

faar_rent_ann <- faar_costar |> select(asking_rent_per_unit, locality, year)

faar_rent_ann <- faar_rent_ann |> rename(price = asking_rent_per_unit) 

faar_rent_ann <- faar_rent_ann |>
  group_by(locality, year) |>
  summarize(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )

write_rds(faar_rent_ann, "data/faar_rent_ann.rds")

## Getting different values from Eric's in faar_rent_annual ?