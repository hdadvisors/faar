library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(fredr)

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

costar <- rbind(staff, fxburg, caroline, orange, kg, spotsy, region) |> 
  janitor::clean_names() |> 
  mutate(quarters = as.yearqtr(period)) |> 
  mutate(year = year(quarters)) |> 
  filter(year >= 2016)


cpi_rent <- fredr(
  series_id = "CUUR0000SA0L2" # Consumer Price Index for All Urban Consumers: Rent of Primary Residence in U.S. City Average
) |> 
  mutate(quarters = as.yearqtr(date)) |> 
  mutate(year = year(quarters)) |> 
  filter(year >= 2016) |> 
  group_by(quarters) |> 
  summarise(cpi = mean(value))


costar_adj <- costar |> 
  left_join(cpi_rent, by = "quarters") |> 
  mutate(adj_rent = (284.2240/cpi) * asking_rent_per_unit) 


write_rds(costar_adj, "data/faar_costar.rds")
