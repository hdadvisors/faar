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
  mutate(locality = "Caroline Spotsylvania County")

costar <- rbind(staff, fxburg, caroline, orange, kg, spotsy) |> 
  janitor::clean_names() |> 
  mutate(quarters = as.yearqtr(period)) |> 
  mutate(year = year(quarters)) |> 
  filter(year >= 2016)


cpi_ls <- fredr(
  series_id = "CUSR0000SA0L2" # Consumer Price Index for All Urban Consumers: All Items Less Shelter in U.S. City Average
) |> 
  mutate(quarters = as.yearqtr(date)) |> 
  mutate(year = year(quarters)) |> 
  filter(year >= 2016) |> 
  group_by(quarters) |> 
  summarise(cpi = mean(value))



