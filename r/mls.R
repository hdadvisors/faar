# Setup

library(tidyverse)
library(janitor)
library(tidygeocoder)
library(fredr)
library(lubridate)

# Download BrightMLS data for the study area
# (Closed home sales from 1.1.2020 to 5.1.2024)

# List all MLS csv files
files <- list.files(
  path = "data/raw",
  pattern = "_mls\\.csv$",  # Match files ending with _mls.csv
  full.names = TRUE         # Get full file paths
)

# Create empty list to store dataframes
geocoded_dfs <- list()

# Import each file and geocode addresses
for (file in files) {
  
  # Extract the name before the underscore
  obj_name <- str_extract(basename(file), "^[^_]+")
  
  # Read in the CSV file and assign to the extracted name
  current_df <- read_csv(file) |>
    clean_names() |> 
    select(!c(2, 3, 12, 26, 27)) |>
    mutate(
      county = str_remove_all(county, ", VA"),
      zip_code = as.character(zip_code),
      full_address = paste(address, county, "VA", zip_code, sep = ", ")
    ) |> 
    geocode(
      address = full_address,
      method = 'geocodio',
      lat = latitude,
      long = longitude
    )
  
  # Store the dataframe in our list with its name
  geocoded_dfs[[obj_name]] <- current_df

}

# Combine all dataframes into one
faar_geocode <- bind_rows(geocoded_dfs)

# Get monthly CPI
cpi_sales <- fredr(
    series_id = "CUUR0000SA0L2",
    observation_start = as.Date("2020-01-01"),
    observation_end = as.Date("2024-05-01")
  ) |> 
  select(month = 1, cpi = 3)

cpi_latest <- tail(cpi_sales$cpi, 1)

# Adjust prices by CPI
faar_cpi <- faar_geocode |> 
  mutate(
    close_date = as_date(close_date, format = "%m/%d/%y"),
    across(11:12, ~as.numeric(gsub("[\\$,]", "", .x)))
  ) |> 
  mutate(
    month = floor_date(close_date, "month"),
    .after = close_date
  ) |> 
  left_join(cpi_sales, by = "month") |> 
  mutate(
    adj_list = cpi_latest/cpi * list_price,
    adj_close = cpi_latest/cpi * close_price,
    .after = close_price
  )

# Reconfigure columns and fix missing/wrong data entries
faar_mls <- faar_cpi |> 
  select(
    county,
    full_address,
    zip = zip_code,
    close_date,
    close_month = month,
    dom,
    cdom,
    structure_type,
    year_built,
    nc = new_construction_yn,
    fin_sqft = total_finished_sqft,
    beds,
    br_full = bathrooms_full,
    br_half = bathrooms_half,
    lot_size = lot_size_acres,
    year_reno = year_major_reno_remodel,
    property_condition,
    list_price,
    close_price,
    adj_list,
    adj_close,
    sale_type,
    close_sale_type,
    buyer_financing,
    latitude,
    longitude
  ) |> 
  mutate(
    county = str_remove_all(county, " City"), # Shorten to Fredericksburg
    br_half = replace_na(br_half, 0), # Replace NAs with 0
    year_reno = replace(year_reno, year_reno == 2033, 2023), # Fix typo in data
    year_built = case_when(
      year_built == 2922 ~ 2022, # Fix typo in data
      year_built %in% c(0, 9999) & nc == "No" ~ NA, # Replace invalid dates with NA
      year_built == 0 & nc == "Yes" ~ year(close_date), # Assign year_built for NC homes when data = 0
      is.na(year_built) & nc == "Yes" ~ year(close_date), # Assign year_built for NC homes when data is NA
      .default = year_built
    )
  )

write_rds(faar_mls, "data/faar_mls.rds")
  
