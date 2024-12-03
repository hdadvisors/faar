## Setup ----------------------------------------

library(tidyverse)
library(janitor)
library(tidygeocoder)
library(fredr)
library(sf)
library(mapview)
library(hdatools)

# Download BrightMLS data for the study area
# (Closed home sales from 5.2.2020 to 11.30.2024)

## Add May-Nov 2024 data ------------------------

extra_mls <- read_csv("data/raw/extra_mls.csv") |>
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

# Get monthly CPI
cpi_sales <- fredr(
  series_id = "CUUR0000SA0L2",
  observation_start = as.Date("2020-01-01"),
  observation_end = as.Date("2024-11-30")
) |> 
  select(month = 1, cpi = 3) |> 
  add_row(
    month = as.Date("2024-11-01"), cpi = tail(pull(cpi_sales, cpi), 1)
  )

cpi_latest <- tail(cpi_sales$cpi, 1)

# Adjust prices by CPI
extra_cpi <- extra_mls |> 
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


## Data cleanup ---------------------------------

# Reconfigure columns and fix missing/wrong data entries
extra_fix <- extra_cpi |> 
  filter(structure_type != "Garage/Parking Space") |>
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
    nc = fct_case_when(
      nc == "Yes" ~ "New Construction",
      nc == "No" ~ "Resale"
    )
  ) |> 
  mutate(
    br_half = replace_na(br_half, 0), # Replace NAs with 0
    year_built = case_when(
      year_built == 2924 ~ 2024, # Fix typo in data
      year_built %in% c(0, 9999) & nc == "No" ~ NA, # Replace invalid dates with NA
      year_built == 0 & nc == "Yes" ~ year(close_date), # Assign year_built for NC homes when data = 0
      is.na(year_built) & nc == "Yes" ~ year(close_date), # Assign year_built for NC homes when data is NA
      .default = year_built
    )
  )

## Geometry cleanup -----------------------------

# Load locality geometries
va_local <- read_rds("data/ami/local_va.rds") |> 
  mutate(
    NAMELSAD = str_remove_all(NAMELSAD, " city| County")
  )

# Final cleanup of geocoded data
extra_mls_sf <- extra_fix |> 
  
  # Correct coordinates for geocode errors
  mutate(
    longitude = case_match(
      full_address,
      "Lot 3 Windsor Dr., King George, VA, 22485" ~ -77.0816349,
      "407 Meadows Drive, Orange, VA, 22960" ~ -78.1270826,
      "409 Meadows Dr, Orange, VA, 22960" ~ -78.1249473,
      .default = longitude
    )
  ) |>
  mutate(
    latitude = case_match(
      full_address,
      "Lot 3 Windsor Dr., King George, VA, 22485" ~ 38.3076138,
      "407 Meadows Drive, Orange, VA, 22960" ~ 38.2584359,
      "409 Meadows Dr, Orange, VA, 22960" ~ 38.258382,
      .default = latitude
    )
  ) |>
  
  # Add point geometries
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  
  # Spatial join with actual locality boundaries
  st_join(va_local) |> 
  
  # Update county names to actual localities based on spatial join
  mutate(county = NAMELSAD) |> 
  
  # Remove homes not actually in region
  filter(!county %in% c("Greene")) |> 

  # Add year and quarter columns
  mutate(
    year = year(close_date),
    qtr = quarter(close_date)
  ) |>

  # Final column arrangement
  select(
    name = county, fips = GEOID, year, qtr, 2:25
  )

# Test map views
#extra_mls_sf |> filter(NAMELSAD == "Greene") |>  mapview()


## Combine with original data -------------------

# Update CPI adjustments to latest  
faar_mls_sf <- read_rds("data/faar_mls_sf.rds") |> 
  mutate(
    month = floor_date(close_date, "month")
  ) |> 
  left_join(cpi_sales, by = "month") |> 
  mutate(
    adj_list = cpi_latest/cpi * list_price,
    adj_close = cpi_latest/cpi * close_price
  ) |> 
  select(-29, -30)

# Combine and do some belated cleanup
combo_mls_sf <- bind_rows(faar_mls_sf, extra_mls_sf) |> 
  filter(
    structure_type %in% c(
      "Detached", "Interior Row/Townhouse", "End of Row/Townhouse", "Twin/Semi-Detached",
      "Mobile Pre 1976", "Manufactured" 
    )
  ) |> 
  mutate(
    fin_sqft = na_if(fin_sqft, 0)
  )


## Save updated MLS data as rds files -----------

# Save with geometry
write_rds(combo_mls_sf, "data/faar_mls_sf.rds")

# Save without geometry
combo_mls_sf |> st_drop_geometry() |> write_rds("data/faar_mls.rds")


## Annual and quarterly summary sales data ------

# Annual
faar_mls_a <- read_rds("data/faar_mls.rds") |> 
  summarise(
    med_price = median(close_price),
    avg_price = mean(close_price),
    adj_med_price = median(adj_close),
    adj_avg_price = mean(adj_close),
    .by = c(name, year)
  ) |> 
  mutate(qtr = NA, .after = year) |> 
  mutate(period = "annual", .before = 1)

# Quarterly
faar_mls_q <- read_rds("data/faar_mls.rds") |> 
  summarise(
    med_price = median(close_price),
    avg_price = mean(close_price),
    adj_med_price = median(adj_close),
    adj_avg_price = mean(adj_close),
    .by = c(name, year, qtr)
  ) |> 
  mutate(period = "quarterly", .before = 1)

# Combine
faar_sales <- bind_rows(
  faar_mls_a,
  faar_mls_q
)

write_rds(faar_sales, "data/faar_sales.rds")

