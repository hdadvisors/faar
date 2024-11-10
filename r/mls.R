## Setup ----------------------------------------

library(tidyverse)
library(janitor)
library(tidygeocoder)
library(fredr)
library(lubridate)
library(sf)
library(mapview)

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


## Load and geocode csv files -------------------

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


## Add CPI adjustments --------------------------

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


## Data cleanup ---------------------------------

# Reconfigure columns and fix missing/wrong data entries
faar_fix <- faar_cpi |> 
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
    year_reno = replace(year_reno, year_reno == 2033, 2023), # Fix typo in data
    year_built = case_when(
      year_built == 2922 ~ 2022, # Fix typo in data
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
faar_mls_sf <- faar_fix |> 

  # Remove addresses geocoded outside of region and not verifiable
  filter(
    full_address != "6106 Zachary Taylor Hwy, Spotsylvania, VA, 23117",
    full_address != "3462 Willow Branch Rd Rd, Spotsylvania, VA, 23024"
  )

  # Correct coordinates for geocode errors
  mutate(
    longitude = case_match(
      full_address,
      "401 Meadows Dr, Orange, VA, 22960" ~ -78.1243503,
      "402 Meadows Drive, Orange, VA, 22960" ~ -78.1263216,
      "403 Meadows Dr, Orange, VA, 22960" ~ -78.1244872,
      "404 Meadows Drive, Orange, VA, 22960" ~ -78.1242112,
      "406 Meadows Drive, Orange, VA, 22960" ~ -78.1243743,
      "407 Meadows Dr, Orange, VA, 22960" ~ -78.1270826,
      "408 Meadows Drive, Orange, VA, 22960" ~ -78.1245322,
      "409 Meadows Dr, Orange, VA, 22960" ~ -78.1249473,
      "413 Meadows Dr, Orange, VA, 22960" ~ -78.1275386,
      "Lot 2 Independence Rd, Orange, VA, 22567" ~ -77.9408127,
      "Stonewall, Orange, VA, 22508" ~ -77.8490612,
      "18123 Wolftrap Ct, Orange, VA, 22230" ~ -78.1408539,
      "29 Royal Ct, Spotsylvania, VA, 22534" ~ -77.6536989,
      "13331 Fredericksburg, Caroline, VA, 22580" ~ -77.363643,
      "Lot 10 Hope Ln, Orange, VA, 22960" ~ -78.063874,
      "69 Potomac Lndg, King George, VA, 22485" ~ -77.2316574,
      "Lot 1 Pinewood Lane, King George, VA, 22485" ~ -77.3301686,
      .default = longitude
    )
  ) |> 
  mutate(
    latitude = case_match(
      full_address,
      "401 Meadows Dr, Orange, VA, 22960" ~ 38.2586439,
      "402 Meadows Drive, Orange, VA, 22960" ~ 38.2580879,
      "403 Meadows Dr, Orange, VA, 22960" ~ 38.258552,
      "404 Meadows Drive, Orange, VA, 22960" ~ 38.258075,
      "406 Meadows Drive, Orange, VA, 22960" ~ 38.258034,
      "407 Meadows Dr, Orange, VA, 22960" ~ 38.2584359,
      "408 Meadows Drive, Orange, VA, 22960" ~ 38.2579769,
      "409 Meadows Dr, Orange, VA, 22960" ~ 38.258382,
      "413 Meadows Dr, Orange, VA, 22960" ~ 38.2582749,
      "Lot 2 Independence Rd, Orange, VA, 22567" ~ 38.2451163,
      "Stonewall, Orange, VA, 22508" ~ 38.2559917,
      "18123 Wolftrap Ct, Orange, VA, 22230" ~ 38.1578034,
      "29 Royal Ct, Spotsylvania, VA, 22534" ~ 38.1050171,
      "13331 Fredericksburg, Caroline, VA, 22580" ~ 38.0787474,
      "Lot 10 Hope Ln, Orange, VA, 22960" ~ 38.1570256,
      "69 Potomac Lndg, King George, VA, 22485" ~ 38.330767,
      "Lot 1 Pinewood Lane, King George, VA, 22485" ~ 38.301811,
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
  filter(county != "Essex")

# Test map views
#faar_mls_sf |> mapview()

  
## Save MLS data as rds files -------------------
  
# Save with geometry
write_rds(faar_mls_sf, "data/faar_mls_sf.rds")

# Save without geometry
faar_mls_sf |> st_drop_geometry() |> write_rds("data/faar_mls.rds")




## Remove all categories but Name, Closing Date, Closing Price

faar_mls_annual <- faar_mls |> select(county, close_date, close_price)


## Rename to match values for further joins in needs qmd 

faar_mls_annual <- faar_mls_annual |> rename( locality = county,
                                              price = close_price,
                                              year = close_date
)

# Save

write_rds(faar_mls_annual, "data/faar_mls_annual.rds")