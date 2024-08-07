install.packages("lehdr")
library(lehdr)
library(dplyr)

# Define the years of interest (LODES8 only goes up to 2021)
years <- 2018:2021

# List of FIPS codes for the counties of interest
counties <- c("51630", "51033", "51099", "51177", "51179", "51137") # Example FIPS codes for counties in VA

# Initialize an empty list to store data frames
rac_list <- list()
wac_list <- list()
od_list <- list()

# Loop through each year and download the RAC data
for (year in years) {
  rac_data <- grab_lodes(state = "va", year = year, lodes_type = "rac", job_type = "JT00", segment = "S000")
  rac_data$year <- year # Add a column for the year
  rac_list[[as.character(year)]] <- rac_data
}

# Loop through each year and download the WAC data
for (year in years) {
  try({
    wac_data <- grab_lodes(state = "va", year = year, lodes_type = "wac", job_type = "JT00", segment = "S000")
    wac_data$year <- year # Add a column for the year
    wac_list[[as.character(year)]] <- wac_data
    print(paste("WAC data loaded for year:", year))
  }, silent = TRUE)
}

# Check if WAC data was successfully loaded
if (length(wac_list) == 0) {
  print("No WAC data loaded.")
} else {
  print("WAC data loaded successfully.")
}

# Loop through each year and download the OD data
for (year in years) {
  try({
    od_data <- grab_lodes(state = "va", year = year, lodes_type = "od", job_type = "JT00", segment = "S000")
    od_data$year <- year # Add a column for the year
    od_list[[as.character(year)]] <- od_data
    print(paste("OD data loaded for year:", year))
  }, silent = TRUE)
}

# Combine the list of data frames into single data frames
virginia_rac <- bind_rows(rac_list)
virginia_wac <- bind_rows(wac_list)
virginia_od <- bind_rows(od_list)

# View the structure of the combined data frames
str(virginia_rac)
str(virginia_wac)
str(virginia_od)

# Summary of the combined data frames
summary(virginia_rac)
summary(virginia_wac)
summary(virginia_od)

# Filter the data for specific counties
county_wac <- virginia_wac %>%
  filter(substr(w_geocode, 1, 5) %in% counties)

# Filter RAC data for specific counties
county_rac <- virginia_rac %>%
  filter(substr(h_geocode, 1, 5) %in% counties)

#Filter OD data for specific counties
county_od <- virginia_od %>%
  filter(substr(w_geocode, 1, 5) %in% counties | substr(h_geocode, 1, 5) %in% counties)

# Merge WAC and RAC data on the geocode columns
merged_data <- county_wac %>%
  left_join(county_rac, by = c("w_geocode" = "h_geocode"), suffix = c("_work", "_home"))

# View the first few rows of the merged data
head(merged_data)

library(ggplot2)
library(plotly)

#rename FIPS to county names

# Create a lookup table for FIPS codes and county names
fips_lookup <- data.frame(
  county_fips = c("51033", "51099", "51137", "51177", "51179", "51630"), # Example FIPS codes
  county_name = c("Caroline County", "King George County", "Orange County", "Spotsylvania County", "Stafford County", "Fredericksburg city") # Corresponding county names
) 

# Aggregate jobs by county
jobs_summary <- county_wac %>%
  mutate(county_fips = substr(w_geocode, 1, 5)) %>%
  group_by(county_fips) %>%
  summarize(total_jobs = sum(C000)) %>%
  left_join(fips_lookup, by = "county_fips") # Merge with the lookup table

# Create interactive plot
p <- ggplot(jobs_summary, aes(x = county_name, y = total_jobs)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Jobs per County", x = "County Name", y = "Total Jobs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(p)
  
# Aggregate OD data by origin and destination counties
od_summary <- county_od %>%
  mutate(origin_fips = substr(h_geocode, 1, 5),
         destination_fips = substr(w_geocode, 1, 5)) %>%
  group_by(origin_fips, destination_fips) %>%
  summarize(total_commuters = sum(S000)) %>%
  left_join(fips_lookup, by = c("origin_fips" = "county_fips")) %>%
  rename(origin_county = county_name) %>%
  left_join(fips_lookup, by = c("destination_fips" = "county_fips")) %>%
  rename(destination_county = county_name)

# View the first few rows of the aggregated OD data
head(od_summary)

# Create an interactive plot for OD data
p_od <- ggplot(od_summary, aes(x = origin_county, y = total_commuters, fill = destination_county)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Commuter Flows Between Counties", x = "Origin County", y = "Total Commuters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplotly(p_od)

##### NEW ANALYSIS

install.packages("plotly")
install.packages("sf")
install.packages("leaflet")
install.packages("tigris")
library(lehdr)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(tigris)

# Load spatial data for Virginia counties
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")

# Transform to WGS84 datum
va_counties <- st_transform(va_counties, crs = 4326)

# Filter for FAAR counties
counties_of_interest <- c("51630", "51033", "51099", "51177", "51179", "51137")
# Filter spatial data to only include counties of interest
va_counties <- va_counties %>% filter(GEOID %in% counties_of_interest)


str(va_counties)

commuter_totals$origin_fips <- as.character(commuter_totals$origin_fips)
va_counties$GEOID <- as.character(va_counties$GEOID)

# Aggregate data to represent the total number of commuters for each county
commuter_totals <- od_summary %>%
  group_by(origin_fips) %>%
  summarize(total_commuters = sum(total_commuters))


# Merge commuter totals with the spatial data
va_counties <- va_counties %>%
  left_join(commuter_totals, by = c("GEOID" = "origin_fips"))

# Define custom color palette
pal <- c(
  "#011e41", # Shadow
  "#8b85ca", # Lilac
  "#445ca9", # Blue
  "#8baeaa", # Green
  "#e9ab3f", # Yellow
  "#e76f52", # Coral
  "#b1005f"  # Berry
)

# Create a color bin function using the custom palette with updated bins
bins <- c(0, 25000, 50000, 100000, 150000, 200000, Inf) # Updated bins for the color scale
color_pal <- colorBin(palette = pal, domain = va_counties$total_commuters, bins = bins, na.color = "transparent")

# Create the Leaflet map
leaflet(data = va_counties) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(color = "#444444", weight = 1, opacity = 1.0, fillOpacity = 0.7,
              fillColor = ~color_pal(total_commuters),
              highlightOptions = highlightOptions(weight = 3, color = "#666666", fillOpacity = 0.7, bringToFront = TRUE),
              label = ~paste0(NAME, ": ", total_commuters, " commuters")) %>%
  addLegend(pal = color_pal, values = ~total_commuters, opacity = 0.7, title = "Total Commuters",
            position = "bottomright")
