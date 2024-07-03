library(tidycensus)
library(dplyr)
library(stringr)
library(purrr)

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2019:2022

# Load variables for ACS 5-year estimates
b25009_vars <- load_variables(2022, "acs5", cache = TRUE) %>%
  filter(str_sub(name, end = 6) == "B25009")

# Pull ACS data for the specified years
b25009_raw <- map_dfr(years, function(yr) {
  get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) %>%
    mutate(year = yr)
})

# Clean and categorize household size and tenure type
b25009_clean <- b25009_raw %>%
  filter(GEOID %in% cv) %>%
  select(year, GEOID, NAME, B25009_002E, B25009_003E, B25009_004E, B25009_005E, B25009_006E) %>%
  mutate(
    one_person = B25009_002E,
    two_person = B25009_003E,
    three_person = B25009_004E,
    four_person_or_more = B25009_005E + B25009_006E,
    rental = B25009_002E + B25009_004E + B25009_006E,
    homeownership = B25009_003E + B25009_005E
  ) %>%
  select(year, GEOID, NAME, one_person, two_person, three_person, four_person_or_more, rental, homeownership)

# Explanation:
# - Removed B25009_001E from select since it was causing an error.
# - Adjusted mutate to correctly calculate rental (sum of renter-occupied) and homeownership (sum of owner-occupied).
# - Selected relevant columns for analysis or export.

# Preview the cleaned data
head(b25009_clean)


# Print the current working directory
current_dir <- getwd()
print(current_dir)

# Optionally set a new working directory
# setwd("path/to/your/directory")

# Save the cleaned data as an RDS file
write_rds(b25009_clean, "path/to/save/b25009_clean.rds")


#issues with getting this data set where I want it, 

## Going back to using B25010 instead of B25009 
#Pulling from 2010-2022 

cv <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

years <- 2010:2022

b25010_vars <- load_variables(2022, "acs5") |> 
  filter(str_sub(name, end = 6) %in% "B25009")

b25010_raw <- map_dfr(years, function(yr){
  b25010_pull <- get_acs(
    geography = "county",
    state = "VA",
    table = "B25009",
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

b25010_raw <- b25010_raw |> 
  subset(GEOID %in% cv)

b25010_vars_cleaned <- b25010_vars |> 
  separate(label, into = c("est", "total", "tenure", "size"), sep = "!!") |> 
  select(variable = name, tenure, size)  |> 
  drop_na() |>
  mutate(tenure = case_when(
    tenure == "Owner occupied:" ~ "Homeowner",
    tenure == "Renter occupied:" ~ "Renter"
  ))

b25010_data <- b25010_raw |> 
  right_join(b25010_vars_cleaned, by = "variable") |> 
  select(NAME, GEOID, year, tenure, size, estimate) |> 
  mutate(NAME = str_remove_all(NAME, ", Virginia")) |> 
  mutate(size = case_when(
    size == "1-person household" ~ "1-person",
    size == "2-person household" ~ "2-person",
    size == "3-person household" ~ "3-person",
    TRUE ~ "4 or more persons"
  )) 

# Preview the cleaned data
head(b25010_data)


# Print the current working directory
current_dir <- getwd()
print(current_dir)

# Optionally set a new working directory
# setwd("path/to/your/directory")

# Save the cleaned data as an RDS file
write_rds(b25010_data, "/Users/mted/Desktop/HDA/FAAR/faar/data/b25010_decade_region.rds")
