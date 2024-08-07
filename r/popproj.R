##popluation projection 

# Load required libraries
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")
install.packages("readr")
install.packages("showtext")

library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(readr)
library(showtext)

# Add the fonts
font_add_google("Roboto Slab", "roboto_slab")
font_add_google("Lato", "lato")
showtext_auto()


# Prepare the data
va_processed <- wecoop %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(population_change = (`2050` - `2020`) / `2020` * 100) %>%
  select(fips, name, population_change)

# Get Virginia county map data
va_map <- map_data("county", region = "virginia")

# Create a lookup table for FIPS codes to county names
fips_lookup <- va_processed %>%
  select(fips, name) %>%
  mutate(county = tolower(sub(" County", "", name)))

# Merge the map data with our processed dataset, keeping only the counties in fips_lookup
va_map_data <- va_map %>%
  mutate(subregion = tolower(subregion)) %>%
  inner_join(fips_lookup, by = c("subregion" = "county")) %>%
  left_join(va_processed, by = "fips")

# Ensure the name variable is included in the merged dataset
va_map_data <- va_map_data %>%
  select(long, lat, group, subregion, name.x, population_change)

# Calculate the centroids of each county for placing labels
centroids <- va_map_data %>%
  group_by(subregion) %>%
  summarize(long = mean(long), lat = mean(lat), name = first(name.x), .groups = 'drop')

# Create the visualization
ggplot(va_map_data, aes(x = long, y = lat, fill = population_change)) +
  geom_polygon(aes(group = group), color = "white", size = 0.2) +
  geom_text(data = centroids, aes(label = name), size = 3, family = "lato", color = "black") +
  scale_fill_gradient2(
    low = "#445ca9", 
    mid = "#8baeaa", 
    high = "#e9ab3f", 
    midpoint = median(va_processed$population_change, na.rm = TRUE),
    name = "Population\nChange (%)\n2020-2050"
  ) +
  coord_map() +
  theme_minimal() +
  theme(
    text = element_text(family = "lato"),
    plot.title = element_text(family = "roboto_slab", size = 16, face = "bold"),
    plot.subtitle = element_text(family = "roboto_slab", size = 12),
    legend.title = element_text(family = "roboto_slab", size = 10),
    legend.text = element_text(family = "lato", size = 9),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Virginia County Population Projection",
       subtitle = "Projected population change from 2020 to 2050 for selected counties")

# Save the plot
ggsave("virginia_population_projection.png", width = 10, height = 6, dpi = 300)

            