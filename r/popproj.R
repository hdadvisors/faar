


library(tidyverse)
library(sf)
library(hdatools)
library(tigris)


# Load Virginia counties shapefile
va_counties <- counties("VA", cb = TRUE)

# Load weldon center data 
wecoop <- read_rds("data/wecoop.rds") %>%
  mutate(fips = as.character(fips))

#List of FAAR counties
faar_co <- c("Caroline County", "King George County", "Orange County", 
                          "Spotsylvania County", "Stafford County", "Fredericksburg city")

faar_fips <- c("51033", "51099", "51137", "51177", "51179", "51630")

wecoop_filtered <- wecoop %>%
  group_by(fips, name) %>%
  summarize(
    value_2020 = value[year == 2020],
    value_2050 = value[year == 2050],
    percent_change = (value_2050 - value_2020) / value_2020 * 100
  )


#filter for our counties

 va_counties <- va_counties %>%
  filter(NAMELSAD %in% FAAR_counties) %>%
  rename(fips = GEOID)

# Merge the wecoop_filtered data with the shapefile
va_counties_data <- va_counties %>%
  full_join(wecoop_filtered, by = c("fips" = "fips")) 


# Create the map
ggplot() +
  geom_sf(data = va_counties_data, fill = "white", color = "gray") +
  geom_sf(data = va_counties_data, aes(fill = percent_change), color = "black") +
  geom_sf_text(data = va_counties_data, aes(label = NAMELSAD),
               size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low = "#445ca9", mid = "#e9ab3f", high = "#e76f52", midpoint = median(va_counties_data$percent_change),
    name = "% Change",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(title = "Projected Population Change 2020-2050",
       subtitle = "Virginia Counties") +
  theme_hda() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.5, "cm")
  )

avg_hh_size = 2.854

pop <- read_csv("data/raw/pop-total.csv") |> 
  filter(as.character(GEOID) %in% faar_fips, year == 2022) |> 
  mutate(fips = as.character(GEOID)) |> 
  select(5, pop2022 = 4) 

faar_fips <- unique(wecoop$fips)

faar_proj <- wecoop |> 
  group_by(fips, name) |> 
  summarize(
    proj2030 = value[year == 2030],
    proj2040 = value[year == 2040],
    proj2050 = value[year == 2050]
  ) |> 
  left_join(pop) |> 
  mutate(
    d2030 = (proj2030 - pop2022)/avg_hh_size,
    d2040 = (proj2040 - proj2030)/avg_hh_size,
    d2050 = (proj2050 - proj2040)/avg_hh_size
  ) |> 
  ungroup()
  
faar_proj |> 
  summarise(
    hh2030 = sum(d2030),
    hh2040 = sum(d2030 + d2040),
    hh2050 = sum(d2030 + d2040 + d2050)
  )
