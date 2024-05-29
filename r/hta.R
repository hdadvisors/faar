library(tidyverse)
library(janitor)
library(tigris)
library(mapview)
library(sf)
library(leaflet)


local_fips <- c(51177, 51179, 51630, 
                51099, 51033, 51137)

ht <- read_csv("data/raw/hta_index_blocks.csv") |> 
  clean_names() |> 
  mutate(fips = as.numeric(substr(gsub('"', '', blkgrp), 1, 5))) |> 
  mutate(GEOID = gsub('"', '', blkgrp)) |> 
  filter(fips %in% local_fips) |> 
  select(fips, GEOID, ht_ami, ht_80ami, ht_nmi, 
         h_ami, h_80ami, h_nmi, 
         t_ami, t_80ami, t_nmi)
  
  
bg <- block_groups(state = "VA", year = 2019) |> 
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))|> 
  filter(fips %in% local_fips) |> 
  st_transform(4326) |> 
  left_join(ht, by = "GEOID")

plot(tracts$geometry)

bins <- c(0, 25, 50, 75, 100)
pal <- colorBin("YlOrRd", domain = bg$ht_ami, bins = bins)

leaflet(bg) |> 
  addTiles() |> 
  addPolygons(fillColor = ~pal(ht_ami), # Set polygon fill color.
              weight = 1, # Set polygon outline weight.
              color = "white", # Set polygon outline color.
              fillOpacity = 0.8,
              popup = bg$ht_ami) # Set polygon fill color transparency.
