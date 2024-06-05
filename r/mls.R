library(tidyverse)
library(janitor)
library(tidygeocoder)



# Download BrightMLS data for the study area. Pull closed home sales from 1.1.2020 to 5.1.2024

fburg <- read_csv("data/raw/fburg_mls.csv")
kg <- read_csv("data/raw/kg_mls.csv")
orange <- read_csv("data/raw/orange_mls.csv")
spots1 <- read_csv("data/raw/spots1_mls.csv")
spots2 <- read_csv("data/raw/spots2_mls.csv")
spots3 <- read_csv("data/raw/spots3_mls.csv")
spots4 <- read_csv("data/raw/spots4_mls.csv")
spots5 <- read_csv("data/raw/spots5_mls.csv")

staff1 <- read_csv("data/raw/staff1_mls.csv")
staff2 <- read_csv("data/raw/staff2_mls.csv")
staff3 <- read_csv("data/raw/staff3_mls.csv")
staff4 <- read_csv("data/raw/staff4_mls.csv")
staff5 <- read_csv("data/raw/staff5_mls.csv")


faar_mls <- rbind(fburg, kg, orange, spots1, spots2, spots3, spots4, spots5,
                  staff1, staff2, staff3, staff4, staff5) |>
  clean_names() |>
  mutate(zip_code = as.character(zip_code))


max_rows <- 9999
num_chunks <- ceiling(nrow(faar_mls)/max_rows)
chunks <- rep(1:num_chunks, each = max_rows)[1:nrow(faar_mls)]

list_of_dfs <- split(faar_mls, chunks)

faar_mls1 <- data.frame(list_of_dfs[1])
faar_mls2 <- data.frame(list_of_dfs[2])
faar_mls3 <- data.frame(list_of_dfs[3])



colnames(faar_mls1) <- sub("^X1\\.", "", colnames(faar_mls1))
colnames(faar_mls2) <- sub("^X2\\.", "", colnames(faar_mls2))
colnames(faar_mls3) <- sub("^X3\\.", "", colnames(faar_mls3))


parts <- c(faar_mls1, faar_mls2, faar_mls3)


faar_mls_address <- faar_mls1 |>
  mutate(full_address = paste(address, county, zip_code, sep = ",")) |>
  geocode(address = full_address,
          method = 'geocodio',
          lat = latitude,
          long = longitude,
          full_results = TRUE)

faar_mls_address1 <- faar_mls2 |>
  mutate(full_address = paste(address, county, zip_code, sep = ",")) |>
  geocode(address = full_address,
          method = 'geocodio',
          lat = latitude,
          long = longitude,
          full_results = TRUE)

faar_mls_address2 <- faar_mls3 |>
  mutate(full_address = paste(address, county, zip_code, sep = ",")) |>
  geocode(address = full_address,
          method = 'geocodio',
          lat = latitude,
          long = longitude,
          full_results = TRUE)

faar_geocode <- rbind(faar_mls_address, faar_mls_address1, faar_mls_address2)

faar_geocode <- read_rds("data/faar_mls.rds")

cpi_sales <- cpi_rent <- fredr(
  series_id = "CUUR0000SA0L2" 
) |> 
  mutate(quarter = as.yearqtr(date)) |> 
  mutate(year = year(quarter)) |> 
  filter(year >= 2019) |> 
  group_by(quarter) |> 
  summarise(cpi = mean(value))
  
faar_geocode_cpi <- faar_geocode |> 
  mutate(close_price = as.numeric(gsub("[\\$,]", "", close_price))) |> 
  mutate(list_price = as.numeric(gsub("[\\$,]", "", list_price))) |>  
  mutate(sale_date = as.Date(close_date, format = "%m/%d/%y")) |> 
  mutate(quarter = as.yearqtr(sale_date)) |> 
  mutate(year = year(quarter)) |> 
  filter(year >= 2019) |> 
  left_join(cpi_sales, by = "quarter") |> 
  mutate(adj_sales = (284.2240/cpi) * close_price)


write_rds(faar_geocode_cpi, "data/faar_mls_cpi.rds")

# write_rds(faar_geocode, "data/faar_mls.rds")
  
