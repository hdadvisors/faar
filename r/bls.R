library(glue)
library(tidyverse)
library(fredr)
library(zoo)

years <- 2015:2022

cnty <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


qcew_data <- map_dfr(cnty, function(cnty){
  yearly_data <- map_dfr(years, function(yr){
    qcew_pull <- qcewGetAreaData(yr, "a", cnty) |> 
      filter(own_code == 0) |> 
      filter(industry_code == 10) |> 
      select(area_fips, annual_avg_emplvl, annual_avg_wkly_wage,avg_annual_pay) |> 
      mutate(year = yr)
  })
})

cpi <- fredr(
  series_id = "CPIAUCSL" 
) |> 
  mutate(quarter = as.yearqtr(date)) |> 
  mutate(year = year(quarter)) |> 
  filter(year >= 2015) |> 
  group_by(year) |> 
  summarise(cpi = mean(value)) 

cpi <- cpi |> 
  rename(year = year,
         priceindex = cpi) |> 
  transform(year = as.numeric(year))

qcew_data_cpi <- qcew_data |> 
  left_join(cpi, by = 'year') |> 
  transform(adj_pay = ((311.8802/priceindex)*avg_annual_pay))

write_rds(qcew_data_cpi, "data/qcew_data.rds)


oews_2023 <- read_xlsx("data/raw/oews_va_2023.xlsx",
                       sheet = "dc",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake")  |> 
  filter(o_group == "major") |> 
  group_by(area_title) |> 
  slice_max(tot_emp, n = 5) |> 
  select(2, 9, 10, tot_emp_2023 = 12, a_median_2023 = 28)

oews_2020 <- read_xlsx("data/raw/oews_va_2020.xlsx",
                       sheet = "m2020",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake")  |> 
  filter(o_group == "major") |> 
  select(2, 9, 10, tot_emp_2020 = 12, a_median_2020 = 28)
  

oews_occ <- oews_2023 |> 
  left_join(oews_2020, by = c("occ_code", "occ_title", "area_title")) |> 
  mutate(pct_change = (a_median_2023 - a_median_2020) / a_median_2020)


write_rds(oews_occ, "data/oews_data.rds")
  
