## Setup ---------------------------------------------

# Load packages

library(tidyverse)
library(glue)
library(fredr)
library(zoo)

# Set parameters

years <- 2015:2023
qtrs <- as.character(1:4)
cnty <- c("51630", "51033", "51099", "51177", "51179", "51137") # FAAR FIPS codes

names <- data.frame(
  area_fips = as.integer(cnty),
  name = c("Fredericksburg", "Caroline", "King George", "Spotsylvania", "Stafford", "Orange")
)



## Create QCEW data pull function --------------------

# qcewGetAreaData : This function takes a year, quarter, and area argument and
# returns an array containing the associated area data. use 'a' for annual
# averages. 
# For all area codes and titles see:
# http://data.bls.gov/cew/doc/titles/area/area_titles.htm

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


## Create function to get data by year or qtr --------

get_qcew_data <- function(years, counties, period = c("yr", "qtr")) {
  
  # Match and validate period argument
  period <- match.arg(period)
  
  if(period == "yr") {
    # Get annual data
    return(map_dfr(counties, function(cnty){
      map_dfr(years, function(yr){
        qcewGetAreaData(yr, "a", cnty) |> 
          filter(own_code == 0) |> 
          filter(industry_code == 10) |> 
          select(year, area_fips, annual_avg_emplvl, annual_avg_wkly_wage, avg_annual_pay)
      })
    }))
    
  } else {
    # Get quarterly data
    return(map_dfr(counties, function(cnty){
      map_dfr(years, function(yr){
        map_dfr(1:4, function(qtr){
          Sys.sleep(0.5)  # Add delay to be respectful to the API
          tryCatch({
            qcewGetAreaData(yr, qtr, cnty) |> 
              filter(own_code == 0) |> 
              filter(industry_code == 10) |> 
              select(year, qtr, area_fips, month1_emplvl, month2_emplvl, month3_emplvl, 
                     avg_wkly_wage)
          }, error = function(e) {
            warning(sprintf("Failed to fetch Q%d %d data for area %s: %s", 
                            qtr, yr, cnty, e$message))
            return(NULL)
          })
        })
      })
    }))
  }
}


## Pull QCEW data ------------------------------------

# Annual data 2015-2023
qcew_a <- get_qcew_data(years, cnty, period = "yr")

# Quarterly data 2015Q1-2024Q1
qcew_q <- bind_rows(
  qcew_q,
  #get_qcew_data(years, cnty, period = "qtr"), # Full years
  get_qcew_data(2024, cnty, period = "qtr")   # 2024Q1 latest available
)


## CPI ---

# Get quarterly CPI
cpi_q <- fredr(
    series_id = "CPIAUCSL",
    observation_start = as.Date("2015-01-01"),
    observation_end = as.Date("2024-03-31"),
    frequency = "q",
    aggregation_method = "avg"
  ) |> 
  mutate(
    year = year(date),
    qtr = quarter(date)
  ) |> 
  select(date, year, qtr, cpi = value)

# Get annual CPI
cpi_a <- fredr(
    series_id = "CPIAUCSL",
    observation_start = as.Date("2015-01-01"),
    observation_end = as.Date("2023-12-31"),
    frequency = "a",
    aggregation_method = "avg"
  ) |> 
  mutate(
    year = year(date)
  ) |> 
  select(date, year, cpi = value)

# Pull latest CPI as benchmark for adjustment
cpi_latest <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2024-01-01")
) |> 
  slice_max(date, n = 1) |> 
  pull(3)


## Prepare data for analysis -------------------------

# Annual data
faar_qcew_a <- qcew_a |> 
  left_join(cpi_a) |> # Join with CPI data
  mutate(adj_annual_pay = (cpi_latest/cpi)*avg_annual_pay) |> # Adjust pay for inflation
  left_join(names) |> # Join with locality names
  select(
    name, fips = area_fips, year,
    avg_emplvl = annual_avg_emplvl, avg_annual_pay, adj_annual_pay
  ) |> 
  mutate(period = "annual", .before = 1) |> # Add column to mark as annual data
  mutate(qtr = NA, .after = year) # Add blank qtr column for binding with quarterly data

# Quarterly data
faar_qcew_q <- qcew_q |> 
  left_join(cpi_q) |> # Join with CPI data
  mutate(
    avg_emplvl = round( # Average monthly employment levels and round
      (month1_emplvl+month2_emplvl+month3_emplvl)/3
    ), 
    avg_annual_pay = avg_wkly_wage*52, # Calculate annual pay from weekly wages
    adj_annual_pay = (cpi_latest/cpi)*avg_annual_pay # Adjust pay for inflation
  ) |>
  left_join(names) |> # Join with locality names
  select(
    name, fips = area_fips, year, qtr,
    avg_emplvl, avg_annual_pay, adj_annual_pay
    ) |> 
  mutate(period = "quarterly", .before = 1) # Add column to mark as quarterly data

# Bind annual and quarterly data into single dataset
faar_qcew <- bind_rows(
  faar_qcew_a,
  faar_qcew_q
)


## Save QCEW data ------------------------------------

write_rds(faar_qcew, "data/faar_qcew.rds")



######################################################
# ERIC CODE

qcew_plot <- read_rds("data/qcew_data.rds") |> 
  group_by(year) |> 
  summarise(emp = sum(emp)) |> 
  arrange(year) |> # Get in correct order
  mutate(
    chg = emp - first(emp), # Cumulative change
    pct_chg = chg / first(emp)
  )

qcew_plot |> 
  filter(year != 2015) |> 
  ggplot(aes(x = year, y = chg)) +
  geom_col(fill = "#445ca9") +
  scale_x_continuous(breaks = seq(2016, 2023, 1)) +
  scale_y_continuous(labels = label_comma()) +
  add_zero_line() +
  labs(
    title = "Cumulative job growth in Fredericksburg area",
    subtitle = "2016 through 2023",
    caption = "**Source:** Bureau of Labor Statistics, Quarterly Census of Employment and Wages"
  ) +
  theme_hda(base_size = 16)

####

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
  
