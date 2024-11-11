library(glue)
library(tidyverse)
library(fredr)
library(zoo)

years <- 2015:2023

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
  transform(adj_pay = ((311.8802/priceindex)*avg_annual_pay)) |>
write_rds("qcew_data_cpi.rds")

cnty_names <- data.frame(
  fips = as.integer(cnty),
  name = c("Fredericksburg", "Caroline", "King George", "Spotsylvania", "Stafford", "Orange")
)

qcew_data |> 
  select(
    fips = area_fips,
    year,
    emp = annual_avg_emplvl
  ) |> 
  left_join(cnty_names) |> 
  write_rds("data/qcew_data.rds")



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
  
