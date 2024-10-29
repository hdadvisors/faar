
source("_common.R")

library(tidycensus)
library(srvyr)

pums_cap <- "**Source:** HDAdvisors calculations of 2018-2022 ACS 5-year data."

rate = 0.06       
dp = 0.13         
tax_ins = 0.015

##

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

##

# Load clean, labels PUMS data with variables and weights
pums_faar <- read_rds("data/pums/pums_faar.rds")

# Household records only
pums_faar_hh <- pums_faar |> filter(SPORDER == 1)

pums_supply_strtype <- pums_faar_hh |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(tenure, str_yrblt, str_type) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n),
    str_yrblt = str_replace(str_yrblt, "-", " to ")
  ) |>
  ungroup()

pums_supply_strtype |> 
  filter(tenure == "Homeowner") |> 
  mutate(
    str_type = fct_lump_n(str_type, 3, n),
    str_yrblt = str_replace(str_yrblt, "^(.{7})", "\\1\n")
  ) |> 
  ggplot(aes(x = str_yrblt, y = pct, fill = str_type)) +
  geom_col() +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_hda() +
  add_zero_line() +
  labs(
    title = "Owner-occupied homes by age and type",
    subtitle = "Percent of total",
    caption = pums_cap
  ) +
  theme_hda(
    base_size = 16,
    legend.position = "top"
  )

pums_supply_strtype |> 
  filter(tenure == "Renter") |> 
  mutate(
    #str_type = fct_lump_n(str_type, 5, n),
    str_yrblt = str_replace(str_yrblt, "^(.{7})", "\\1\n")
  ) |> 
  ggplot(aes(x = str_yrblt, y = pct, fill = str_type)) +
  geom_col() +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_hda() +
  add_zero_line() +
  labs(
    title = "Renter-occupied homes by age and type",
    subtitle = "Percent of total",
    caption = pums_cap
  ) +
  theme_hda(
    base_size = 16,
    legend.position = "top",
    legend.justification = "left"
  )

pums_supply_br <- pums_faar_hh |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(tenure, str_type, bedrooms) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n)
    ) |>
  ungroup()

pums_supply_br |> 
  filter(tenure == "Renter") |> 
  mutate(
    bedrooms = fct_case_when(
      bedrooms == 0 ~ "Studio",
      bedrooms == 1 ~ "1",
      bedrooms == 2 ~ "2",
      bedrooms == 3 ~ "3",
      bedrooms > 3 ~ "4+"
    )
  ) |> 
  ggplot(aes(x = pct, y = str_type, fill = fct_rev(bedrooms))) +
  geom_col() +
  scale_x_continuous(labels = label_percent()) +
  scale_fill_hda(-1, guide = guide_legend(reverse = TRUE)) +
  add_zero_line() +
  labs(
    title = "Renter-occupied homes by type and number of bedrooms",
    subtitle = "Percent of total",
    caption = pums_cap
  ) +
  theme_hda(
    flip_gridlines = TRUE,
    base_size = 16,
    legend.position = "top",
    legend.justification = "center"
  )

##

locality_wages <- read_csv("data/faar_locality_wages.csv") |> 
  filter(
    job_actual != "Fire Protection",
    !(locality == "Orange" & job_actual == "Social Assistance")
  )

local_wages_cty <- locality_wages |> 
  summarize(
    salary = mean(salary), .by = locality
  ) |> 
  mutate(
    aff_rent = salary/12*0.3,
    aff_own = 
      (salary/12*0.28) /
      ((rate/12 * (1+rate/12)^360) /
         ((1+rate/12)^360 - 1) /
         (1-(tax_ins/12)) /
         (1-dp))
  )

local_wages_cty |> 
  ggplot(aes(x = aff_rent, y = fct_reorder(locality, aff_rent), label = label_dollar(accuracy = 1)(aff_rent))) +
  geom_col(fill = hda_pal[1]) +
  geom_text(color = "white", hjust = 0.5, nudge_x = -100) +
  geom_vline(xintercept = 1620) +
  geom_vline(xintercept = 1812) +
  geom_text(
    aes(x = 1620, y = "King George", label = "Average\n1-bed\nrent:\n$1,620"), vjust = 0.6, hjust = 0, nudge_x = 20
  ) +
  geom_text(
    aes(x = 1812, y = "King George", label = "Average\n2-bed\nrent:\n$1,812"), vjust = 0.6, hjust = 0, nudge_x = 20
  ) +
  scale_x_continuous(limits = c(0, 1950), expand = c(0.01, 0.01), labels = label_dollar()) +
  scale_y_discrete(expand = c(0.1, 0)) +
  add_zero_line("x") +
  labs(
    title = "Affordable rents for public sector salaries",
    subtitle = "Based on average wages for police, fire, and teacher jobs",
    caption = "**Sources:** HDAdvisors calculations of local government salary data; CoStar Group, Inc."
  ) +
  theme_hda(flip_gridlines = T)

local_wages_cty |> 
  ggplot(aes(x = aff_own, y = fct_reorder(locality, aff_own), label = label_dollar(accuracy = 1)(aff_own))) +
  geom_col(fill = hda_pal[1]) +
  geom_text(color = "white", hjust = 0.5, nudge_x = -15000) +
  #geom_vline(xintercept = 1620) +
  #geom_vline(xintercept = 1812) +
  #geom_text(
  #  aes(x = 1620, y = "King George", label = "Average\n1-bed\nrent:\n$1,620"), vjust = 0.6, hjust = 0, nudge_x = 20
  #) +
  #geom_text(
  #  aes(x = 1812, y = "King George", label = "Average\n2-bed\nrent:\n$1,812"), vjust = 0.6, hjust = 0, nudge_x = 20
  #) +
  scale_x_continuous(expand = c(0.01, 0.01), labels = label_dollar(accuracy = 1)) +
  scale_y_discrete(expand = c(0.1, 0)) +
  add_zero_line("x") +
  labs(
    title = "Affordable home purchase prices for public sector salaries",
    subtitle = "Based on average wages for police, fire, and teacher jobs",
    caption = "**Sources:** HDAdvisors calculations of local government salary data"
  ) +
  theme_hda(flip_gridlines = T)
  