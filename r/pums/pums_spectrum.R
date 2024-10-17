## Use PUMS Data for Housing Spectrum Analysis

## Setup --------------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(scales)
library(hdatools)
library(ggridges)

# Load clean, labels PUMS data with variables and weights
pums_faar <- read_rds("data/pums/pums_faar.rds")

# Household records only
pums_faar_hh <- pums_faar |> filter(SPORDER == 1)

## fig-ami ------------------------------------------------

fig_ami <- pums_faar_hh |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  ungroup() |> 
  mutate(
    pct = n/sum(n),
    color = case_when(
      ami_faar == "Zero or negative income" ~ "1",
      .default = "0"
    )
  )

write_rds(fig_ami, "data/spectrum_reg/fig_ami.rds")


## fig-ami-dist -------------------------------------------

ami_wm <- pums_faar_hh |> 
  to_survey(type = "housing", design = "rep_weights") |>
  filter(hh_income > 0) |> 
  group_by(ami_faar) |> 
  summarise(
    wm = survey_median(hh_income, vartype = "cv"),
    q = survey_quantile(hh_income, c(0.1, 0.9))
  ) |> 
  ungroup() |> 
  mutate(
    range = paste(dollar(q_q10), "-", dollar(q_q90))
  )

write_rds(ami_wm, "data/spectrum_reg/ami_wm.rds")

fig_ami_dist <- pums_faar_hh |> 
  select(SERIALNO, WGTP, ami_faar, hh_income) |> 
  filter(hh_income > 0)

write_rds(fig_ami_dist, "data/spectrum_reg/fig_ami_dist.rds")


## fig-ami-tenure -----------------------------------------

fig_tenure_subtitle <- paste(
  "Percent who are",
  "<span style='color: #445ca9'><b>homeowners</b> (with mortgage)</span>,",
  "<span style='color: #8baeaa'><b>homeowners</b> (no mortgage)</span>, or",
  "<span style='color: #e9ab3f'><b>renters</b></span>"
)

fig_ami_tenure <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  mutate(tenure_detail = str_remove_all(tenure_detail, " \\(no rent\\)")) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, tenure_detail) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n)
  ) |>
  ungroup()

write_rds(fig_ami_tenure, "data/spectrum_reg/fig_ami_tenure.rds")


## fig-ami-hh-type ----------------------------------------

fig_ami_hh_type <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, hh_type) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1)),
    pct_label = (ymax + ymin)/2
  ) |>
  ungroup()

write_rds(fig_ami_hh_type, "data/spectrum_reg/fig_ami_hh_type.rds")


## fig-ami-hh-age -----------------------------------------

fig_ami_hh_age <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, hh_age) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1)),
    pct_label = (ymax + ymin)/2
  ) |>
  mutate(
    hh_age = fct_case_when(
      hh_age == "Young" ~ "Young (<35)",
      hh_age == "Middle-age" ~ "Middle-age (35-64)",
      hh_age == "Senior" ~ "Senior (65-79)",
      hh_age == "Elderly" ~ "Elderly (80+)"
    )
  ) |> 
  ungroup()

write_rds(fig_ami_hh_age, "data/spectrum_reg/fig_ami_hh_age.rds")


## fig-ami-hh-child ---------------------------------------

fig_ami_hh_child <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  mutate(
    minors = fct_case_when(
      minors == 0 ~ "None",
      minors == 1 ~ "1",
      minors > 1 ~ "Multiple"
    )
  ) |>
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, minors) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1)),
    pct_label = (ymax + ymin)/2
  ) |>
  ungroup()

write_rds(fig_ami_hh_child, "data/spectrum_reg/fig_ami_hh_child.rds")


## Household typologies

faar_hh_types <- pums_faar |> 
  mutate(
    children = case_when(
      children == 0 ~ "No",
      children > 0 ~ "Yes"
    )
  ) |> 
  #mutate(
  #  hh_earners = case_when(
  #    hh_earners == 0 ~ "No",
  #    hh_earners > 0 ~ "Yes"
  #  )
  #) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(interact(hh_type, hh_age, children, hh_earners)) |>
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  ungroup() |> 
  mutate(pct = n/sum(n)) |> 
  add_reliability()

# 1. Working family (middle-age): Couple > Middle-age > Children > Working
# 2. SINKs/DINKs: Couple > Middle-age > No children > Working
# 3. Working family (young): Couple > Young > Children > Working
# 4. Working single-parent: Single parent > Middle-age > Working
# 5. Familial providers: Relatives > Middle-age > No children > Working


## Regional housing spectrum ------------------------------


pums_faar |> 
  filter(
    hh_type == "Relatives",
    relationship %in% c("Reference person", "Child", "Grandchild")
    ) |> 
  to_survey(type = "person", design = "rep_weights") |> 
  mutate(age_group = cut(age, breaks = seq(0, 100, by = 2))) |>
  group_by(relationship, age_group) |> 
  summarise(
    count = survey_total(vartype = "ci")
  ) |> 
  ggplot(aes(x = age_group, y = count, fill = relationship)) +
  geom_col(position = "dodge") +
  facet_wrap(~relationship)
  


## 3. Stats for all households ----------------------------

# AMI by tenure
pums_faar_fct |> 
  filter(SPORDER == 1, cost_hsg_pct > 0 & cost_hsg_pct < 1) |> 
  mutate(cb_bin = cut(cost_hsg_pct, breaks = 50)) |> 
  #select(cost_hsg_pct, cb_bin)
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(tenure, cb_bin) |> 
  summarise(
    n = survey_prop(vartype = "cv")
  ) |> 
  mutate(bin_midpoint = as.numeric(str_extract(cb_bin, "(?<=,).*(?=\\])"))) |> 
  #add_reliability()
  ggplot(aes(x = bin_midpoint, y = n, fill = tenure)) +
    geom_col(position = "dodge")



## 4. Stats for households with at least 


