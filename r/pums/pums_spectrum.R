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


## Household typologies -----------------------------------

pums_hh_types <- pums_faar_hh |> 
  filter(hh_income > 0) |>
  mutate(
    minors = case_when(
      minors == 0 ~ "No",
      minors > 0 ~ "Yes"
    )
  ) |> 
  mutate(
    hh_earners = case_when(
      hh_earners == 0 ~ "None",
      hh_earners == 1 ~ "Single",
      hh_earners == 2 ~ "Double",
      hh_earners > 2 ~ "Multiple"
    )
  ) |> 
  mutate(
    hh_age = case_match(
      hh_age,
      "Elderly" ~ "Senior",
      .default = hh_age
    )
  ) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(interact(ami_faar, hh_type, hh_age, minors, hh_earners)) |>
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  ungroup() |> 
  add_reliability() |> 
  group_by(ami_faar) |> 
  mutate(pct = n/sum(n)) |> 
  ungroup() |> 
  select(1:6, 9, 8)

write_rds(pums_hh_types, "data/pums/pums_hh_types.rds")


## Summary AMI table --------------------------------------

pums_tbl_tot <- pums_faar |>
  filter(hh_income > 0) |> 
  group_by(ami_faar) |> 
  summarise(
    hh = sum(WGTP[SPORDER == 1]),
    p = sum(PWGTP),
  )

pums_tbl_avg <- pums_faar_hh |>
  filter(hh_income > 0) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar) |> 
  summarise(
    hh_size = survey_mean(hh_size),
    hh_income = survey_median(hh_income)
  ) |> 
  select(-3, -5)

pums_tbl <- pums_tbl_tot |> 
  left_join(pums_tbl_avg) |> 
  pivot_longer(
    cols = 2:5,
    names_to = "var"
  ) |> 
  mutate(value = as.numeric(value)) |> 
  mutate(
    value = case_when(
      var %in% c("hh", "p") ~ formatC(value, format = "d", big.mark = ","),
      var == "hh_size" ~ formatC(value, format = "f", digits = 2, big.mark = ","),
      var == "hh_income" ~ paste0("$", formatC(value, format = "d", big.mark = ","))
    )
  ) |> 
  mutate(
    var = case_match(
      var,
      "hh" ~ "Households",
      "p" ~ "Persons",
      "hh_size" ~ "Average household size",
      "hh_income" ~ "Median household income"
    )
  )

write_rds(pums_tbl, "data/spectrum_reg/tbl_ami.rds")

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


## fig-ami-employ -----------------------------------------

fig_ami_employ <- pums_faar |> 
  filter(
    hh_income > 1,
    age %in% c(25:54),
    disability == "Without a disability"
  ) |> 
  to_survey(type = "person", design = "rep_weights") |>
  group_by(ami_faar, emp_status) |> 
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

write_rds(fig_ami_employ, "data/spectrum_reg/fig_ami_employ.rds")


pums_faar |> 
  filter(
    ami_faar %in% c("Below 30% AMI", "30-50% AMI"),
    age %in% c(25:54),
    disability == "Without a disability",
    emp_status == "Not in labor force"
  ) |> 
  to_survey(type = "person", design = "rep_weights") |>
  group_by(race) |> 
  summarise(
    #q = survey_quantile(age, c(0.25, 0.66, 0.75))
    n = survey_total(vartype = "cv")
  ) |> 
  mutate(
    pct = n/sum(n)
  )


## fig-ami-earn -----------------------------------------

fig_ami_earn <- pums_faar_hh |> 
  filter(hh_income > 1) |> 
  mutate(
    hh_earners = fct_case_when(
      hh_earners == 0 ~ "None",
      hh_earners == 1 ~ "Single",
      hh_earners == 2 ~ "Double",
      hh_earners > 2 ~ "Multiple"
    )
  ) |>
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, hh_earners) |> 
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
  
write_rds(fig_ami_earn, "data/spectrum_reg/fig_ami_earn.rds")

  
## fig-ami-str -------------------

fig_ami_str <- pums_faar_hh |> 
  mutate(tenure = paste0(tenure, "s")) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, tenure, str_type) |> 
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
  
write_rds(fig_ami_str, "data/spectrum_reg/fig_ami_str.rds")

## fig-ami-soi -------------------

fig_ami_soi <- pums_faar |> 
  mutate(
    across(20:27, ~ replace_na(.x, 0)),
    tenure = paste0(tenure, "s")
  ) |> 
  to_survey(type = "person", design = "rep_weights") |>
  group_by(ami_faar, tenure) |> 
  summarise(
    inc_wages = survey_total(inc_wages + inc_selfemp),
    inc_ret = survey_total(inc_ret),
    inc_ss = survey_total(inc_ss),
    inc_public = survey_total(inc_ssi + inc_public),
  ) |> 
  select(1, 2, 3, 5, 7, 9) |> 
  pivot_longer(
    cols = 3:6,
    names_to = "inc",
    values_to = "amt"
  ) |> 
  group_by(ami_faar, tenure) |>
  mutate(
    pct = amt/sum(amt)
  ) |>
  mutate(
    inc = case_match(
      inc,
      "inc_wages" ~ "Wages",
      "inc_ss" ~ "Social Security",
      "inc_ret" ~ "Retirement",
      "inc_public" ~ "Public assistance"
    )
  ) |> 
  ungroup()

write_rds(fig_ami_soi, "data/spectrum_reg/fig_ami_soi.rds")


## fig-30-costs -------------------------------------------

costs_wm <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  mutate(
    tenure = paste0(tenure, "s"),
    costs = case_when(
      cost_own == -1 ~ cost_rent,
      .default = cost_own
    )
  ) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, tenure) |> 
  summarise(
    wm = survey_median(costs, vartype = "cv"),
    q = survey_quantile(costs, c(0.1, 0.9))
  ) |> 
  ungroup()

write_rds(costs_wm, "data/spectrum_reg/costs_wm.rds")

fig_ami_costs <- pums_faar_hh |> 
  filter(hh_income > 0) |> 
  mutate(
    tenure = paste0(tenure, "s"),
    costs = case_when(
      cost_own == -1 ~ cost_rent,
      .default = cost_own
    )
  ) |> 
  select(SERIALNO, WGTP, ami_faar, tenure, costs)

write_rds(fig_ami_costs, "data/spectrum_reg/fig_ami_costs.rds")


## fig-30-cb ----------------------------------------------

fig_ami_cb <- pums_faar_hh |> 
  mutate(tenure = paste0(tenure, "s")) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(ami_faar, tenure, cb_label) |> 
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

write_rds(fig_ami_cb, "data/spectrum_reg/fig_ami_cb.rds")


###########################################################



## Household typologies

pums_faar_hh |> 
  mutate(
    hh_age = case_match(
      hh_age,
      "Elderly" ~ "Senior",
      .default = hh_age
    )
  ) |>
  filter(
    ami_faar == "Below 30% AMI",
    hh_type == "Individual",
    hh_age == "Senior",
    minors == 0,
    hh_earners == 0
  ) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(hh_disability) |>
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  ungroup() |> 
  add_reliability() |> 
  mutate(pct = n/sum(n))
  
# Below 30%
# Individual > Senior > No children > No earners
# Individual > Middle-age > No children > No earners (52% disabled)
# Couple > Middle-age > Children > Single earner
# Single parent > Middle-age > Children > Single earner

# Couple > Middle-age > Children > Double earners
# Couple > Senior > No children > No earners

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



