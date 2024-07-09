



# 8. Survey

# Convert to survey object
pums_svy_h <- pums_ami |> 
  to_survey(type = "housing", design = "rep_weights")

pums_ami <- pums_svy_h |> 
  group_by(ami_category) |> 
  summarise(
    estimate = survey_total()
  )





fxburg_units <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami_unit, tenure) |> 
  summarise(estimate = sum(WGTP)) |> 
  select(ami = ami_unit, tenure, estimate)

fxburg_hh <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami, tenure) |> 
  summarise(estimate = sum(WGTP)) 

pums_race <- pums_join |> 
  filter(SPORDER == 1) |> 
  group_by(ami, race) |> 
  summarise(estimate = sum(WGTP)) |> 
  ungroup() |> 
  group_by(ami) |> 
  mutate(pct = estimate/(sum(estimate)))

fxburg_supply <- fxburg_units |> 
  full_join(fxburg_hh, by = c("ami", "tenure")) |> 
  select(ami, tenure, hh = estimate.y, units = estimate.x) |> 
  pivot_longer(cols = 3:4,
               names_to = "value",
               values_to = "estimate") |> 
  mutate(value = case_when(
    value == "hh" ~ "Demand",
    value == "units" ~ "Supply"
  ))

ggplot(pums_race,
       aes(x = ami,
           y = pct,
           fill = race)) +
  geom_col(position = "stack") +
  coord_flip()


ami_order <- factor(fxburg_supply$ami, levels = c("30% AMI or less", "31 to 50% AMI",
                                                  "51 to 80% AMI", "81 to 100% AMI", "101 to 120% AMI",
                                                  "121% AMI or more"))


ggplot(fxburg_supply,
       aes(x = ami_order,
           y = estimate,
           fill = value)) +
  geom_col(position = "dodge") +
  facet_wrap(~tenure) +
  theme_hda(base_size = 10) +
  scale_fill_hda() +
  theme(legend.position = "right")

# Top Household Type

top_hht <- pums_join |> 
  group_by(ami, tenure, HHT2_label) |> 
  summarise(count = sum(WGTP)) |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup() |> 
  select(ami, tenure, HHT2_label)


# TOP INDUSTRIES BY AMI

top_occ <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> # Nesting based on top household type
  mutate(OCCP_label = as.character(OCCP_label)) |> 
  filter(OCCP_label != "NA") |> 
  group_by(ami, tenure, OCCP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()

top_jobs <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> 
  filter(NAICSP != "N") |> 
  group_by(ami, tenure, NAICSP_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()

top_bld <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> 
  group_by(ami, tenure, BLD_label) |> 
  summarise(count = sum(PWGTP), .groups = 'drop') |> 
  group_by(ami, tenure) |> 
  arrange(ami, tenure, desc(count)) %>%
  slice_head(n = 1) %>%
  ungroup()


# Summary Stats
# Average household age
# Median Household Income
# Average number of workers
# Average number of children

stat <- pums_join |> 
  right_join(top_hht, by = c("ami", "tenure", "HHT2_label")) |> # Nesting based on top household type
  mutate(WIF = as.numeric(WIF)) |> 
  mutate(NOC = as.numeric(NOC)) |>
  mutate(BDSP = as.numeric(BDSP)) |> 
  group_by(tenure, ami) |> 
  summarise(
    mean_hhage = weighted.mean(HHLDRAGEP, WGTP),
    med_inc = weighted.median(HINCP, WGTP, na.rm = TRUE),
    mean_wif = weighted.mean(WIF, WGTP, na.rm = TRUE),
    mean_noc = round(weighted.mean(NOC, WGTP, na.rm = TRUE), 1),
    mean_bed = weighted.mean(BDSP, WGTP, na.rm = TRUE))

# Top Household Type by AMI



# Create a data frame that breaks out
# 

new_order <- c("30% AMI or less", "31 to 50% AMI", "51 to 80% AMI", "81 to 100% AMI",
               "101 to 120% AMI", "121% AMI or more")

profile <- top_jobs |> 
  left_join(stat, by = c("ami", "tenure")) |> 
  left_join(top_hht, by = c("ami", "tenure")) |> 
  left_join(top_occ, by = c("ami", "tenure")) |> 
  left_join(top_bld, by = c("ami", "tenure")) |> 
  mutate(Industry = str_replace(NAICSP_label, ".*-", "")) |> 
  mutate(Industry = str_replace_all(Industry, "\\s*\\([^\\)]+\\)", "")) |> 
  mutate(Occupation = str_replace(OCCP_label, ".*-", "")) 

|> 
  mutate(hht = case_when(
    HHT2_label == "Married couple household with children of the householder less than 18" ~ "Married couple with children",
    HHT2_label == "Married couple household, no children of the householder less than 18" ~ "Married couple",
    HHT2_label == "Female householder, no spouse/partner present, living alone" ~ "Single female",
    HHT2_label == "Male householder, no spouse/partner present, living alone" ~ "Single male"
  )) |> 
  mutate(structure = case_when(
    BLD_label == "One-family house detached" ~ "Single-family detached home",
    BLD_label == "10-19 Apartments" ~ "Small-sized multifamily (10-19 units)",
    BLD_label == "20-49 Apartments" ~ "Medium-sized multifamily (20 to 49 units)" 
  )) |> 
  select(ami, tenure, Industry, hht, structure, mean_hhage, mean_wif, med_inc, mean_noc)


profile$ami <- factor(profile$ami, levels = new_order) 


profile_table <- profile |> 
  arrange(ami) 




