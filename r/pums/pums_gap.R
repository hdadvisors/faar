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

faar_ami <- read_rds("data/pums/faar_ami_pums.rds")

rate = 0.06       
dp = 0.13         
tax_ins = 0.015

faar_ami_join <- faar_ami |> 
  mutate(hh_size = as.numeric(str_sub(hh_size, end = 1))) |>
  mutate(
    aff_rent = (income/12)*0.3,
    aff_own = 
      (income/12*0.28) /
      ((rate/12 * (1+rate/12)^360) /
      ((1+rate/12)^360 - 1) /
      (1-(tax_ins/12)) /
      (1-dp)),
    ami_faar =
      case_match(
        level,
        "ami30" ~ "Below 30% AMI",
        "ami50" ~ "30-50% AMI",
        "ami80" ~ "50-80% AMI",
        "ami100" ~ "80-100% AMI",
        "ami120" ~ "100-120% AMI"
      )
  ) |> 
  arrange(hh_size, ami_faar) |> 
  mutate(
    ami_faar = factor(
      ami_faar, levels = c("Below 30% AMI", "30-50% AMI", "50-80% AMI", "80-100% AMI", "100-120% AMI")
    )
  ) |> 
  #select(hh_size, ami_faar, income, aff_rent, aff_own)
  select(hh_size, ami_faar, aff_rent)



pums_gap_rent <- pums_faar_hh |> 
  filter(tenure == "Renter") |> 
  left_join(
    faar_ami_join,
    by = "hh_size"
  )





pums_gap_supply <- pums_faar_hh |> 
  left_join(faar_ami_join, by = join_by(hh_size, ami_faar)) |> 
  mutate(
    cost = case_when(
      tenure == "Homeowner" ~ cost_own,
      tenure == "Renter" ~ cost_rent
    )
  )
    
    unit_afford = factor(
      case_when(
        
        HINCP < 1 ~ ami_levels[1],
        HINCP <= ami30 ~ ami_levels[2],
        HINCP <= ami50 ~ ami_levels[3],
        HINCP <= ami80 ~ ami_levels[4],
        HINCP <= ami100 ~ ami_levels[5],
        HINCP <= ami120 ~ ami_levels[6],
        TRUE ~ ami_levels[7]
      ),
      levels = ami_levels,
      ordered = TRUE
    )
  )
  
  mutate(
    
    supply_rent = case_when(
      tenure_detail == "Renter (no rent)" & cost_rent
    )
  )

pums_gap_cb <- pums_faar_hh |> 
  filter(
    hh_income > 0,
    cb_label != "Not cost-burdened"
  ) |> 
  mutate(tenure = paste0(tenure, "s")) |> 
  to_survey(type = "housing", design = "rep_weights") |>
  group_by(core_workforce, ami_faar) |> 
  summarise(
    n = survey_total(vartype = "cv")
  ) |> 
  ungroup() |> 
  mutate(
    pct = n/sum(n),
    ymax = cumsum(pct),
    ymin = c(0, head(ymax, n = -1)),
    pct_label = (ymax + ymin)/2
  ) |>
  mutate(
    core_workforce = case_match(
      core_workforce,
      core_workforce = T ~ "Core workforce",
      .default = "Not core workforce"
    )
  )

pums_gap_cb |> 
  ggplot(aes(x = n, y = ami_faar, fill = fct_rev(core_workforce))) +
  geom_col() +
  geom_text(
    data = . %>% filter(n > 500),
    aes(label = label_comma()(n)),
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_hda(-1, guide = guide_legend(reverse = T)) +
  labs(
    title = "Households who need affordable homes",
    subtitle = "Number of households paying more than 30% of income on housing",
    caption = pums_cap
  ) +
  theme_hda(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    axis.text.x = element_blank()
  )

  
            
            
            