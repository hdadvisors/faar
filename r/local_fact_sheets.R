## Setup --------------------

source("_common.R")

library(tidycensus)
library(tidytext)




## ---

b25119_pull <- get_acs(
  geography = "county",
  state = "VA",
  table = "B25119",
  year = 2022,
  survey = "acs5",
  cache_table = TRUE
)

b25119_raw <- b25119_pull |> 
  filter(GEOID %in% faar_fips)


## ---

mls <- read_rds("data/faar_sales.rds") |> 
  filter(period == "annual")

## ---

avg_hh_size = 2.854

wecoop <- read_rds("data/wecoop.rds") |> 
  mutate(fips = as.character(fips)) |> 
  filter(fips %in% faar_fips) |> 
  mutate(name = str_remove_all(name, " County| city"))

pop <- read_csv("data/raw/pop-total.csv") |> 
  filter(as.character(GEOID) %in% faar_fips, year == 2022) |> 
  mutate(fips = as.character(GEOID)) |> 
  select(5, pop2022 = 4) 

faar_proj <- wecoop |> 
  group_by(fips, name) |> 
  summarize(
    proj2030 = value[year == 2030],
    proj2040 = value[year == 2040],
    proj2050 = value[year == 2050]
  ) |> 
  left_join(pop) |> 
  mutate(
    d2030 = (proj2030 - pop2022)/avg_hh_size,
    d2040 = (proj2040 - proj2030)/avg_hh_size,
    d2050 = (proj2050 - proj2040)/avg_hh_size
  ) |> 
  ungroup()

proj_pop <- faar_proj |> 
  select(2, 6, 3:5) |> 
  pivot_longer(
    cols = 2:5,
    names_to = "year",
    values_to = "pop"
  ) |> 
  mutate(year = str_sub(year, start = -4)) |> 
  mutate(pop = round(pop)) |> 
  mutate(chg = (pop - lag(pop)), .by = name) |> 
  mutate(chg = replace_na(chg, 0)) |> 
  mutate(chg_cum = cumsum(chg), .by = name) 

proj_hh <- faar_proj |> 
  select(2, 7:9) |> 
  pivot_longer(
    cols = 2:4,
    names_to = "year",
    values_to = "hh"
  ) |> 
  mutate(year = str_sub(year, start = -4)) |> 
  mutate(hh = round(hh)) |> 
  mutate(chg_cum = cumsum(hh), .by = name) 

proj_hh |> 
  filter(year == "2050")

## ---

bps <- read_rds("data/faar_cbps.rds") |> 
  mutate(type = case_when(
    type == "2-units" ~ "2-4 units",
    type == "3-4 units" ~ "2-4 units",
    TRUE ~ type
  ))

bps |> 
  filter(year > 2019) |> 
  summarise(
    units = sum(units)/3.75,
    .by = NAMELSAD
  )


## ---

calc_min_income <- function(price, 
                            rate = 0.065,
                            tax_ins = 0.25,
                            dp = 0.20,
                            term = 30,
                            dti = 0.28) {
  
  # Calculate monthly payment based on price
  monthly_loan <- price * (1 - dp) * 
    (rate/12 * (1 + rate/12)^(term * 12)) / 
    ((1 + rate/12)^(term * 12) - 1)
  
  # Add taxes and insurance to get total monthly payment
  monthly_total <- monthly_loan / (1 - tax_ins/12)
  
  # Calculate annual income needed based on DTI ratio
  annual_income <- (monthly_total / dti) * 12
  
  return(annual_income)
}

## ---

library(patchwork)

lfs <- read_csv("data/local_fact_sheets.csv")

lfs_plot <- lfs |> 
  filter(
    metric %in% c(
      "Average wage", "Median renter income", "Income to afford"
    )
  ) |> 
  mutate(
    detail = str_wrap(detail, 17)
  ) |>
  mutate(
    metric = str_replace(metric, "Median renter income", " ")
  ) |>
  mutate(
    metric = factor(
      metric, c("Income to afford", "Average wage", " ")
    )
  )

names = unique(lfs$locality)

lfs_plot |> 
  filter(locality == "Fredericksburg") |> 
  ggplot(
    aes(
      x = value,
      y = reorder_within(detail, value, metric),
      fill = metric
    )
  ) +
  facet_grid(
    rows = vars(metric),
    scales = "free_y", space = "free_y", switch = "y"
  ) +
  geom_col() +
  geom_text(
    aes(
      label = label_currency(accuracy = 1)(value) 
    ),
    color = "white",
    hjust = 1,
    nudge_x = -800,
    size = 16
  ) +
  geom_text(
    aes(
      x = 1000, label = detail 
    ),
    color = "white",
    hjust = 0,
    lineheight = 0.3,
    size = 12
  ) +
  scale_fill_manual(values = c(hda_pal[1], "#e76f52", hda_pal[5])) +
  scale_y_reordered(expand = c(0.3,0.3)) +
  scale_x_continuous(
    expand = c(0, 0)
  ) +
  theme_hda(
    strip.placement = "outside",
    strip.text.y.left = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    plot.margin = margin(0,0,0,0,"pt")
  )

ggsave(
  "rpt/img/x.png",
  width = 10, height = 4, bg = "white"
)

lfs_plot_fn <- function(name = "Fredericksburg") {

  plot <- lfs_plot |> 
    filter(locality == name) |> 
    ggplot(
      aes(
        x = value,
        y = reorder_within(detail, value, metric),
        fill = metric
      )
    ) +
    facet_grid(
      rows = vars(metric),
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    geom_col() +
    geom_text(
      aes(
       label = label_currency(accuracy = 1)(value) 
      ),
      color = "white",
      hjust = 1,
      nudge_x = -800,
      size = 16
    ) +
    geom_text(
      aes(
        x = 1000, label = detail 
      ),
      color = "white",
      hjust = 0,
      lineheight = 0.3,
      size = 12
    ) +
    scale_fill_manual(values = c(hda_pal[1], "#e76f52", hda_pal[5])) +
    scale_y_reordered(expand = c(0.3,0.3)) +
    scale_x_continuous(
      expand = c(0, 0)
    ) +
    theme_hda(
      strip.placement = "outside",
      strip.text.y.left = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      plot.margin = margin(0,0,0,0,"pt")
    )
  
  ggsave(
    filename = paste0("rpt/img/lfs_", name, ".png"),
    plot = plot,
    width = 10, height = 4, bg = "white"
  )
  
  }

names |> 
  walk(\(x) lfs_plot_fn(name = x))


## ---

lfs_mls_fn <- function(x) {
  
  plot <- mls |> 
    filter(name == x) |> 
    ggplot(aes(x = year, y = med_price)) +
    geom_area(
      color = hda_pal[1],
      fill = hda_pal[1],
      alpha = 0.3
    ) +
    scale_x_continuous(expand = c(0.05, 0.05), breaks = c(2020, 2024)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    add_zero_line() +
    theme_hda(
      axis.text.x = element_text(hjust = 0.5, size = 24, color = "grey50"),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    filename = paste0("rpt/img/lfs_mls_", x, ".png"),
    plot = plot,
    width = 2, height = 2.5, bg = "white"
  )
}

names |> 
  walk(\(x) lfs_mls_fn(x = x))
  
  
## ---
  
costar <- read_rds("data/faar_costar.rds") |> 
  filter(year > 2019) |> 
  select(1, 2, 3, rent = 7) |> 
  mutate(date = yq(paste0(year, "Q", qtr)))

names_costar = unique(costar$name)

lfs_rent_fn <- function(x) {
  
  plot <- costar |> 
    filter(name == x) |> 
    ggplot(aes(x = date, y = rent)) +
    geom_area(
      color = hda_pal[4],
      fill = hda_pal[4],
      alpha = 0.3
    ) +
    scale_x_continuous(
      expand = c(0.05, 0.05),
      breaks = c(as.Date("2020-01-01"), as.Date("2024-01-01")),
      labels = label_date("%Y")
    ) +
    scale_y_continuous(expand = c(0.01, 0)) +
    add_zero_line() +
    theme_hda(
      axis.text.x = element_text(hjust = 0.5, size = 24, color = "grey50"),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    filename = paste0("rpt/img/lfs_rent_", x, ".png"),
    plot = plot,
    width = 2, height = 2.5, bg = "white"
  )
}

names_costar |> 
  walk(\(x) lfs_rent_fn(x = x))
