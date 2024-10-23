
library(tidyverse)
library(hdatools)
library(scales)

faar_fips <- c("51033", "51099", "51137", "51177", "51179", "51630")

pop_total <- read_csv("data/raw/pop-total.csv")

pop_faar <- pop_total |> 
  filter(GEOID %in% faar_fips) |> 
  mutate(GEOID = as.character(GEOID)) |> 
  mutate(name = case_match(
    GEOID,
    "51033" ~ "Caroline",
    "51099" ~ "King George",
    "51137" ~ "Orange",
    "51177" ~ "Spotsylvania",
    "51179" ~ "Stafford",
    "51630" ~ "Fredericksburg"
  )) |> 
  mutate(group = case_when(
    name == "Spotsylvania" ~ name,
    name == "Stafford" ~ name,
    .default = "Other localities"
  ))

pop_faar |> 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_col() +
  facet_wrap(~fct_rev(group)) +
  scale_fill_hda() +
  scale_x_continuous(
    breaks = seq(2010, 2022, 2),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.05),
    labels = label_number(scale = 0.001, suffix = "k")
  ) +
  add_zero_line() +
  labs(
    title = "Total population by locality",
    subtitle = "2010-2022",
    caption = "**Source:** Census Bureau, Population Estimates Program."
  ) +
  theme_hda(
    base_size = 18,
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


inc <- read_rds("data/b25118.rds")

inc |> 
  summarise(
    estimate = sum(estimate),
    .by = c("year", "income")
  ) |>
  group_by(year) |> 
  mutate(
    pct = estimate/sum(estimate),
    income = fct_relevel(
      income,
      c(
      "$150,000 or more",
      "$100,000 to $149,999",
      "$75,000 to $99,999",
      "$50,000 to $74,999",
      "$25,000 to $49,999",
      "$15,000 to $24,999",
      "Less than $15,000"
      )
    )
  ) |> 
  ggplot(aes(x = as.character(year), y = pct, fill = income)) +
  geom_col() +
  scale_fill_brewer(palette = "RdYlBu") +
  #scale_x_continuous(
   # expand = c(0.01, 0.01)
    #labels = label_number(accuracy = 1, big.mark = "")
  #) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Household income",
    subtitle = "All households in region (2010-2022)",
    caption = "**Source:** Census Bureau, American Community Survey, Table B25118."
  ) +
  theme_hda(
    base_size = 16,
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
