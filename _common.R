# Set global knitr chunk options

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE,
  message = FALSE,
  fig.show = "hold",
  fig.asp = 0.618,
  fig.align = "left"
)

# Load core packages

library(tidyverse)
library(scales)
library(kableExtra)
library(formattable)
library(hdatools)
library(ggtext)

# Color palettes

hda_pal <- c(
  "#445ca9", # Blue
  "#8baeaa", # Green
  "#e9ab3f", # Yellow
  "#e76f52", # Coral
  "#a97a92", # Lavender
  "#8abc8e"  # Sea Green
)

# Cost burden label color palette

cb_pal <- c(
  "Severely cost-burdened" = hda_pal[4],
  "Cost-burdened" = hda_pal[3],
  "Not cost-burdened" = "grey80"
)

# List of FAAR region locality FIPS codes

faar_fips <- c("51033", "51099", "51137", "51177", "51179", "51630")

# Shortcut function for captions on plots with ACS data

acs_cap <- function(table, year = "2018-2022") {
  paste0(
    "**Source:** U.S. Census Bureau, ",
    year,
    " American Community Survey 5-year estimates, Table ",
    table,
    "."
  )  
}

# Special function to apply str_wrap() to ordered factors

fct_wrap <- function(f, width) {
  fct_relabel(f, ~ str_wrap(., width = width))
}

# Set plot rendering options

if (knitr::is_html_output()) {
  
  knitr::opts_chunk$set(out.width = "100%")
  
} else {
  
  knitr::opts_chunk$set(dpi = 150)
  
}



