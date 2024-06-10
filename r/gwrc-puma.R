library(tidyverse)
library(tigris)
library(mapview)

va_puma <- pumas(state = "VA", cb = TRUE, year = 2020)

va_local <- counties(state = "VA", cb = TRUE, year = 2020)

gwrc <- va_puma |> 
  filter(str_detect(NAMELSAD20, "George Washington Regional Commission"))

mapview(va_local) + mapview(gwrc)


library(tidycensus)
