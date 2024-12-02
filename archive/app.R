#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
install.packages(c("shiny", "plotly", "scales"))

remotes::install_github("hdadvisors/hdatools")

library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(tigris)
library(plotly)
library(hdatools)

# Load Virginia counties shapefile
va_counties <- counties("VA", cb = TRUE)

# Load weldon center data 
wecoop <- wecoop %>%
  mutate(fips = as.character(fips))

#List of FAAR counties
FAAR_counties <- c("Caroline County", "King George County", "Orange County", 
                   "Spotsylvania County", "Stafford County", "Fredericksburg city")

wecoop_filtered <- wecoop %>%
  group_by(fips, name) %>%
  summarize(
    value_2020 = value[year == 2020],
    value_2050 = value[year == 2050],
    percent_change = (value_2050 - value_2020) / value_2020 * 100
  )


#filter for our counties

va_counties <- va_counties %>%
  filter(NAMELSAD %in% FAAR_counties) %>%
  rename(fips = GEOID)

# Merge the wecoop_filtered data with the shapefile
va_counties_data <- va_counties %>%
  full_join(wecoop_filtered, by = c("fips" = "fips")) 

# Define UI 
ui <- fluidPage(
  titlePanel("Projected Population Change 2020-2050"),
  plotlyOutput("map", height = "600px")
)

# Define server logic
server <- function(input, output) {
  
  output$map <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = va_counties_data, fill = "lightgrey", color = "white") +
      geom_sf(data = va_counties_data, aes(fill = percent_change, 
                                           text = paste0("County: ", NAMELSAD, "<br>",
                                                         "2020 Population: ", value_2020, "<br>",
                                                         "2050 Population: ", value_2050, "<br>",
                                                         "Percent Change: ", round(percent_change, 2), "%")), 
              color = "black") +
      scale_fill_gradient2(
        low = "#445ca9", mid = "#e9ab3f", high = "#e76f52", 
        midpoint = median(va_counties_data$percent_change),
        name = "% Change",
        labels = scales::percent_format(scale = 1)
      ) +
      labs(title = "Projected Population Change 2020-2050",
           subtitle = "Selected Virginia Counties") +
      theme_hda() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(0.5, "cm")
      ) +
      coord_sf(xlim = c(min(st_coordinates(va_counties_data)[,1]), 
                        max(st_coordinates(va_counties_data)[,1])),
               ylim = c(min(st_coordinates(va_counties_data)[,2]), 
                        max(st_coordinates(va_counties_data)[,2])),
               expand = TRUE)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)