#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(zoo)
library(plotly)
library(spacyr)
library(sf)
library(stringr)
library(tidyr)
library(countries)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(knitr)
library(skimr)
library(webshot2)
library(htmlwidgets)
library(terra)
library(knitr)
library(tibble)
library(GGally)
library(float)
library(htmltools)

sdf_eq=st_read('sdf_earthquake.gpkg')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Spatial Distribution of Earthquake Occurances"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Year",
                        "Year Range:",
                        min = min(year(sdf_eq$date_final)),
                        max = max(year(sdf_eq$date_final)),
                        value = 2020)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("spatial_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$spatial_plot <- renderLeaflet({
    
    col_pal <- colorNumeric(
      palette = "YlOrRd",
      domain = sdf_eq$magnitude
    )
    
    id_df <- sdf_eq %>% 
      filter(year(date_final) == input$Year) %>% 
      distinct(id, country_final, magnitude, location, 
               richter_scale, horizontaldistance, geom, depth)
    
    labels <- lapply(seq_len(nrow(id_df)), function(i) {
      HTML(paste0(
        "<strong>", id_df$id[i], "</strong><br/>",
        "Country: ", id_df$country_final[i], "<br/>",
        "Magnitude: ", id_df$magnitude[i], "<br/>",
        "Scale: ", id_df$richter_scale[i], "<br/>",
        "Horizontal distance: ", id_df$horizontaldistance[i], "<br/>",
        "Depth: ", id_df$depth[i], "<br/>"
      ))
    })
    
    leaflet(id_df) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~magnitude,
        color = "purple",
        weight = 1,
        opacity = 1,
        fillColor = ~col_pal(magnitude),
        fillOpacity = 0.8,
        stroke = TRUE,
        label = labels
      ) %>%
      addLegend(
        pal = col_pal,
        values = ~magnitude,
        position = "bottomright",
        title = "Magnitude"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
