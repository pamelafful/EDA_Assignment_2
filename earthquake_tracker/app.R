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
           plotOutput("spatial_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$spatial_plot <- renderPlot({
      col_pal <- colorNumeric(
        palette = "YlOrRd",
        domain = sdf_earthquake$magnitude
      )
      
      id_df=sdf_earthquake %>% filter(year(date_final)==input$Year ) %>% 
        # mutate(country=case_when(
        #   location %in% all_countries ~location)) %>% 
        # filter( !is.na(country)) %>% 
        distinct(id,country_final,magnitude,location,richter_scale,horizontaldistance,geometry,depth)
      
      labels = lapply(seq_len(nrow(sdf_earthquake)), function(i) {
        HTML(paste0(
          "<strong>", id_df$id[i], "</strong><br/>",
          "Country: ", id_df$country_final[i], "<br/>",
          "magnitude: ", id_df$magnitudel[i], "<br/>",
          "scale: ", id_df$richter_scale[i], "<br/>",
          "horizontal distance: ", id_df$horizontaldistance[i], "<br/>",
          "depth: ", id_df$depth[i], "<br/>"
        ))
      })
      
      mp=leaflet(id_df) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = ~magnitude,   
          color = "purple",       
          weight = 1,             
          opacity = 1,               
          fillColor = ~col_pal(magnitude), 
          fillOpacity = 0.8,         
          stroke = TRUE,
          label = ~labels
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
