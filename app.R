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
library(shinydashboard)

sdf_eq=st_read('sdf_earthquake.gpkg')
sdf_eq_countries=st_read('country_geometries.gpkg')

darken <- function(col, factor = 0.8) {
  rgb_vals <- grDevices::col2rgb(col) / 255
  rgb(
    rgb_vals[1, ] * factor,
    rgb_vals[2, ] * factor,
    rgb_vals[3, ] * factor
  )
}



country_geom=sdf_eq_countries %>% group_by(country_final) %>% summarize()

ids=sdf_eq %>% select(id,date_final,magnitude,country_final) %>% st_drop_geometry()
id_country_geom=st_as_sf(inner_join(ids,country_geom, by=('country_final')))






# Define UI for application that draws a histogram

#
ui = dashboardPage(
  dashboardHeader(title='Earthquake Tracker'),
  dashboardSidebar(
    sliderInput("Year", "Date Slider:", 
                min(year(sdf_eq$date_final)), 
                max(year(sdf_eq$date_final)), 
                2020)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Global Earthquakes",
        leafletOutput("spatial_plot", height = 350),
        width = 12
      )
    )
  )
)


# Define server logic required to draw a histogram
server = function(input, output, session) {
  
  output$spatial_plot <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = sdf_eq$magnitude
    )
    
      country_map=id_country_geom %>%
        mutate(year_=year(date_final)) %>%
        filter(year_==input$Year &country_final!='Unknown') %>%
        group_by(country_final,year_) %>%
        summarize(mean_mg=mean(magnitude))

      country_map=st_transform(country_map, 4326)
      #pal = colorNumeric(palette = "YlOrRd", domain = country_map$mean_mg)

      labels_c <- lapply(seq_len(nrow(country_map)), function(i) {
        HTML(paste0(
          "Country: ", country_map$country_final[i], "<br/>",
          "Average Magnitude: ", round(country_map$mean_mg[i],2), "<br/>"
        ))
      })
    
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
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = country_map,
        fillColor = ~pal(mean_mg),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        group = "Countries",
        label = labels_c
      ) %>%
      
      addCircleMarkers(
        data = id_df,
        radius = ~magnitude,
        stroke = FALSE,
        fillOpacity = 0.6,
        color = ~darken(pal(magnitude),0.8),
        group = "Earthquakes",
        label = labels
      ) %>%
      addLegend(
        pal = pal,
        values = country_map$mean_mg,
        title = "Magnitude",
        position = "bottomright"
      ) %>%
      setView(lng =18.2812 , lat = 9.1021, zoom = 2) %>% 
      
      # 🎛 Layer control (this is what makes it properly "layered")
      addLayersControl(
        overlayGroups = c("Countries", "Earthquakes"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
}







# Run the application 
shinyApp(ui = ui, server = server)
