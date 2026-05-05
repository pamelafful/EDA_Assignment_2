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
library(plotly)

library(sf)
library(stringr)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(tibble)
library(GGally)
library(float)
library(shinydashboard)
library(plotly)
library(shinyWidgets)

# sdf_eq=st_read('sdf_earthquake.gpkg') %>% filter( country_final !='Unknown')
# sdf_eq_countries=st_read('country_geometries.gpkg')
sdf_eq <- readRDS("sdf_eq.rds") %>% filter( country_final !='Unknown')
sdf_eq_countries <- readRDS("sdf_eq_countries.rds")%>% filter( country_final !='Unknown')


sdf_eq <- sdf_eq %>%
  mutate(
    date_month = floor_date(date_final, unit = "month"),
    year_ = year(date_final)
  )

darken <- function(col, factor = 0.8) {
  rgb_vals <- grDevices::col2rgb(col) / 255
  rgb(
    rgb_vals[1, ] * factor,
    rgb_vals[2, ] * factor,
    rgb_vals[3, ] * factor
  )
}



country_geom=sdf_eq_countries %>% group_by(country_final) %>% summarize()

ids=sdf_eq %>% select(id,date_final,magnitude,country_final,continent) %>% st_drop_geometry()
id_country_geom=st_as_sf(inner_join(ids,country_geom, by=('country_final')))

country_options=sdf_eq %>% distinct(country_final) %>% pull()
country_options=country_options[country_options!='Unknown']


scale_options=sdf_eq %>% distinct(richter_scale) %>% pull()
# scale_options[lenghth(scale_options)+1]='All'

continent_options=sdf_eq %>% distinct(continent) %>% pull()
# continent_options[lenghth(continent_options)+1]='All'

ui = dashboardPage(
  dashboardHeader(title='Earthquake Tracker'),
  dashboardSidebar(
    
    dateRangeInput(
      "Year",
      "Select Date Range:",
      start = min(floor_date(sdf_eq$date_final,unit='month')),
      end = max(floor_date(sdf_eq$date_final,unit='month')),
      min = min(floor_date(sdf_eq$date_final,unit='month')),
      max = max(floor_date(sdf_eq$date_final,unit='month')),
      format = "M yyyy",
      startview = "year"
    ),
    
    # selectInput(
    #   inputId = "Country_Filter",
    #   label = "Choose Country:",
    #   choices = country_options,
    #   multiple = TRUE,
    #   selected = country_options
    # ),
    
    pickerInput(
      "Country_Filter",
      "Choose Country:",
      choices = country_options,
      multiple = TRUE,
      selected = country_options,
      options = list(
        `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
        `live-search` = TRUE,  # Enables search box
        `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
      )
    ),
    
    pickerInput(
      "Continent_Filter",
      "Choose Region:",
      choices = continent_options,
      selected=continent_options,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
        `live-search` = TRUE,  # Enables search box
        `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
      )
    ),
    pickerInput(
      "Scale_Filter",
      "Choose Magnitude Scale:",
      choices = scale_options,
      selected=scale_options,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
        `live-search` = TRUE,  # Enables search box
        `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
      )
    )
    
    # selectInput(
    #   inputId = "Continent_Filter",
    #   label = "Choose Region:",
    #   choices = continent_options,
    #   multiple = TRUE,
    #   selected = continent_options
    # ),
    
    # selectInput(
    #   inputId = "Scale_Filter",
    #   label = "Choose Magnitude Scale:",
    #   choices = scale_options,
    #   multiple = TRUE,
    #   selected = continent_options
    # )
    
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Global Earthquakes",
        leafletOutput("spatial_plot", height = 350),
        width = 12
      )
    ),
    fluidRow(
      box(
        title = "Earthquake Statistics",
        width = 12,
        fluidRow(
          column(
            width = 6,
            plotlyOutput("eq_pr_yr", height = 350)
          ),
          column(
            width = 6,
            plotlyOutput("con_bb_plot", height = 350)
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output, session) {
  
    
    # -------------------------------
    # 1. CORE FILTERED DATA
    # -------------------------------
    filtered_data <- reactive({
      req(input$Year, input$Country_Filter, input$Scale_Filter)
      
      sdf_eq %>%
        filter(
          date_month >= input$Year[1],
          date_month <= input$Year[2],
          country_final %in% input$Country_Filter,
          richter_scale %in% input$Scale_Filter
        )
    }) %>% debounce(300)
    
    
    # -------------------------------
    # 2. COUNTRY-LEVEL SUMMARY
    # -------------------------------
    country_data <- reactive({
      filtered_data() %>% st_drop_geometry() %>%
        group_by(country_final) %>%
        summarize(
          mean_mg = mean(magnitude, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        inner_join(country_geom, by = "country_final") %>%
        st_as_sf()
    })
    
    
    # -------------------------------
    # 3. INITIAL MAP (render once)
    # -------------------------------
    output$spatial_plot <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 18.2812, lat = 9.1021, zoom = 2)
    })
    
    
    # -------------------------------
    # 4. MAP UPDATES (fast)
    # -------------------------------
    observe({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      # palette based on filtered data
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = df$magnitude
      )
      
      # country polygons
      country_map <- country_data() %>%
        st_transform(4326)
      
      # earthquake points (keep geometry safe)
      points_df <- df %>%
        select(id, country_final, magnitude, location,
               richter_scale, horizontaldistance, depth, geom) %>%
        distinct()
      
      # labels (vectorised = faster than lapply)
      labels_points <- sprintf(
        "<strong>%s</strong><br/>Country: %s<br/>Magnitude: %.2f<br/>Scale: %s<br/>Distance: %s<br/>Depth: %s",
        points_df$id,
        points_df$country_final,
        points_df$magnitude,
        points_df$richter_scale,
        points_df$horizontaldistance,
        points_df$depth
      ) %>% lapply(htmltools::HTML)
      
      labels_countries <- sprintf(
        "Country: %s<br/>Avg Magnitude: %.2f",
        country_map$country_final,
        country_map$mean_mg
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("spatial_plot") %>%
        clearShapes() %>%
        clearMarkers() %>%
        
        addPolygons(
          data = country_map,
          fillColor = ~pal(mean_mg),
          weight = 1,
          color = "white",
          fillOpacity = 0.7,
          group = "Countries",
          label = labels_countries
        ) %>%
        
        addCircleMarkers(
          data = points_df,
          radius = ~pmax(3, magnitude),  # prevent tiny circles
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.6,
          fillColor = ~pal(magnitude),
          color = ~darken(pal(magnitude), 0.8),
          group = "Earthquakes",
          label = labels_points,
          clusterOptions = markerClusterOptions()  # HUGE performance gain
        ) %>%
        
        addLegend(
          pal = pal,
          values = df$magnitude,
          title = "Magnitude",
          position = "bottomright"
        ) %>%
        
        addLayersControl(
          overlayGroups = c("Countries", "Earthquakes"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    
    # -------------------------------
    # 5. EARTHQUAKES PER YEAR
    # -------------------------------
    # output$eq_pr_yr <- renderPlotly({
    #   df <- filtered_data()
    #   req(nrow(df) > 0)
    #   
    #   plot_df <- df %>%
    #     group_by(year_, country_final) %>%
    #     summarize(
    #       eq_cnt = n_distinct(id),
    #       mean_mg = mean(magnitude),
    #       .groups = "drop"
    #     )
    #   
    #   fig=ggplot(plot_df, aes(x = year_, y = eq_cnt, fill = mean_mg)) +
    #     geom_col() +
    #     scale_fill_gradient(low = "yellow", high = "red") +
    #     labs(
    #       title = "Number of Earthquakes per Year",
    #       y = "# Occurrences",
    #       x = "Year",
    #       fill = "Avg Magnitude"
    #     ) +
    #     theme_minimal() 
    #     ggplotly(fig)
    # })
    
    
    # -------------------------------
    # 6. CONTINENT BUBBLE PLOT
    # -------------------------------
    # output$con_bb_plot <- renderPlotly({
    #   df <- filtered_data()
    #   req(nrow(df) > 0)
    #   
    #   plot_df <- df %>%
    #     filter(!is.na(continent)) %>%
    #     group_by(continent) %>%
    #     mutate(cont_occ_sum = n_distinct(id)) %>%
    #     ungroup()
    #   
    #   fig=ggplot(plot_df,
    #          aes(x = nst, y = magnitude,
    #              size = cont_occ_sum,
    #              color = continent)) +
    #     geom_point(alpha = 0.5) +
    #     scale_size(range = c(1, 5)) +
    #     labs(
    #       title = "Magnitude vs Seismic Stations (NST)",
    #       y = "Magnitude",
    #       x = "NST",
    #       color = "Continent",
    #       size = ""
    #     ) +
    #     theme_minimal() 
    #     
    #   ggplotly(fig)
    # })
    
  }






# Run the application 
shinyApp(ui = ui, server = server)