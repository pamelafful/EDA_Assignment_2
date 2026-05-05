#library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)

# Load data once
sdf_eq <- st_read('sdf_earthquake.gpkg') %>% 
  filter(country_final != 'Unknown') %>%
  mutate(date_month = floor_date(date_final, unit = 'month'),
         year_ = year(date_final))

sdf_eq_countries <- st_read('country_geometries.gpkg')

# Precompute static elements
country_geom <- sdf_eq_countries %>% 
  group_by(country_final) %>% 
  summarize() %>%
  st_transform(4326)  # Transform once

# Create id data without geometry for faster filtering
ids_df <- sdf_eq %>% 
  select(id, date_final, date_month, year_, magnitude, country_final, 
         continent, richter_scale, location, horizontaldistance, depth) %>% 
  st_drop_geometry()

# Keep spatial data separate for mapping
sdf_eq_spatial <- sdf_eq %>%
  select(id, magnitude, geom)

country_options <- sort(unique(ids_df$country_final))
scale_options <- sort(unique(ids_df$richter_scale))
continent_options <- sort(unique(ids_df$continent[!is.na(ids_df$continent)]))

darken <- function(col, factor = 0.8) {
  rgb_vals <- grDevices::col2rgb(col) / 255
  rgb(rgb_vals[1, ] * factor, rgb_vals[2, ] * factor, rgb_vals[3, ] * factor)
}

ui <- dashboardPage(
  dashboardHeader(title = 'Earthquake Tracker'),
  dashboardSidebar(
    dateRangeInput(
      "Year",
      "Select Date Range:",
      start = min(ids_df$date_month),
      end = max(ids_df$date_month),
      min = min(ids_df$date_month),
      max = max(ids_df$date_month),
      format = "M yyyy",
      startview = "year"
    ),
    pickerInput(
      "Country_Filter",
      "Choose Country:",
      choices = country_options,
      multiple = TRUE,
      selected = country_options,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    pickerInput(
      "Continent_Filter",
      "Choose Region:",
      choices = continent_options,
      selected = continent_options,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    pickerInput(
      "Scale_Filter",
      "Choose Magnitude Scale:",
      choices = scale_options,
      selected = scale_options,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    )
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
          column(width = 6, plotlyOutput("eq_pr_yr", height = 350)),
          column(width = 6, plotlyOutput("con_bb_plot", height = 350))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for filtered data (computed once per input change)
  filtered_data <- reactive({
    req(input$Year, input$Country_Filter, input$Scale_Filter)
    
    ids_df %>%
      filter(
        date_month >= input$Year[1],
        date_month <= input$Year[2],
        country_final %in% input$Country_Filter,
        richter_scale %in% input$Scale_Filter
      )
  })
  
  # Reactive for country-level aggregates
  country_summary <- reactive({
    filtered_data() %>%
      group_by(country_final, year_) %>%
      summarize(mean_mg = mean(magnitude, na.rm = TRUE), .groups = 'drop') %>%
      left_join(country_geom, by = "country_final") %>%
      st_as_sf()
  })
  
  # Create color palette once
  pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 10))
  
  output$spatial_plot <- renderLeaflet({
    # Initialize map only once
    leaflet() %>%
      addTiles() %>%
      setView(lng = 18.2812, lat = 9.1021, zoom = 2) %>%
      addLegend(
        pal = pal,
        values = c(0, 10),
        title = "Magnitude",
        position = "bottomright"
      ) %>%
      addLayersControl(
        overlayGroups = c("Countries", "Earthquakes"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Update map layers with leafletProxy for better performance
  observe({
    req(filtered_data())
    
    country_map <- country_summary()
    
    # Get spatial data for filtered earthquakes
    filtered_ids <- filtered_data() %>% distinct(id)
    id_spatial <- sdf_eq_spatial %>%
      filter(id %in% filtered_ids$id)
    
    # Create labels
    labels_c <- lapply(seq_len(nrow(country_map)), function(i) {
      HTML(paste0(
        "Country: ", country_map$country_final[i], "<br/>",
        "Average Magnitude: ", round(country_map$mean_mg[i], 2)
      ))
    })
    
    # Join for earthquake labels (only necessary fields)
    id_df <- filtered_data() %>%
      inner_join(st_drop_geometry(id_spatial), by = "id") %>%
      select(id, country_final, magnitude, location, richter_scale, 
             horizontaldistance, depth) %>%
      inner_join(id_spatial, by = c("id", "magnitude"))
    
    labels <- lapply(seq_len(nrow(id_df)), function(i) {
      HTML(paste0(
        "<strong>", id_df$id[i], "</strong><br/>",
        "Country: ", id_df$country_final[i], "<br/>",
        "Magnitude: ", id_df$magnitude[i], "<br/>",
        "Scale: ", id_df$richter_scale[i], "<br/>",
        "Horizontal distance: ", id_df$horizontaldistance[i], "<br/>",
        "Depth: ", id_df$depth[i]
      ))
    })
    
    leafletProxy("spatial_plot") %>%
      clearGroup("Countries") %>%
      clearGroup("Earthquakes") %>%
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
        stroke = TRUE,
        weight = 1.5,
        fillOpacity = 0.6,
        fillColor = ~pal(magnitude),
        color = ~darken(pal(magnitude), 0.8),
        group = "Earthquakes",
        label = labels
      )
  })
  
  output$eq_pr_yr <- renderPlotly({
    req(filtered_data())
    
    fig <- filtered_data() %>%
      group_by(year_, country_final) %>%
      summarize(eq_cnt = n_distinct(id), 
                mean_mg = mean(magnitude, na.rm = TRUE), 
                .groups = 'drop') %>%
      ggplot(aes(x = year_, y = eq_cnt, fill = mean_mg)) +
      geom_col() +
      scale_fill_gradient(low = "yellow", high = "red") +
      labs(title = "Number of Earthquakes per year",
           y = '#occurances',
           x = "year",
           fill = 'avg magnitude') +
      theme_minimal()
    
    ggplotly(fig)
  })
  
  output$con_bb_plot <- renderPlotly({
    req(filtered_data())
    
    # Note: you reference 'sdf_earthquake' which isn't in your original code
    # I'm assuming you meant 'sdf_eq' or using filtered_data
    bc <- filtered_data() %>%
      filter(!is.na(continent)) %>%
      group_by(continent) %>%
      mutate(cont_occ_sum = n_distinct(id)) %>%
      ungroup() %>%
      ggplot(aes(x = nst, y = magnitude, size = cont_occ_sum, color = continent)) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(1, 5)) +
      labs(title = "Avg Magnitude vs Avg Number of Seismic Stations (NST)",
           y = 'magnitude',
           x = "NST",
           color = 'continent',
           size = '') +
      theme_minimal()
    
    ggplotly(bc)
  })
}

shinyApp(ui = ui, server = server)