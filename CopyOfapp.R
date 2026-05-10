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
library(htmltools)
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
library(gt)
library(DT)
#install.packages('gt')
#install.packages('DT')

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

# ui = dashboardPage(
#   dashboardHeader(title='Global Filters'),
#   dashboardSidebar(
#     
#     sliderInput(
#       "Year",
#       "Select Year Range:",
#       min = year(min(sdf_eq$date_final, na.rm = TRUE)),
#       max = year(max(sdf_eq$date_final, na.rm = TRUE)),
#       value = c(
#         year(min(sdf_eq$date_final, na.rm = TRUE)),
#         year(max(sdf_eq$date_final, na.rm = TRUE))
#       ),
#       step = 1,
#       sep = ""
#     ),    
#     pickerInput(
#       "Country_Filter",
#       "Choose Country:",
#       choices = country_options,
#       multiple = TRUE,
#       selected = country_options,
#       options = list(
#         `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
#         `live-search` = TRUE,  # Enables search box
#         `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
#       )
#     ),
#     
#     pickerInput(
#       "Continent_Filter",
#       "Choose Region:",
#       choices = continent_options,
#       selected=continent_options,
#       multiple = TRUE,
#       options = list(
#         `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
#         `live-search` = TRUE,  # Enables search box
#         `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
#       )
#     ),
#     pickerInput(
#       "Scale_Filter",
#       "Choose Magnitude Scale:",
#       choices = scale_options,
#       selected=scale_options,
#       multiple = TRUE,
#       options = list(
#         `actions-box` = TRUE,  # Adds "Select All" / "Deselect All" buttons
#         `live-search` = TRUE,  # Enables search box
#         `selected-text-format` = "count > 3"  # Shows "3 items selected" after 3
#       )
#     )
#   ),
#   dashboardBody(
#     fluidRow(
#       box(
#         title = "Global Earthquakes",
#         leafletOutput("spatial_plot", height = 350),
#         width = 12
#       )
#     ),
#     
#     fluidRow(
#       column(
#         width = 6,
#         box(
#           title = "Top 5 Countries by Earthquake Metrics",
#           status = "primary",
#           solidHeader = TRUE,
#           width = 12,
#           
#           fluidRow(
#             column(
#               width = 6,
#               radioButtons(
#                 "feature_select",
#                 "Select Feature:",
#                 choices = c(
#                   "Earthquake Frequency" = "frequency",
#                   "Average Magnitude" = "avg_magnitude",
#                   "Scale Category" = "scale_category"
#                 ),
#                 selected = "frequency",
#                 inline = FALSE
#               )
#             ),
#             column(
#               width = 6,
#               conditionalPanel(
#                 condition = "input.feature_select == 'scale_category'",
#                 selectInput(
#                   "scale_select",
#                   "Select Scale:",
#                   choices = scale_options,
#                   selected = "Moderate"
#                 )
#               )
#             )
#           ),
#           
#           DTOutput("top_countries_table")
#         )
#       ),
#       column(
#         width = 6,
#         box(
#           title = "Earthquake Occurrences by Hour of Day",
#           status = "info",
#           solidHeader = TRUE,
#           width = NULL,
#           
#           plotlyOutput("hrly_dist")
#           
#         )
#       )
#     ),
#     
# 
#     fluidRow(
#       box(
#         title = "Magnitude Analysis",
#         width = 12,
#         
#         fluidRow(
#           column(
#             width = 6,
#             plotlyOutput("eq_pr_yr", height = 350)
#           ),
#           
#          
#           column(
#             width = 6,
#             box(
#               width = NULL,  # NULL makes it fill the column width
#               status = "primary",
#               solidHeader = TRUE,
#               #title = "",
#               
#               radioButtons(
#                 "bubble_var_select",
#                 "Select X-axis Variable:",
#                 choices = c(
#                   "Seismic Stations (NST)" = "nst",
#                   "Depth" = "depth",
#                   "Horizontal Distance" = "horizontaldistance"
#                 ),
#                 selected = "nst",
#                 inline = TRUE
#               ),
#               
#               plotlyOutput("con_bb_plot", height = 350)
#             )
#           ), 
#   
#           
#         )
#       )
#     )
#     
# 
# 
#   )
# )




ui = dashboardPage(
  dashboardHeader(title='Global Filters'),
  dashboardSidebar(
    
    sliderInput(
      "Year",
      "Select Year Range:",
      min = year(min(sdf_eq$date_final, na.rm = TRUE)),
      max = year(max(sdf_eq$date_final, na.rm = TRUE)),
      value = c(
        year(min(sdf_eq$date_final, na.rm = TRUE)),
        year(max(sdf_eq$date_final, na.rm = TRUE))
      ),
      step = 1,
      sep = ""
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
      selected=continent_options,
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
      selected=scale_options,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    )
  ),
  dashboardBody(
    # First row - Map
    fluidRow(
      box(
        title = "Global Earthquakes",
        leafletOutput("spatial_plot", height = 350),
        width = 12
      )
    ),
    
    # Second row - Table and Hourly Distribution (matching third row structure)
    fluidRow(
      box(
        title = "Country Rankings & Hourly Patterns",
        width = 12,
        
        fluidRow(
          column(
            width = 6,
            radioButtons(
              "feature_select",
              "Select Feature:",
              choices = c(
                "Earthquake Frequency" = "frequency",
                "Average Magnitude" = "avg_magnitude",
                "Scale Category" = "scale_category"
              ),
              selected = "frequency",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.feature_select == 'scale_category'",
              selectInput(
                "scale_select",
                "Select Scale:",
                choices = scale_options,
                selected = "Moderate"
              )
            ),
            DTOutput("top_countries_table", height = "350px")
          ),
          column(
            width = 6,
            plotlyOutput("hrly_dist", height = 400)
          )
        )
      )
    ),
    
    # Third row - Time Series and Bubble Plot
    fluidRow(
      box(
        title = "Magnitude Analysis",
        width = 12,
        
        fluidRow(
          column(
            width = 6,
            plotlyOutput("eq_pr_yr", height = 400)
          ),
          column(
            width = 6,
            radioButtons(
              "bubble_var_select",
              "Select X-axis Variable:",
              choices = c(
                "Seismic Stations (NST)" = "nst",
                "Depth" = "depth",
                "Horizontal Distance" = "horizontaldistance"
              ),
              selected = "nst",
              inline = TRUE
            ),
            plotlyOutput("con_bb_plot", height = 400)
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
          year_ >= input$Year[1],
          year_ <= input$Year[2],
          country_final %in% input$Country_Filter,
          richter_scale %in% input$Scale_Filter
        )
    }) %>% debounce(300)
    
    joy_filtered_data <- reactive({
      req(input$Year, input$Country_Filter, input$Scale_Filter)
      sdf_eq %>%
        filter(
          year_ >= input$Year[1],
          year_ <= input$Year[2],
          country_final %in% input$Country_Filter
        )
    }) %>% debounce(300) 
    
    
    # filter for eq_stats visuals
    ind_filtered_data <- reactive({
      req(input$Country_Filter, input$Scale_Filter, input$Continent_Filter)
      
        sdf_eq %>% mutate(year_= year(date_final)) %>% 
        filter(richter_scale %in% input$Scale_Filter & 
                 country_final %in% input$Country_Filter &
                 continent %in% input$Continent_Filter)
    }) %>% debounce(300)
    
 
    bb_filtered_data <- reactive({
      req(input$Scale_Filter, input$Year)
      
      sdf_eq %>% mutate(year_= year(date_final)) %>% 
        filter(richter_scale %in% input$Scale_Filter,
               year_ >= input$Year[1],
               year_ <= input$Year[2])
    }) %>% debounce(300)  
    
    
    top_5_filtered_data=reactive({
        sdf_eq %>%
          filter(year(date_final) >= input$Year[1] & 
                   year(date_final) <= input$Year[2])
      })
    
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
      points_df <- df %>% filter(country_final %in% input$Country_Filter) %>% 
        select(id, country_final, magnitude, location,
               richter_scale, horizontaldistance, depth, geom) %>%
        distinct()
      
      
      mg=points_df %>% summarize(mean_mg=round(mean(magnitude),2) ) %>% 
        pull(mean_mg)
      
      total=points_df %>% summarize(total_id=n_distinct(id) ) %>% pull(total_id)
      
      ht=points_df %>% filter(country_final!='Unknown') %>% 
        group_by(country_final) %>% 
        summarize(cnt=n_distinct(id)) %>% 
        arrange(desc(cnt)) %>% 
        head(1) %>% 
        pull(country_final)
      
      
      hd=points_df %>% filter( !is.na(horizontaldistance) ) %>% 
        summarize(mean_hd=round(mean(horizontaldistance,rm.na=T),2) ) %>% 
        pull(mean_hd)
      
      md=points_df %>% filter( !is.na(depth) ) %>% 
        summarize(mean_d=round(mean(depth,rm.na=T),2) ) %>% 
        pull(mean_d)     
      
      min_mg=points_df %>% 
        summarize(min_mg=min(magnitude) ) %>% 
        pull(min_mg)  
      
      max_mg=points_df %>% 
        summarize(max_mg=max(magnitude) ) %>% 
        pull(max_mg)
      
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
      
      tag_box=tags$div(
        style = "background: white; padding: 10px; border-radius: 5px; opacity: 0.8;",
        HTML(paste0(
          "<strong>Statistics ", input$Year[1], "-", input$Year[2], "</strong>",
          "<br>Total Earthquakes: ", total,
          "<br>Average Magnitude: ", mg,
          "<br>Min Magnitude: ", min_mg,
          "<br>Max Magnitude: ", max_mg,
          "<br>Average Depth: ", md,
          "<br>Average Horizontal Distance: ", hd
        )))
      
      leafletProxy("spatial_plot") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        
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
          group = "Earthquake IDs",
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
          overlayGroups = c("Countries", "Earthquake IDs"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        
        addControl(html = tag_box, position = "bottomleft")
    })
    
    
    # -------------------------------
    # 5. EARTHQUAKES PER YEAR
    # -------------------------------
    output$eq_pr_yr <- renderPlotly({
      df <- ind_filtered_data() #filtered_data() 
      req(nrow(df) > 0)

      plot_df <- df %>%
        group_by(year_) %>%
        summarize(
          eq_cnt = n_distinct(id),
          mean_mg = mean(magnitude),
          .groups = "drop"
        )

      fig=ggplot(plot_df, aes(x = year_, y = eq_cnt, fill = mean_mg)) +
        geom_col() +
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(
          title = "Number of Earthquakes per Year",
          y = "# Occurrences",
          x = "Year",
          fill = "Avg Magnitude"
        ) +
        theme_minimal()
        ggplotly(fig)
    })
    
    
    # -------------------------------
    # 6. CONTINENT BUBBLE PLOT
    # -------------------------------
    
    output$con_bb_plot <- renderPlotly({
      df <- bb_filtered_data()
      req(nrow(df) > 0)
      
      plot_df <- df %>%
        filter(!is.na(continent)) %>%
        group_by(continent) %>%
        mutate(cont_occ_sum = n_distinct(id)) %>%
        ungroup()
      
      # Dynamic labels and data based on selection
      x_var <- input$bubble_var_select
      
      # Set labels based on selection
      labels <- list(
        "nst" = list(
          title = "Magnitude vs Seismic Stations (NST)",
          xlab = "Number of Seismic Stations (NST)"
        ),
        "depth" = list(
          title = "Magnitude vs Depth",
          xlab = "Depth (km)"
        ),
        "horizontaldistance" = list(
          title = "Magnitude vs Horizontal Distance",
          xlab = "Horizontal Distance (km)"
        )
      )
      
      fig <- ggplot(plot_df,
                    aes(x = .data[[x_var]], y = magnitude,
                        size = cont_occ_sum,
                        color = continent)) +
        geom_point(alpha = 0.5) +
        scale_size(range = c(1, 5)) +
        labs(
          title = labels[[x_var]]$title,
          y = "Magnitude",
          x = labels[[x_var]]$xlab,
          color = "Continent",
          size = "Total Earthquakes"
        ) +
        theme_minimal()
      
      ggplotly(fig)
    })

    
    output$top_countries_table <- renderDT({
      
      df <- top_5_filtered_data() %>% 
        filter( country_final!='Unknown')
      
      top_countries <- switch(
        input$feature_select,
        
        "frequency" = {
          df %>% group_by(country_final) %>% 
            summarise(cnt=n_distinct(id),mean_mg=mean(magnitude)) %>% 
            arrange(desc(cnt)) %>% 
            head(10) %>% 
            select(country_final,cnt) %>% st_drop_geometry() %>% 
            mutate(Rank=row_number()) %>% 
            rename(country=country_final,`Earthquake Count`=cnt)
          
            
        },
        
        "avg_magnitude" = {
          df %>%  
            group_by(country_final) %>% 
            summarise(mean_mg=  round( mean(magnitude),2)        ) %>% 
            arrange(desc(mean_mg)) %>% 
            head(10) %>% 
            select(country_final,mean_mg) %>% st_drop_geometry() %>% 
            mutate(Rank=row_number()) %>% 
            rename(country=country_final,`Average Magnitude`=mean_mg)
        },
        
        "scale_category" = {
          # Filter by selected scale first
          df %>%
            filter(richter_scale ==input$scale_select) %>%
            group_by(country_final) %>% 
            summarize(n_cnt=n_distinct(id)) %>% 
            arrange(desc(n_cnt)) %>%
            head(10) %>%
            mutate(Rank = row_number()) %>% 
            st_drop_geometry() %>%
            select(Rank, Country = country_final, `Earthquake Count`=n_cnt)
                   #!!paste0(tools::toTitleCase(input$scale_select), " Earthquakes") := Value)
          
        }
      )
      
      datatable(
        top_countries,
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 8,
          scrollX = FALSE,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 0:2)
          )
        ),
        rownames = FALSE,
        selection = 'none',
        class = 'cell-border stripe',
        style = 'bootstrap4'
      ) %>%
        formatStyle(
          'Rank',
          fontWeight = 'bold',
          backgroundColor = '#f8f9fa'
        ) %>%
        formatStyle(
          columns = colnames(top_countries),
          fontSize = '14px'
        )
    })   
    
    output$hrly_dist<-renderPlotly({
      df=joy_filtered_data()
      
      fig=df %>% select(dt_date_time,id,magnitude,richter_scale) %>%
        mutate(
          hr_day = hour(dt_date_time),
          richter_scale = factor(
            richter_scale,
            levels = c(
              'Micro','Minor','Slight','Light',
              'Moderate','Strong','Major',
              'Great','Extreme'
            )
          )
        ) %>% 
        ggplot(aes(x = hr_day,
                   y = richter_scale,
                   fill = richter_scale)) +
        geom_density_ridges() +
        scale_x_continuous(
          #breaks = 0:23,
          limits = c(0, 26)
        ) +
        labs(title = "Distributions of Earthquake Occurances By Hour of Day",
          y = 'Scale',
          x = "Hour of Day") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 10)
        )
      
      ggplotly(fig)
      
      
    })

    
  }






# Run the application 
shinyApp(ui = ui, server = server)