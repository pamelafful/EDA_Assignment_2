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
library(gapminder)
library(plotly)
library(shinyWidgets)

sdf_eq=st_read('sdf_earthquake.gpkg') %>% filter( country_final !='Unknown')
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
  
  output$spatial_plot <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = sdf_eq$magnitude
    )
    
    if(length(input$Country_Filter)!=0){
      country_map=id_country_geom %>%
        mutate(date_month=floor_date(date_final,unit='month')) %>% 
        mutate(year_=year(date_final)) %>%
        filter(date_month>=input$Year[1] & date_month<=input$Year[2] & country_final %in% input$Country_Filter ) %>%
        group_by(country_final,year_) %>%
        summarize(mean_mg=mean(magnitude))
      
      id_df <- sdf_eq %>% mutate(date_month=floor_date(date_final,unit='month')) %>% 
        filter(date_month>=input$Year[1] & date_month<=input$Year[2] & country_final !='Unknown' & country_final %in% input$Country_Filter)%>% 
        distinct(id, country_final, magnitude, location, 
                 richter_scale, horizontaldistance, geom, depth)
      
    }else{
      country_map=id_country_geom %>%
        mutate(date_month=floor_date(date_final,unit='month')) %>% 
        mutate(year_=year(date_final)) %>%
        filter(date_month>=input$Year[1] & date_month<=input$Year[2] & country_final !='Unknown') %>%
        group_by(country_final,year_) %>%
        summarize(mean_mg=mean(magnitude))
      
      
      id_df <- sdf_eq %>% mutate(date_month=floor_date(date_final,unit='month')) %>% 
        filter(date_month>=input$Year[1] & date_month<=input$Year[2] & country_final !='Unknown')%>% 
        distinct(id, country_final, magnitude, location, 
                 richter_scale, horizontaldistance, geom, depth)
      
    }
    
    
    country_map=st_transform(country_map, 4326)
    #pal = colorNumeric(palette = "YlOrRd", domain = country_map$mean_mg)
    
    labels_c <- lapply(seq_len(nrow(country_map)), function(i) {
      HTML(paste0(
        "Country: ", country_map$country_final[i], "<br/>",
        "Average Magnitude: ", round(country_map$mean_mg[i],2), "<br/>"
      ))
    })
    
    
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
        stroke = TRUE,                     
        weight = 1.5,                     
        fillOpacity = 0.6,
        fillColor = ~pal(magnitude),       
        color = ~darken(pal(magnitude), 0.8), 
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
      
      addLayersControl(
        overlayGroups = c("Countries", "Earthquakes"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  output$eq_pr_yr <- renderPlotly({
    

    fig=sdf_eq %>% filter(country_final %in% input$Country_Filter & richter_scale %in% input$Scale_Filter) %>% 
      mutate(year_=year(date_final)) %>% 
      group_by(year_,country_final) %>% summarize(eq_cnt=n_distinct(id), mean_mg=mean(magnitude)) %>% 
      ggplot(aes(x=year_,y=eq_cnt,fill=mean_mg))+
      geom_col()+
      scale_fill_gradient(low = "yellow", high = "red")+
      labs(title = "Number of Earthquakes per year",y='#occurances',
           x= "year",fill='avg magnitude') +
      theme_minimal()
    
    ggplotly(fig)
  })
  
  
  output$con_bb_plot <- renderPlotly({
    
    bc=sdf_earthquake %>%  mutate(date_month=floor_date(date_final,unit='month')) %>% 
      mutate(year_=year(date_final)) %>%
      filter(date_month>=input$Year[1] & date_month<=input$Year[2] & !is.na(continent) & richter_scale %in% input$Scale_Filter  ) %>% 
      group_by(continent) %>% 
      mutate(cont_occ_sum = n_distinct(id)) %>%
      ungroup() %>%  
      mutate(
        country_final = reorder(continent, -cont_occ_sum)  
      ) %>% 
      ggplot(aes(x = nst, y =magnitude, size = cont_occ_sum,color=continent)) +
      geom_point(alpha = 0.5) +  
      scale_size(range = c(1,5))+
      labs(title = "Avg Magnitude vs Avg Number of Siesmic Stations (NST)  ",y='magnitude',x= "NST",
           color='continent',size='')+
      theme_minimal()
    
    ggplotly(bc)
  })  
  
}







# Run the application 
shinyApp(ui = ui, server = server)
