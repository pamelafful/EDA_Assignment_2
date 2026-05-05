# Test functions for app
sdf_eq=st_read('sdf_earthquake.gpkg')
sdf_eq_countries=st_read('country_geometries.gpkg') %>% rename(geometry=geom)

sdf_eq <- readRDS("sdf_eq.rds")
sdf_eq_countries <- readRDS("sdf_eq_countries.rds")

names(sdf_eq_countries)

country_geom=sdf_eq_countries %>% group_by(country_final) %>% summarize()

ids=sdf_eq %>% select(id,date_final,magnitude,country_final) %>% st_drop_geometry()
id_country_geom=st_as_sf(inner_join(ids,country_geom, by=('country_final')))

#c_plot=id_country_geom %>% mutate(year_=year(date_final)) %>% group_by(country_final,year_,geometry) %>% summarize(mean_mg=mean(magnitude))

world_countries <- ne_countries(scale = "medium", returnclass = "sf")

wc=world_countries %>% select(name,geometry) %>% rename(country_=name)

library(lubridate)

start = min(floor_date(sdf_eq$date_final,unit='month'))
end = max(floor_date(sdf_eq$date_final,unit='month'))
min = min(month(sdf_eq$date_final))
max = max(month(sdf_eq$date_final))
sdf_eq %>% mutate(yr=floor_date(date_final,unit='month')) %>% pull(yr)

country_options=sdf_eq %>% distinct(country_final) %>% pull()
scale_options=sdf_eq %>% distinct(richter_scale) %>% pull()
continent_options=sdf_eq %>% distinct(continent) %>% pull()
input=list(Year=c(start ,end),Country_Filter=country_options, Scale_Filter=scale_options,Continent_Filter=continent_options)



country_map=id_country_geom %>% 
  mutate(year_=year(date_final)) %>% 
  filter(year_==2020 &country_final!='Unknown') %>% 
  group_by(country_final,year_) %>% 
  summarize(mean_mg=mean(magnitude))

country_map=st_transform(country_map, 4326) 
pal = colorNumeric(palette = "YlOrRd", domain = country_map$mean_mg)



labels <- lapply(seq_len(nrow(country_map)), function(i) {
  HTML(paste0(
    "Country: ", country_map$country_final[i], "<br/>",
    "Average Magnitude: ", round(country_map$mean_mg[i],2), "<br/>"
  ))
})

leaflet(country_map) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(mean_mg),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = labels # Tooltip on hover
  ) %>%
  addLegend(pal = pal, 
            values = ~mean_mg, 
            opacity = 0.7, 
            title = "Average Magnitude", 
            position = "bottomright")

###################################
country_map=id_country_geom %>% 
  mutate(year_=year(date_final)) %>% 
  filter(year_==2020 &country_final!='Unknown') %>% 
  group_by(country_final,year_) %>% 
  summarize(mean_mg=mean(magnitude))

country_map=st_transform(country_map, 4326) 
pal = colorNumeric(palette = "YlOrRd", domain = country_map$mean_mg)


library(leaflet)
library(sf)

# Palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = sdf_eq$magnitude
)

labels_c <- lapply(seq_len(nrow(country_map)), function(i) {
  HTML(paste0(
    "Country: ", country_map$country_final[i], "<br/>",
    "Average Magnitude: ", round(country_map$mean_mg[i],2), "<br/>"
  ))
})


darken <- function(col, factor = 0.8) {
  rgb_vals <- grDevices::col2rgb(col) / 255
  rgb(
    rgb_vals[1, ] * factor,
    rgb_vals[2, ] * factor,
    rgb_vals[3, ] * factor
  )
}

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

id_df <- sdf_eq %>% 
  filter(year(date_final) == 2020) %>% 
  distinct(id, country_final, magnitude, location, 
           richter_scale, horizontaldistance, geom, depth)
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
  
  # 📊 Legend (IMPORTANT: explicit values)
  addLegend(
    pal = pal,
    values = country_map$mean_mg,
    title = "Average Magnitude",
    position = "bottomright"
  ) %>%
  setView(lng =18.2812 , lat = 9.1021, zoom = 2) %>% 
  
  # 🎛 Layer control (this is what makes it properly "layered")
  addLayersControl(
    overlayGroups = c("Countries", "Earthquakes"),
    options = layersControlOptions(collapsed = FALSE)
  )

###############
library(ggplot2)

# Sample Data
df <- data.frame(
  x = rnorm(50), 
  y = rnorm(50), 
  size_var = runif(50, 1, 20), 
  group = sample(c("A", "B"), 50, replace = TRUE)
)

# Basic Bubble Plot
ggplot(df, aes(x = x, y = y, size = size_var, color = group)) +
  geom_point(alpha = 0.5) +  # Alpha adds transparency to handle overlapping
  scale_size(range = c(1, 15))

######countor line plot test####

library(ggplot2)

# Basic example using the built-in volcano dataset
df <- as.data.frame(as.table(volcano))
colnames(df) <- c("x", "y", "z")
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)

ggplot(df, aes(x, y, z = z)) +
  geom_contour_filled()





input.Year=c(start ,end)

input.Country_Filter=c('Afghanistan')

country_map=id_country_geom %>%
  mutate(date_month=floor_date(date_final,unit='month')) %>% 
  mutate(year_=year(date_final)) %>%
  filter(date_month>=input.Year[1] & date_month<=input.Year[2] & country_final %in% input.Country_Filter & country_final !='Unknown') %>%
  group_by(country_final,year_) %>%
  summarize(mean_mg=mean(magnitude))

id_df <- sdf_eq %>% mutate(date_month=floor_date(date_final,unit='month')) %>% 
  filter(date_month>=input.Year[1] & date_month<=input.Year[2] & country_final !='Unknown')%>% 
  
  #filter(year(date_final) == input$Year) 
  distinct(id, country_final, magnitude, location, 
           richter_scale, horizontaldistance, geom, depth)





country_options=sdf_eq %>% distinct(country_final) %>% pull()
scale_options=sdf_eq %>% distinct(richter_scale) %>% pull()
continent_options=sdf_eq %>% distinct(continent) %>% pull()
input=list(Year=c(start ,end),Country_Filter=country_options, Scale_Filter=scale_options,Continent_Filter=continent_options)


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



ids_df <- sdf_eq %>% mutate(date_month=floor_date(date_final,unit='month'),year_=year(date_final)) %>% 
  select(id, date_final, date_month, year_, magnitude, country_final, 
         continent, richter_scale, location, horizontaldistance, depth,nst) %>% 
  st_drop_geometry()

sdf_eq_spatial <- sdf_eq %>%
  select(id, magnitude, geom)

filtered_data=ids_df %>%
  filter(
    date_month >= input$Year[1],
    date_month <= input$Year[2],
    country_final %in% input$Country_Filter,
    richter_scale %in% input$Scale_Filter
  )


filtered_ids <- filtered_data %>% distinct(id)
id_spatial <- sdf_eq_spatial %>%
  filter(id %in% filtered_ids$id)

ids_df %>%
  filter(
    date_month >= input$Year[1],
    date_month <= input$Year[2],
    country_final %in% input$Country_Filter,
    richter_scale %in% input$Scale_Filter
  ) %>%
  inner_join(st_drop_geometry(id_spatial), by = c("magnitude","id")) %>% 
  select(id, country_final, magnitude, location, richter_scale, 
         horizontaldistance, depth) %>%
  inner_join(id_spatial, by = c("id", "magnitude"))

















sdf_eq %>%mutate(
  date_month = floor_date(date_final, unit = "month"),
  year_ = year(date_final)
) %>% 
  filter(
    date_month >= input$Year[1],
    date_month <= input$Year[2],
    country_final %in% input$Country_Filter,
    richter_scale %in% input$Scale_Filter) %>% 
  st_drop_geometry() %>%
  group_by(country_final) %>%
  summarize(
    mean_mg = mean(magnitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(country_geom, by = "country_final") %>%
  st_as_sf()


country_geom=sdf_eq_countries %>% group_by(country_final) %>% summarize()

ids=sdf_eq %>% select(id,date_final,magnitude,country_final,continent) %>% st_drop_geometry()
id_country_geom=st_as_sf(inner_join(ids,country_geom, by=('country_final')))
