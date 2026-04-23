# Test functions for app
sdf_eq=st_read('sdf_earthquake.gpkg')
sdf_eq_countries=st_read('country_geometries.gpkg') %>% rename(geometry=geom)

country_geom=sdf_eq_countries %>% group_by(country_final) %>% summarize()

ids=sdf_eq %>% select(id,date_final,magnitude,country_final) %>% st_drop_geometry()
id_country_geom=st_as_sf(inner_join(ids,country_geom, by=('country_final')))

#c_plot=id_country_geom %>% mutate(year_=year(date_final)) %>% group_by(country_final,year_,geometry) %>% summarize(mean_mg=mean(magnitude))

world_countries <- ne_countries(scale = "medium", returnclass = "sf")

wc=world_countries %>% select(name,geometry) %>% rename(country_=name)




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


