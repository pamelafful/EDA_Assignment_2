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
    "Average Magnitude: ", round(country_map$mean_mg[i]), "<br/>"
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




