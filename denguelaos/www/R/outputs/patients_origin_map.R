output$patients_origin_map <- renderLeaflet({
  req(dengue_dta_filt())
  
  leaflet(dengue_dta_filt()) %>% 
    setView(lng = 102.691992723991, lat = 17.9787350677193, zoom = 7) %>%
    addTiles() %>% 
    addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())
})