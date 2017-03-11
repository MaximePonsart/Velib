
  output$carteGeo <- renderLeaflet({
    
    if (input$allstation == "all")
      stations_filtrees <- stations
    else
      stations_filtrees <- stations[stations$available_bikes > 0,]
    
    map <- leaflet(data = stations_filtrees, height="100%") %>%
      addTiles() %>%
      addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Velos disponibles : %s</b>",as.character(available_bikes)),
                 radius = ~ sqrt(bike_stands),
                 color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                 stroke = TRUE, fillOpacity = 0.75)
    
    mapOptions(map, zoomToLimits="first")
    
    
    #if(input$go > 0){
    #  isolate({
    #  })
    #}
    
  })
  

