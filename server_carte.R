
  output$carteGeo <- renderLeaflet({
    
    if(input$go > 0){

      isolate({

        leaflet(data = stations, height="100%") %>%
          addTiles() %>%
          addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),
                     radius = ~ sqrt(bike_stands),
                     color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                     stroke = TRUE, fillOpacity = 0.75)
        

      })
    }
    
  })
  

