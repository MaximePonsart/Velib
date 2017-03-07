
  output$carteGeo <- renderLeaflet({
    
    #hist(stations$available_bikes)
    
    # le code s'éxécute quand je clique sur le bouton
    
    if(input$go > 0){
      # code isole
      isolate({

        leaflet(data = stations, height="100%") %>%
          addTiles() %>%
          addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),
                     radius = ~ sqrt(bike_stands),
                     color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                     stroke = TRUE, fillOpacity = 0.75)
        
        #m
                
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, input$var]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = input$couleur, border = 'white')
      })
    }
    
  })
  

