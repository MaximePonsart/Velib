#initialisation de la carte géo restituant les données Velib et configurée par les paramètres de l'IHM :
  output$carteGeo <- renderLeaflet({
    
    #filtrage des stations suivant leur statut d'ouverture :
    if (input$allstation == "all")
      stations_filtrees <- stations
    else
      stations_filtrees <- stations[stations$available_bikes > 0,]
    
    #initialisation de la carte géo :
    map <- leaflet(data = stations_filtrees, height="100%") %>%
      addTiles() %>%
      setView(zoom=12, lat=48.90, lng=2.33) %>%
      addCircles(~ longitude, ~ latitude, popup = ~ sprintf("%s<br>%s vélos disponibles",name,as.character(available_bikes)),
                 radius = ~ sqrt(bike_stands),
                 color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                 stroke = TRUE, fillOpacity = 0.75)
    
    if(input$go > 0){
      isolate({
        #acquisition des positions géo des adresses de départ & arrivée + positionnement carte :
        if (input$adresseDepart != "") {
          #adresseDepartGeo <- reactive({geocode(input$adresseDepart)})
          adresseDepartGeo <- geocode(input$adresseDepart)
          #cat(file=stderr(), paste("hello","\n",adresseDepartGeo,"\n",sep="*"))
          map <- addMarkers(map, lng=adresseDepartGeo[1,"lon"], lat=adresseDepartGeo[1,"lat"], popup="départ")
        }
        if (input$adresseArrivee != "") {
          adresseArriveeGeo <- geocode(input$adresseArrivee)
          map <- addMarkers(map, lng=adresseArriveeGeo[1,"lon"], lat=adresseArriveeGeo[1,"lat"], popup="arrivée")
        }
      })
    }
    
    #positionnement sur la carte des stations sélectionnées :
    #isolate({
      if (input$stationDepart != "0") {
        map <- addCircleMarkers(map,
                          color="orange",
                          lng=stations[input$stationDepart,]$longitude,
                          lat=stations[input$stationDepart,]$latitude,
                          popup=sprintf("station de départ :<br>%s<br>%s vélos disponibles",stations[input$stationDepart,]$name,stations[input$stationDepart,]$available_bikes))
      }
      if (input$stationArrivee != "0") {
        map <- addCircleMarkers(map,
                          color="green",
                          lng=stations[input$stationArrivee,]$longitude,
                          lat=stations[input$stationArrivee,]$latitude,
                          popup=sprintf("station d'arrivée :<br>%s<br>%s vélos disponibles",stations[input$stationArrivee,]$name,stations[input$stationArrivee,]$available_bikes))
      }
    #})
    
    mapOptions(map, zoomToLimits="first")
    
  })
  

