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
      setView(zoom=12, lat=48.863, lng=2.35) %>%
      addCircles(~ longitude, ~ latitude,
                 layerId=stations_filtrees$number,
                 popup = ~ sprintf("%s<br>%s vélos disponibles",name,as.character(available_bikes)),
                 radius = ~ sqrt(bike_stands)*10,
                 color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                 stroke = TRUE, fillOpacity = 0.75)
    
    if(input$go > 0){
      isolate({
        #acquisition des positions géo des adresses de départ & arrivée + positionnement carte :
        if (input$adresseDepart != "") {
          #adresseDepartGeo <- reactive({geocode(input$adresseDepart)})
          adresseDepartGeo <- geocode(input$adresseDepart)
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
    displaystation <- function(deparr,m) {
      i <- ifelse(deparr=="depart", input$stationDepart, input$stationArrivee)
      if (i != "0") {
        #cat(file=stderr(), "***hello ",deparr,i,"\n")
        m <- addCircles(m,
                          color=ifelse(deparr=="depart","orange","green"),
                          lng=stations[i,]$longitude,
                          lat=stations[i,]$latitude,
                          radius=input$stationsProx,
                          popup=sprintf("station %s :<br>%s<br>%s vélos disponibles",
                                        ifelse(deparr=="depart","de départ","d'arrivée"),
                                        stations[i,]$name,stations[i,]$available_bikes))
      }
      return(m)
    }
    
    map <- displaystation("depart",map)
    map <- displaystation("arrivee",map)
    #})
    
    mapOptions(map, zoomToLimits="first")
    
  })
  

