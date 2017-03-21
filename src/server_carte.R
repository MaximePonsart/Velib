#icônes des points de départ/arrivée géo :

greenLeafIcon <- makeIcon(
  iconUrl = "img/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)

redLeafIcon <- makeIcon(
  iconUrl = "img/leaf-red.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)

#initialisation de la carte géo restituant les données Velib et configurée par les paramètres de l'IHM :
  output$carteGeo <- renderLeaflet({
    
    s_ouvertes <- stations[stations$status=="OPEN",]
    
    #initialisation de la carte géo :
    map <- leaflet(data = s_ouvertes, height="100%") %>%
      addTiles() %>%
      setView(zoom=12, lat=getLat(geoParis), lng=getLon(geoParis)) %>%
      addCircles(data=s_ouvertes[s_ouvertes$available_bikes>0,],
                 lng=~longitude,
                 lat=~latitude,
                 group="vélos disponibles",
                 layerId=~number,
                 popup = ~ sprintf("%s<br>%s vélos disponibles",name,as.character(available_bikes)),
                 radius = ~ sqrt(bike_stands)*10,
                 color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
                 stroke = TRUE, fillOpacity = 0.75) %>%
      addCircles(data=s_ouvertes[s_ouvertes$available_bikes==0,],
                 lng=~longitude,
                 lat=~latitude,
                 group="vélos non disponibles",
                 layerId=~number,
                 popup = ~ sprintf("%s<br>0 vélo disponible",name),
                 radius = ~ sqrt(bike_stands)*10,
                 color = ColorPal(0),
                 stroke = TRUE, fillOpacity = 0.75) %>%      
      addLayersControl(
        overlayGroups = c("vélos disponibles", "vélos non disponibles"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("vélos non disponibles") %>%
      addLegend(pal = ColorPal,
                values = ~(available_bikes / (available_bikes + available_bike_stands)),
                labFormat = labelFormat(transform = function(x) 100 * x),
                title = "% disponibilité",
                opacity = 1)
    
    mapOptions(map, zoomToLimits="first")
    
  })
  

