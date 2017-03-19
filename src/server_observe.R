updStationFromAdresse <- function (adresse, deparr) {
  adresseGeo <- geocode(adresse)
  latitude <- adresseGeo[1,"lat"]
  longitude <- adresseGeo[1,"lon"]
  if (deparr=="depart")
    icone <- greenLeafIcon
  else
    icone <- redLeafIcon
  leafletProxy("carteGeo") %>%
    addMarkers(lng=longitude, lat=latitude,
               icon=icone)
  #calcul de la station la plus proche et màj UI :
  s <- getProchesStations(latitude, longitude, "classement", 1)$number
  print("!!!!!!")
  print(s)
  setMapCircleDeparr(s, deparr)
  updateSelectInput(session,
                    inputId = ifelse(deparr=="depart","stationDepart","stationArrivee"),
                    selected=s)
}

setMapCircleDeparr <- function (idStation, deparr) {
  leafletProxy("carteGeo") %>%
    addCircles(
      color=ifelse(deparr=="depart","orange","green"),
      lng=stations[idStation,]$longitude,
      lat=stations[idStation,]$latitude,
      layerId=as.vector(deparr),
      radius=input$stationsProx)
}


observeEvent(input$carteGeo_shape_click,{
  
  print("***1")
  event <- input$carteGeo_shape_click
  
  eventIsArrivee <- event$id==input$stationArrivee || event$id=="arrivee"
  
  cible <- ifelse(eventIsArrivee, input$stationArrivee, event$id)
  setMapCircleDeparr(cible, "depart")
  updateSelectInput(session, inputId = "stationDepart", selected=cible)
  
  if (eventIsArrivee) {
    leafletProxy("carteGeo") %>%
      removeShape("arrivee")
    updateSelectInput(session, inputId = "stationArrivee", selected="0")
  }
  
})

observeEvent(input$stationsProx,{
  print("***2")
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart, "depart")
  if (input$stationArrivee != "0") setMapCircleDeparr(input$stationArrivee, "arrivee")
})

observeEvent(input$stationDepart,{
  print("***3")
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart, "depart")
})

observeEvent(input$stationArrivee,{
  print("***4")
  if (input$stationArrivee != "0") setMapCircleDeparr(input$stationArrivee, "arrivee")
})

observeEvent(input$go,{
  #pour plus tard (déclenchement du calcul du parcours)
})

observeEvent(input$goCtrlDep,{
  if(input$goCtrlDep > 0){
    if (input$adresseDepart != "") {
      updStationFromAdresse(input$adresseDepart,"depart")
      # #acquisition de la position géo + positionnement carte :
      # adresseDepartGeo <- geocode(input$adresseDepart)
      # latitude <- adresseDepartGeo[1,"lat"]
      # longitude <- adresseDepartGeo[1,"lon"]
      # leafletProxy("carteGeo") %>%
      #   addMarkers(lng=longitude, lat=latitude,
      #              icon=greenLeafIcon)
      # #calcul de la station la plus proche et màj UI :
      # s <- getProchesStations(latitude, longitude, "classement", 1)
      # print("!!!!!!")
      # print(s)
      # setMapCircleDeparr(s$number, "depart")
      # updateSelectInput(session, inputId = "stationDepart", selected=s$number)
    }
  }
})

observeEvent(input$goCtrlArr,{
  if(input$goCtrlArr > 0){
    #acquisition de la position géo + positionnement carte :
    if (input$adresseArrivee != "") {
      updStationFromAdresse(input$adresseArrivee,"arrivee")
    #   adresseArriveeGeo <- geocode(input$adresseArrivee)
    #   leafletProxy("carteGeo") %>%
    #     addMarkers(
    #       lng=adresseArriveeGeo[1,"lon"], lat=adresseArriveeGeo[1,"lat"],
    #       icon=redLeafIcon)
    }
  }
})