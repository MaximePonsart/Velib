#m?j les stations de proximit? en fonction des adresses saisies
updStationFromAdresse <- function (adresse, deparr) {
  
  adresseGeo <- geocode(adresse, output="latlona")
  latitude <- adresseGeo[1,"lat"]
  longitude <- adresseGeo[1,"lon"]
  
  if (deparr=="depart")
    icone <- greenLeafIcon
  else
    icone <- redLeafIcon
  
  leafletProxy("carteGeo") %>%
    addMarkers(lng=longitude, lat=latitude,
               icon=icone)
  
  #calcul de la station la plus proche et maj UI :
  s <- getProchesStations(latitude, longitude, "classement", 1)$number
  setMapCircleDeparr(s, deparr)
  updateSelectInput(session,
                    inputId = ifelse(deparr=="depart","stationDepart","stationArrivee"),
                    selected=s)
  #maj l'adresse clean :
  adresseClean <- paste(head(strsplit(adresseGeo[1,"address"],",")[[1]],-1),collapse=",")
  updateTextInput(session,
                  inputId=ifelse(deparr=="depart","adresseDepart","adresseArrivee"),
                  value=adresseClean)
  
  #sauvegarde coordonn?es en variable globale :
  geoAdr <- paste(latitude, longitude, sep=",")
  if (deparr=="depart")
    geoAdrDepart <<- geoAdr
  else
    geoAdrArrivee <<- geoAdr
  
}

#trace sur la carte le cercle de la stations de d�part ou d'arriv�e
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
  #màj météo
  getMeteo(dtTrajet)
  
  #coming very soon déclenchement du traitement
  #goCalcTrajet()
})

observeEvent(input$goCtrlDep,{
  if(input$goCtrlDep > 0){
    if (input$adresseDepart != "") {
      updStationFromAdresse(input$adresseDepart,"depart")
    }
  }
})

observeEvent(input$goCtrlArr,{
  if(input$goCtrlArr > 0){
    #acquisition de la position géo + positionnement carte :
    if (input$adresseArrivee != "") {
      updStationFromAdresse(input$adresseArrivee,"arrivee")
    }
  }
})

observeEvent(input$horizon,{
  dtTrajet <<- Sys.time() + as.numeric(input$horizon)*60
})