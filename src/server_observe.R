
# trace le trajet du parcours sur la carte
# df : data frame calculé par l'API Google Direction
traceTrajet <- function () {
  
  geoStaDepTrajet <- stations[stationDepTrajet,]$position
  geoStaArrTrajet <- stations[stationArrTrajet,]$position
  
  # appel de l'API
  df_marche_dep <- setGoogleTrajet(geoAdrDepart, geoStaDepTrajet, "walking")
  df_velo <- setGoogleTrajet(geoStaDepTrajet, geoStaArrTrajet, "bicycling")
  df_marche_arr <- setGoogleTrajet(geoStaArrTrajet, geoAdrArrivee, "walking")
  
  leafletProxy("carteGeo") %>%
    addPolylines(data = df_marche_dep, lat = ~lat, lng = ~lon) %>%
    addPolylines(data = df_velo, lat = ~lat, lng = ~lon) %>%
    addPolylines(data = df_marche_arr, lat = ~lat, lng = ~lon)
  
}

#màj les stations de proximité en fonction des adresses saisies
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
  
  #sauvegarde coordonnées en variable globale :
  geoAdr <- paste(latitude, longitude, sep=",")
  if (deparr=="depart")
    geoAdrDepart <<- geoAdr
  else
    geoAdrArrivee <<- geoAdr
  
}

#trace sur la carte le cercle de la stations de départ ou d'arrivée
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
  
  #calcul du parcours optimal
  goCalcTrajet()
  
  #màj les stations de départ et d'arrivée sur la carte
  setMapCircleDeparr(stationDepTrajet, "depart")
  setMapCircleDeparr(stationArrTrajet, "arrivee")
  
  #tracé du trajet sur la carte :
  traceTrajet()
  
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
  dtTrajet <<- as.POSIXct(paste0(dateSimulee, heureSimulee)) + as.numeric(input$horizon)*60
})

observeEvent(input$dateSimulee,{
  dateSimulee <<- as.Date(input$dateSimulee)
})

observeEvent(input$hSimulee, {
  heureSimulee <<- paste0(input$hSimulee,":",strsplit(heureSimulee,":")[[1]][2])
  dtTrajet <<- as.POSIXct(paste0(dateSimulee, heureSimulee))
})

observeEvent(input$mSimulee, {
  heureSimulee <<- paste0(strsplit(heureSimulee,":")[[1]][1],":",input$mSimulee)
  dtTrajet <<- as.POSIXct(paste0(dateSimulee, heureSimulee))
})

