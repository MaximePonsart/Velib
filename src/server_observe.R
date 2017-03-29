
# trace le trajet du parcours sur la carte
# df : data frame calculé par l'API Google Direction
traceTrajet <- function () {
  
  geoStaDepTrajet <<- stations[stationDepTrajet,]$position
  geoStaArrTrajet <<- stations[stationArrTrajet,]$position
  
  # appel de l'API
  df_marche_dep <- setGoogleTrajet(geoAdrDepart, geoStaDepTrajet, "walking")
  df_velo <- setGoogleTrajet(geoStaDepTrajet, geoStaArrTrajet, "bicycling")
  df_marche_arr <- setGoogleTrajet(geoStaArrTrajet, geoAdrArrivee, "walking")
  
  leafletProxy("carteGeo") %>%
    addPolylines(data = df_marche_dep, lat = ~lat, lng = ~lon, layerId="walk_dep") %>%
    addPolylines(data = df_velo, lat = ~lat, lng = ~lon, layerId="bicycle") %>%
    addPolylines(data = df_marche_arr, lat = ~lat, lng = ~lon, layerId="walk_arr")
  
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
  if (deparr=="depart") {
    stationDepSel <<- NA
    if (input$stationDepart!="0")
      updateSelectInput(session, inputId ="stationDepart", selected=0)
  }
  else { #arrivée
    stationArrSel <<- NA
    if (input$stationArrivee!="0")
      updateSelectInput(session, inputId ="stationArrivee", selected=0)
  }
  setMapCircleDeparr(stations[s,]$position, deparr)
  
  #maj l'adresse clean :
  adresseClean <- paste(head(strsplit(adresseGeo[1,"address"],",")[[1]],-1),collapse=",")
  updateTextInput(session,
                  inputId=ifelse(deparr=="depart","adresseDepart","adresseArrivee"),
                  value=adresseClean)
  
  #sauvegarde coordonnées et adresses en variable globale :
  geoAdr <- paste(latitude, longitude, sep=",")
  if (deparr=="depart") {
    geoAdrDepart <<- geoAdr
    adrDepart <<- adresseClean
  }
  else {
    geoAdrArrivee <<- geoAdr
    adrArrivee <<- adresseClean
  }
  
}

#trace sur la carte le cercle de la station de départ ou d'arrivée
setMapCircleDeparr <- function (geo, deparr) {
  leafletProxy("carteGeo") %>%
    addCircles(
      color=ifelse(deparr=="depart","darkviolet","green"),
      lng=getLon(geo),
      lat=getLat(geo),
      layerId=as.vector(deparr),
      radius=input$stationsProx)
}


observeEvent(input$carteGeo_shape_click,{
  
  print("***1")
  event <- input$carteGeo_shape_click
  
  eventIsArrivee <- event$id==input$stationArrivee || event$id=="arrivee"
  
  cible <- ifelse(eventIsArrivee, input$stationArrivee, event$id)
  setMapCircleDeparr(stations[cible,]$position, "depart")
  updateSelectInput(session, inputId = "stationDepart", selected=cible)
  stationDepSel <<- cible
  
  if (eventIsArrivee) {
    leafletProxy("carteGeo") %>%
      removeShape("arrivee")
    updateSelectInput(session, inputId = "stationArrivee", selected="0")
  }
  
})

observeEvent(input$stationsProx,{
  print("***2")
  if (input$stationDepart != "0")
    setMapCircleDeparr(stations[input$stationDepart,]$position, "depart")
  else
    if (input$adresseDepart != "")
      setMapCircleDeparr(input$adresseDepart, "depart")
  
  if (input$stationArrivee != "0")
    setMapCircleDeparr(stations[input$stationArrivee,]$position, "arrivee")
  else
    if (input$adresseArrivee != "")
      setMapCircleDeparr(input$adresseArrivee, "arrivee")
})

observeEvent(input$stationDepart,{
  print("***3")
  if (input$stationDepart != "0") {
    setMapCircleDeparr(stations[input$stationDepart,]$position, "depart")
    stationDepSel <<- input$stationDepart
    geoAdrDepart <<- NA
    updateTextInput(session, inputId = "adresseDepart", value="")
  }
})

observeEvent(input$stationArrivee,{
  print("***4")
  if (input$stationArrivee != "0") {
    setMapCircleDeparr(stations[input$stationArrivee,]$position, "arrivee")
    stationArrSel <<- input$stationArrivee
    geoAdrArrivee <<- NA
    updateTextInput(session, inputId = "adresseArrivee", value="")
  }
})

observeEvent(input$go,{
  
  #lancement du traitement si départ et arrivée sont renseignés
  if ((!is.na(stationDepSel) || !is.na(geoAdrDepart)) && (!is.na(stationArrSel) || !is.na(geoAdrArrivee))) {
    
    #màj météo
    #getMeteo(dtTrajet)
    
    #calcul du parcours optimal
    goCalcTrajet()
    
    #màj les stations de départ et d'arrivée sur la carte
    setMapCircleDeparr(stations[stationDepTrajet,]$position, "depart")
    setMapCircleDeparr(stations[stationArrTrajet,]$position, "arrivee")
    
    #tracé du trajet sur la carte :
    traceTrajet()
    
    #initialise l'onglet détail de parcours :
    choix <- unique(dfParcours[order(dfParcours$dureeparcours),c("idparcours", "dureeparcours")])
    #choixLib <- paste0("[",choix$idparcours,"] en ",
    choixLib <- paste0("en ",
                       sapply(choix$dureeparcours, FUN=function(x){paste0(trunc(x)," min ",trunc((x-floor(x))*60)," s")}))
    choixLib[1]<-paste0(choixLib[1]," (le +rapide)")
    updateSelectInput(session, inputId ="choixParcours",
                      selected=unique(dfParcours[which(dfParcours$dureeparcours==min(dfParcours$dureeparcours)),]$idparcours),
                      choices=setNames(choix$idparcours,choixLib))
    
  }
  
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

observeEvent(input$modele_stat, {
  modele <<- input$modele_stat
})


