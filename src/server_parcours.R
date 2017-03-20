#initialise le texte descriptif de départ/arrivée :
setTextDepArr <- function(deparr) {
  if (deparr=="depart") {
    adresse <- input$adresseDepart
    station <- input$stationDepart
    noChoix <- "le départ"
  } else {
    adresse <- input$adresseArrivee
    station <- input$stationArrivee
    noChoix <- "l'arrivée"
  }
  s <- ""
  if (input$adresseDepart !="")
    s <- isolate(paste("(adresse)", adresse, sep=" "))
  if (input$stationDepart !="0")
    s <- paste(s,
               paste("(station)", stations[station,]$name, sep=" "),
               sep=ifelse(s=="","","\n"))
  s <- ifelse(s!="", s, paste0("pas de choix pour le ", noChoix))
  return(s)
}

output$parcoursDepart <- renderText({
  # if (input$stationDepart != "0" && input$stationArrivee != "0")
  #     getDistanceGeoStations(input$stationDepart, input$stationArrivee)
  input$goCtrlDep
  return(setTextDepArr("depart"))
    
})

output$parcoursArrivee <- renderText({
  input$goCtrlArr
  return(setTextDepArr("arrivee"))
})

output$parcoursDetail <- renderText({
  #d <- gdist.total(rbind(rev(getLatLon(geoAdrDepart)), (stations[input$stationDepart,]$position)
  #s <- paste("Distance géo entre adresse et station de départ ", )
})

output$parcoursDateHeure <- renderText({
  input$horizon
  s <- "Heure de départ : "
  s <- paste0(s, dtTrajet)
})