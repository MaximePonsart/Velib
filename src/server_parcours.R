#initialise le texte descriptif de départ/arrivée :
setTextDepArr <- function(deparr) {
  if (deparr=="depart") {
    adresse <- input$adresseDepart
    station <- input$stationDepart
    noChoix <- "le départ"
    geo <- geoAdrDepart
  } else {
    adresse <- input$adresseArrivee
    station <- input$stationArrivee
    noChoix <- "l'arrivée"
    geo <- geoAdrArrivee
  }
  s <- ""
  if (adresse !="")
    s <- isolate(paste("(adresse)", adresse, "[",geo,"]", sep=" "))
  if (station !="0")
    s <- paste(s,
               paste("(station)", stations[station,]$name, sep=" "),
               sep=ifelse(s=="","","\n"))
  s <- ifelse(s!="", s, paste0("pas de choix pour ", noChoix))
  return(s)
}

output$parcoursDepart <- renderText({
  input$goCtrlDep
  return(setTextDepArr("depart"))
})

output$parcoursArrivee <- renderText({
  input$goCtrlArr
  return(setTextDepArr("arrivee"))
})

output$parcoursDetail <- renderText({
  input$go
  s <- ""
  s <- paste0(s, ifelse(!is.na(stationDepTrajet),
                        paste0("Station de départ retenue : ", stations[stationDepTrajet,]$name),
                        paste0("Pas de station de départ")),"\n")
  s <- paste0(s, ifelse(!is.na(stationArrTrajet),
                        paste0("Station d'arrivée retenue : ", stations[stationArrTrajet,]$name),
                        paste0("Pas de station d'arrivée")),"\n")
  s <- paste0(s, "Durée du parcours : ", ifelse(is.na(dureeTrajet), "-", paste0(dureeTrajet, " min")),"\n")
  
})

output$parcoursDateHeure <- renderText({
  input$go
  isolate({
    s <- "Heure de départ : "
    s <- paste0(s, dtTrajet)
  })
  return(s)
})

output$parcoursMeteo <- renderText({
  input$go
  s <- ""
  s <- paste0(s, "Température : ", meteoTemperature, "\n")
  s <- paste0(s, "Précipitations : ", meteoPrecipitations, "\n")
  return(s)
})