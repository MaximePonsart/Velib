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
                        paste0("Station de départ retenue : (", stationDepTrajet, ") ", stations[stationDepTrajet,]$name),
                        paste0("Pas de station de départ")),"\n")
  s <- paste0(s, ifelse(!is.na(stationArrTrajet),
                        paste0("Station d'arrivée retenue : (", stationArrTrajet, ") ", stations[stationArrTrajet,]$name),
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

#restitution matrice
output$matrix <- renderUI({
  
  input$go
  if(is.na(dfParcours) || is.na(dureeTotale))
    dureeTotale <- matrix(nrow=5, ncol=5)
  
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  M <- print(xtable(dureeTotale, align=rep("c", ncol(dureeTotale)+1)), 
             floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE, include.rownames=T,
             sanitize.colnames.function=bold,
             sanitize.rownames.function=bold
  )
  html <- paste0("$$", M, "$$")
  list(
    withMathJax(HTML(html))
  )  

  
})


#restitution tableau des parcours
output$tabParcours <- DT::renderDataTable(options=list(paging=F, searching=F, pageLength=7, ordering=F, fixedHeader.footer=F, 
                                                       bInfo=F, columnDefs = list(list(className = 'dt-center', targets = c(1,3:5))))
                                          , rownames=c("départ","marcher vers station","prendre vélo","trajet vélo", "déposer vélo","marcher vers destination","arrivée")
                                          ,{
  input$go
                                            
  if (!is.na(dfParcours)) {
    
    res <- dfParcours[dfParcours$idparcours==input$choixParcours,
                      c("dateheure","libEmplacement","duree","available_bikes","available_bike_stands")]
    
    res$duree <- sapply(res$duree, FUN=function(x){ifelse(is.na(x),"",paste0(trunc(x)," min ",trunc((x-floor(x))*60)," s"))})
    res$dateheure <- strftime(res$dateheure, format="%Hh %M")
    
    res <- setNames(res, c("heure","emplacement","durée","vélos dispo","stands dispo"))
    return(res)
    
  }
    
})

