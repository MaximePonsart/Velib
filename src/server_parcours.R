#initialise le texte descriptif de d�part/arriv�e :
setTextDepArr <- function(deparr) {
  if (deparr=="depart") {
    adresse <- input$adresseDepart
    station <- input$stationDepart
    noChoix <- "le d�part"
    geo <- geoAdrDepart
  } else {
    adresse <- input$adresseArrivee
    station <- input$stationArrivee
    noChoix <- "l'arriv�e"
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
                        paste0("Station de d�part retenue : (", stationDepTrajet, ") ", stations[stationDepTrajet,]$name),
                        paste0("Pas de station de d�part")),"\n")
  s <- paste0(s, ifelse(!is.na(stationArrTrajet),
                        paste0("Station d'arriv�e retenue : (", stationArrTrajet, ") ", stations[stationArrTrajet,]$name),
                        paste0("Pas de station d'arriv�e")),"\n")
  s <- paste0(s, "Dur�e du parcours : ", ifelse(is.na(dureeTrajet), "-", paste0(dureeTrajet, " min")),"\n")
  
})

output$parcoursDateHeure <- renderText({
  input$go
  isolate({
    s <- "Heure de d�part : "
    s <- paste0(s, dtTrajet)
  })
  return(s)
})

output$parcoursMeteo <- renderText({
  input$go
  s <- ""
  s <- paste0(s, "Temp�rature : ", meteoTemperature, "\n")
  s <- paste0(s, "Pr�cipitations : ", meteoPrecipitations, "\n")
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
                                          , rownames=c("d�part","marcher vers station","prendre v�lo","trajet v�lo", "d�poser v�lo","marcher vers destination","arriv�e")
                                          ,{
  input$go
                                            
  if (!is.na(dfParcours)) {
    
    res <- dfParcours[dfParcours$idparcours==input$choixParcours,
                      c("dateheure","libEmplacement","duree","available_bikes","available_bike_stands")]
    
    res$duree <- sapply(res$duree, FUN=function(x){ifelse(is.na(x),"",paste0(trunc(x)," min ",trunc((x-floor(x))*60)," s"))})
    res$dateheure <- strftime(res$dateheure, format="%Hh %M")
    
    res <- setNames(res, c("heure","emplacement","dur�e","v�los dispo","stands dispo"))
    return(res)
    
  }
    
})

