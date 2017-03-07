# renvoit la dernière disponibilité connue (temps réel) des stations Velib
# entrée : rien
# sortie : le dataframe contenant les données
getDispoStationTR <- function() {

  DecauxKey <- "78b0313157762db775e02e176699871287378746"
  DecauxContractName <- "Paris"
  
  UrlDecaux <-  paste("https://api.jcdecaux.com/vls/v1/stations?contract=",DecauxContractName,"&apiKey=",DecauxKey,sep="")
  
  library(jsonlite)
  library(RCurl)
  library(curl)
  
  #récupération des données temps réel
  stations_dl <- jsonlite::fromJSON(UrlDecaux)
  
  #suppression des variables inutiles
  stations_dl$name <- NULL
  stations_dl$address <- NULL
  stations_dl$bonus <- NULL
  
  return(stations_dl)
  
}
