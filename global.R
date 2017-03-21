#### configuration globale


#chargement des packages requis
source("src/loadPackages.R", local = TRUE)$value
loadPackages(c("leaflet","shiny","shinythemes","Imap","ggmap","placement","geosphere","darksky"))

#options diverses
options(shiny.trace=TRUE)
options(xtable.include.rownames=F)

#inclusion de fonctions génériques
source("src/getDispoStationTR.R", local = TRUE)$value
source("src/calcDistanceStations.R", local = TRUE)$value

#----------------------------------------------------------------------------------------
# initialisation des variables constantes

#palette pour la carte
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

#API key DarkSky et variables météo (précipitations, température)
apiKeyDarksky <- "9778cdc6ddc2eaf7b6854ad412c21eec"
meteoPrecipitations <<- NULL
meteoTemperature <<- NULL

#Coordonnées de Paris
geoParis <- "48.863,2.35"

#fichier jeu de données pour les dispos Vélib
fStation <- "data/stations-velib-disponibilites-en-temps-reel.csv"

#fichier liste des monuments de Paris
fMonuments <- "data/monuments_paris.txt"

#fichier matrice des distances stations
fMatDistanceStation <- "data/mDistanceStation.Rda"

#data frame des stations
stations <- NULL

#vecteur des noms des stations actives
stations_actives_nom <- NULL

#----------------------------------------------------------------------------------------
# initialisation des variables globales

dtTrajet <<- NULL # date-heure du trajet
geoAdrDepart <<- NULL # adresse de départ au format "lat,lon"
geoAdrArrivee <<- NULL # idem pour adresse d'arrivée
modele <<- NULL # modèle statistique de prévision

#----------------------------------------------------------------------------------------

# récupération des stations :
stations<-read.csv(fStation, row.names=1, sep=";", fileEncoding = "UTF-8")
position <- as.character(stations$position)
v <- do.call('rbind',strsplit(position,',',fixed=TRUE))
stations$longitude <- as.numeric(v[,2])
stations$latitude <- as.numeric(v[,1])
rm(v, position)

#toilettage des noms des stations :
stations$number <- rownames(stations)
stations$name <- trimws(sapply(strsplit(levels(stations$name),"-"),'[',2))
stations_actives_nom <- stations[stations$status=="OPEN",c("number","name")]
stations_actives_nom <- stations_actives_nom[order(stations_actives_nom$name),]
stations_actives_nom <- rbind(c("0","(aucune)"), stations_actives_nom)

#calcul de la distance entre les stations :
o <- "mDistanceStation"
if (!exists(o) && file.exists(fMatDistanceStation)) {
  load(file=fMatDistanceStation)
} else if (!exists(o)) {
  s <- stations
  s$name <- rownames(s)
  mDistanceStation <- getMatrixDistanceStation(s, c("name","latitude","longitude"))
  rm(s)
  save(mDistanceStation, file=fMatDistanceStation)
}
rm(o)

### rÃ©cupÃ©ration des monuments
monuments <- sort(scan(fMonuments, what="character", sep="\n", fileEncoding = "UTF-8"))

