#### configuration globale


#chargement des packages requis
source("src/loadPackages.R", local = TRUE)$value
loadPackages(c("leaflet","shiny","shinythemes","Imap","ggmap","placement","geosphere","darksky","googleway"))

#options diverses
options(shiny.trace=TRUE)
options(xtable.include.rownames=F)

#inclusion de fonctions g√©n√©riques
source("src/getDispoStationTR.R", local = TRUE)$value
source("src/calcDistanceStations.R", local = TRUE)$value
source("src/trajet.R", local = TRUE)$value

#----------------------------------------------------------------------------------------
# initialisation des variables constantes

#palette pour la carte
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

#API key DarkSky et variables m√©t√©o (pr√©cipitations, temp√©rature)
apiKeyDarksky <- "9778cdc6ddc2eaf7b6854ad412c21eec"

#API key Google Direction
apiKeyGoogleDirection <- "AIzaSyA6A8jEGpG3__YDEIZngB5x_t8K12g_v7s"

#Coordonn√©es de Paris
geoParis <- "48.863,2.35"

#fichier jeu de donn√©es pour les dispos V√©lib
fStation <- "data/stations-velib-disponibilites-en-temps-reel.csv"

#fichier liste des monuments de Paris
fMonuments <- "data/monuments_paris.txt"

#fichier matrice des distances stations
fMatDistanceStation <- "data/mDistanceStation.Rda"

#fichier du modËle de prÈvision (Random Forest, aka 'Serious")
fModRandomForest <- "data/modRandomForest.RDS"

#data frame des stations
stations <- NULL

#vecteur des noms des stations actives
stations_actives_nom <- NULL

#----------------------------------------------------------------------------------------
# initialisation des variables globales

dtTrajet <<- NULL # date-heure du trajet

geoAdrDepart <<- NULL # adresse de dÈpart au format "lat,lon"
geoAdrArrivee <<- NULL # idem pour adresse d'arrivÈe
geoStaDepTrajet <<- NULL #idem pour la station de dÈpart
geoStaArrTrajet <<- NULL #idem pour la station d'arrivÈe

stationDepTrajet <<- NULL # la station de dÈpart retenue pour le trajet
stationArrTrajet <<- NULL # idem pour la station d'arrivÈe

modele <<- "happy" # mod√®le statistique de pr√©vision
meteoPrecipitations <<- NULL # pr√©cipitations m√©t√©o de l'heure cible
meteoTemperature <<- NULL # temp√©rature m√©t√©o de l'heure cible

#----------------------------------------------------------------------------------------

# r√©cup√©ration des stations :
stations<-read.csv(fStation, row.names=1, sep=";", fileEncoding = "UTF-8", stringsAsFactors = T)
stations$position <- as.character(stations$position)
#stations$status <- as.factor(stations$status)
#position <- as.character(stations$position)
v <- do.call('rbind',strsplit(stations$position,',',fixed=TRUE))
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

### r√©cup√©ration des monuments
monuments <- sort(scan(fMonuments, what="character", sep="\n", fileEncoding = "UTF-8"))

### chargement du modËle statistique de prÈvision
#modele_RF<-readRDS(file=fModRandomForest)
#(‡ tester avant activation)

