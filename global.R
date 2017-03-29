#### configuration globale


#chargement des packages requis
source("src/loadPackages.R", local = TRUE)$value
loadPackages(c("leaflet","shiny","shinythemes","Imap","ggmap","placement","geosphere","darksky",
               "googleway","xtable","DT","dplyr","randomForest","lubridate"))

#options diverses
options(shiny.trace=TRUE)
options(xtable.include.rownames=F)

#inclusion de fonctions generiques
source("src/meteo.R", local = TRUE)$value
source("src/modeleRF.R", local = TRUE)$value
source("src/getDispoStationTR.R", local = TRUE)$value
source("src/calcDistanceStations.R", local = TRUE)$value
source("src/trajet.R", local = TRUE)$value

#----------------------------------------------------------------------------------------
# initialisation des variables constantes

#palette pour la carte
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

#API key DarkSky et variables meteo (precipitations, temperature)
apiKeyDarksky <- "9778cdc6ddc2eaf7b6854ad412c21eec"

#API key Google Direction
apiKeyGoogleDirection <- "AIzaSyA6A8jEGpG3__YDEIZngB5x_t8K12g_v7s"

#API key Google Distance Matrix
apiKeyGoogleDistanceMatrix <- ""

#Coordonnees de Paris
geoParis <- "48.863,2.35"

#fichier jeu de donnees pour les dispos Velib
fStation <- "data/stations-velib-disponibilites-en-temps-reel.csv"

#fichier liste des monuments de Paris
fMonuments <- "data/monuments_paris.txt"

#fichier matrice des distances stations
fDistanceStation <- "data/mDistanceStation.Rda"

#fichier temps de trajet deja calcules (offline Google drive_time)
fGoogleDistanceMatrix <- "data/mGoogleDistanceMatrix.Rda"

#fichier du modele de prevision (Random Forest, aka 'Serious")
fModRandomForest <- "data/modRandomForest.RDS"

#data frame des stations
stations <- NULL

#vecteur des noms des stations actives
stations_actives_nom <- NULL

#----------------------------------------------------------------------------------------
# initialisation des variables globales

dateSimulee <<- as.Date("2017-02-01")
heureSimulee <<- "05:00"
dtTrajet <<- as.POSIXct(paste0(dateSimulee, heureSimulee)) # date-heure du trajet

mGoogleDistanceMatrix <<- data.frame(from=character(0),
                                     to=character(0),
                                     mode=character(0),
                                     time_mins=numeric(0),
                                     dist_num=numeric(0),
                                     origin=character(0),
                                     destination=character(0)
                                     ) # durees de trajet deja calcules via API Google

geoAdrDepart <<- NA # adresse de depart au format "lat,lon"
geoAdrArrivee <<- NA # idem pour arrivee

adrDepart <<- NA # adresse de depart au format adresse textuelle
adrArrivee <<- NA # idem pour arrivee

stationDepSel <<- NA # station de depart selectionnee
stationArrSel <<- NA # idem pour la station d'arrivee

stationDepTrajet <<- NA # la station de depart retenue pour le trajet
stationArrTrajet <<- NA # idem pour la station d'arrivee

dureeTrajet <<- NA # duree calculee pour le trajet retenu
dureeTotale <<- NA # matrice des durees calculees
dfParcours <<- NA # data frame des parcours calcules

modele <<- "none" # modele statistique de prevision

meteoPrecipitations <<- NA # precipitations meteo de l'heure cible
meteoTemperature <<- NA # temperature meteo de l'heure cible
getMeteo(dtTrajet)

wlog <<- "" # messages logs des traitements en cours
winput <<- NA # fichier de donnees en cours de traitement

#----------------------------------------------------------------------------------------

# recuperation des stations :
stations<-read.csv(fStation, row.names=1, sep=";", fileEncoding = "UTF-8", stringsAsFactors = T)
stations$position <- as.character(stations$position)
#stations$status <- as.factor(stations$status)
#position <- as.character(stations$position)
v <- do.call('rbind',strsplit(stations$position,',',fixed=TRUE))
stations$longitude <- as.numeric(v[,2])
stations$latitude <- as.numeric(v[,1])
rm(v)

#toilettage des noms des stations :
stations$number <- rownames(stations)
stations$name <- trimws(sapply(strsplit(levels(stations$name),"-"),'[',2))
stations_actives_nom <- stations[stations$status=="OPEN",c("number","name")]
stations_actives_nom <- stations_actives_nom[order(stations_actives_nom$name),]
stations_actives_nom <- rbind(c("0","(aucune)"), stations_actives_nom)

#calcul de la distance geo entre les stations :
o <- "mDistanceStation"
if (!exists(o) && file.exists(fDistanceStation)) {
  load(file=fDistanceStation)
} else if (!exists(o)) {
  s <- stations
  s$name <- rownames(s)
  mDistanceStation <- getMatrixDistanceStation(s, c("name","latitude","longitude"))
  rm(s)
  save(mDistanceStation, file=fDistanceStation)
}


### recuperation des monuments
monuments <- sort(scan(fMonuments, what="character", sep="\n", fileEncoding = "UTF-8"))

### chargement du modele statistique de prevision
#modele_RF<-readRDS(file=fModRandomForest)
#(a tester avant activation)



