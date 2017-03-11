#### configuration globale

### chargement des packages requis :

source("src/loadPackages.R", local = TRUE)$value
loadPackages(c("leaflet","shiny","Imap"))

source("src/getDispoStationTR.R", local = TRUE)$value
source("src/calcDistanceStations.R", local = TRUE)$value

### initialisation des variables globales :

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

### récupération des stations :

stations<-read.csv("data/stations-velib-disponibilites-en-temps-reel.csv",
                   row.names=1, sep=";", fileEncoding = "UTF-8")
position <- as.character(stations$position)
v <- do.call('rbind',strsplit(position,',',fixed=TRUE))
stations$longitude <- as.numeric(v[,2])
stations$latitude <- as.numeric(v[,1])
rm(v, position)
stations <- stations[stations$status=="OPEN",]
#toilettage des noms des stations :
stations_nom <- sort(trimws(sapply(strsplit(levels(stations$name),"-"),'[',2)))

###calcul de la distance entre les stations :
f <- "data/mDistanceStation.Rda"
o <- "mDistanceStation"
if (!exists(o) && file.exists(f)) {
  load(file=f)
} else if (!exists(o)) {
  s <- stations
  s$name <- rownames(s)
  mDistanceStation <- getMatrixDistanceStation(s, c("name","latitude","longitude"))
  rm(s)
  save(mDistanceStation, file=f)
}
rm(f,o)
