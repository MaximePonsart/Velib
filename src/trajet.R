
#additionne des délais de trajet ligne avec des délais de trajet matrice
# df : data frame (trajet ligne)
# mat : matrice de durées
# dir : direction d'appariement ("ligne" ou "colonne")
# dim : dimension de la somme ("duree" : durée + durée, "date" : date + durée)
# résultat : la matrice résultant de la somme
getSommeDateDuree <- function(df, mat, dir, dim) {
  
  m <- mat
  coef <- ifelse(dim=="duree", 1, 60)
  var <- ifelse(dim=="duree", "duree", "date_heure")
  
  if (dir=="ligne") 
    for (i in 1:nrow(m)) m[i,] <- m[i,]*coef + df[df$number==rownames(m)[i],c(var)]
  else # "colonne"
    for (i in 1:ncol(m)) m[i,] <- m[i,]*coef + df[df$number==colnames(m)[i],c(var)]
    
  return(m)
  
}
  

# calcule la prévision du nombre de vélos ou parkings disponibles
#---
# sta : la station (id)
# dateheure : la date-heure de prévision
# mode : la variable à prévoir, "bike" pour vélo et "stand" pour parking
# météo : la météo
#---
# renvoit un data frame avec pour chaque station (id) la prévision calculée
#---
getPrevDispo <- function(sta, dateheure, meteo, mode) {
  
  getPrev <- function (s, dh, m) {
    if (modele=="happy") return(1)
    if (modele=="random") return(sample(0:s$bike_stands,1))
    #if (modele=="serious") return(...)
  }
  
  if (mode=="bike")
    res <- data.frame(number=character(0), available_bikes=numeric(0))
  else # mode=="parking"
    res <- data.frame(number=character(0), available_bike_stands=numeric(0))
  
  for (i in 1:nrow(sta)) {
    p <- getPrev(sta[i,], dateheure, meteo)
    res <- rbind(res, data.frame(number=sta[i,]$number, available_bikes=p))
  }
  return(res)
}


#calcule la durée du trajet à pieds entre une adresse et une suite de stations
getTrajetsFromAdrToStations <- function(geoAdr, sta) {
  res <- data.frame(number=character(0), duree=numeric(0))
  for (i in 1:nrow(sta)) {
    dt <- drive_time(address=geoAdr, dest=sta[i,]$position, auth="standard_api",
               privkey="", clean=FALSE, add_date='today',
               verbose=FALSE, travel_mode="walking",
               units="metric")
    res <- rbind(res, data.frame(number=sta[i,]$number, duree=dt$time_mins))
  }
  return(res)
}


#calcule en matrice la durée du trajet en vélo entre chaque station
getTrajetsFromStationToStation <- function(sdep, sarr) {
  
  dt <- function(sd,sa) {
    res <- drive_time(address=sd,
                      dest=sa,
                      auth="standard_api", privkey="", clean=FALSE, add_date='today',
                      verbose=FALSE, travel_mode="bicycling",
                      units="metric")
    return(res)
  }
  
  mat <- matrix(NA, nrow=nrow(sdep), ncol=nrow(sarr))
  dimnames(mat) <- list(sdep$number, sarr$number)
  
  for (i in 1:ncol(mat))
    mat[,i] <- dt(sdep$position,rep(sarr[i,]$position,nrow(mat)))$time_mins
  
  return(mat)
  
}


# ---
# actualise les infos météo (précipitations, température en fonction de l'heure souhaitée)
# dt : l'heure souhaitée
# ---
# renvoit un data frame avec la précipitation et la température (et fixe les mêmes infos en variable globale)
# ---
getMeteo <- function(dt) {
  
  Sys.setenv(DARKSKY_API_KEY = apiKeyDarksky)
  
  #récupération de la météo disponible
  tm <- get_current_forecast(as.numeric(getLat(geoParis)),
                             as.numeric(getLon(geoParis)),
                             units="si",language = "fr")
  
  #filtrage sur l'heure courante ainsi que sur les deux premières heures disponibles
  v <- c("time","precipIntensity","temperature")
  meteo <- data.frame()
  meteo <- rbind.data.frame(meteo, as.data.frame(tm$currently[,v]))
  meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[1,v]))
  meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[2,v]))

  #on retient la météo la plus proche de l'heure souhaitée  
  meteo <- meteo[which.min(abs(meteo$time - dt)),]
  
  #fixe variables globales
  meteoPrecipitations <<- meteo$precipIntensity
  meteoTemperature <<- meteo$temperature
  
  return(meteo)
  
}


#renvoit la température d'un objet météo
getTemp <- function(meteo) {
  return(meteo[1])
}


#renvoit les précipitations d'un objet météo
getPrecip <- function(meteo) {
  return(meteo[2])
}


goCalcTrajet <- function() {
  
  #récupération des 5 stations les plus proches depuis l'adresse départ
  s_depart <- getProchesStations(getLat(geoAdrDepart), getLon(geoAdrDepart), "classement", 5)
  
  #idem pour l'adresse d'arrivée
  s_arrivee <- getProchesStations(getLat(geoAdrArrivee), getLon(geoAdrArrivee), "classement", 5)
  
  #récupération de la météo
  meteo <- getMeteo(dtTrajet)
  
  #récupération heure de départ
  dtTrajet
  
  #calcul de la durée du trajet pédestre entre l'adresse de départ et les stations proches
  duree_marche_depart <- getTrajetsFromAdrToStations(geoAdrDepart, s_depart)
  dt_stations_depart <- data.frame(number=duree_marche_depart$number, #date-heure de départ depuis les stations
                                   date_heure=duree_marche_depart$duree*60+dtTrajet) 
  
  #calcul des prévisions de vélos dispos sur les stations de départ à l'heure de départ
  velos_dispos <- getPrevDispo(s_depart, dt_stations_depart, meteo, "bike")
  
  #calcul de la durée des trajets en vélos entre les stations de départ et d'arrivée
  duree_velo_trajets <- getTrajetsFromStationToStation(s_depart, s_arrivee)
  
  #calcul de la prévision des parkings dispos sur les stations d'arrivée
  dt_stations_arrivee <- getSommeDateDuree(dt_stations_depart, #date-heure d'arrivée aux stations
                                           duree_velo_trajets,
                                           "ligne", "date")
  parkings_dispos <- getPrevDispo(s_arrivee, dt_stations_arrivee, meteo, "stand")
  
  #calcul de la durée du trajet pédestre entre l'adresse d'arrivée et les stations proches
  duree_marche_arrivee <- getTrajetsFromAdrToStations(geoAdrArrivee, s_arrivee)
  
  #calcul de la durée totale
  duree_totale <- getSommeDateDuree(duree_marche_depart,
                                    getSommeDateDuree(duree_marche_arrivee, duree_velo_trajets, "colonne", "duree"),
                                    "ligne", "duree")
  
  #retrait des stations dont les prévisions de dispo (vélo ou parking) sont nulles
  duree_totale[rownames(duree_totale)==velos_dispos[velos_dispos$available_bikes==0,]$number,] <- NA
  duree_totale[,colnames(duree_totale)==parkings_dispos[parkings_dispos$available_stands==0,]$number] <- NA
  
  #résultat final : le trajet de parcours dont la durée est la plus faible
  duree_min <- min(duree_totale, na.rm=T)
  trajet_retenu <- head(which(duree_totale==min(duree_totale, na.rm=T), arr.ind=T),1) #on retient le premier en cas d'égalité
  station_depart_cible <- rownames(duree_totale)[trajet_retenu$row] 
  station_arrivee_cible <- colnames(duree_totale)[trajet_retenu$col] 
  
}




