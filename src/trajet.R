
#---
# calcule le tracé trajet via l'API Google Direction
# d : lieu de départ
# a : lieu d'arrivée
# m : le mode de déplacement ("bicycling" : vélo, ...)
#---
# renvoit un data frame
#---
setGoogleTrajet <- function (d, a, m) {
  
  df <- google_directions(origin = c(getLat(d), getLon(d)),
                          destination = c(getLat(a), getLon(a)),
                          key = apiKeyGoogleDirection,
                          mode = m,
                          simplify = TRUE)
  
  polyline <-df$routes$overview_polyline$points
  
  df <- decode_pl(polyline)
  
}

# trace le trajet du parcours sur la carte
# df : data frame calculé par l'API Google Direction
traceTrajet <- function () {
  
# appel de l'API
  df_marche_dep <- setGoogleTrajet(geoAdrDepart, geoStaDepTrajet, "walking")
  df_velo <- setGoogleTrajet(geoStaDepTrajet, geoStaArrTrajet, "bicycling")
  df_marche_arr <- setGoogleTrajet(geoStaArrTrajet, geoAdrArrivee, "walking")
  
  leafletProxy("carteGeo") %>%
    addPolylines(data = df_marche_dep, lat = ~lat, lng = ~lon) %>%
    addPolylines(data = df_velo, lat = ~lat, lng = ~lon) %>%
    addPolylines(data = df_marche_arr, lat = ~lat, lng = ~lon)
  
}


#additionne des délais de trajet ligne avec des dÃ©lais de trajet matrice
# df : data frame (trajet ligne)
# mat : matrice de durées
# dir : direction d'appariement ("ligne" ou "colonne")
# dim : dimension de la somme ("duree" : durée + durée, "date" : date + durée)
# résultat : la matrice résultant de la somme
getSommeDateDuree <- function(df, mat, dir, dim) {
  
  coef <- ifelse(dim=="duree", 1, 60)
  var <- ifelse(dim=="duree", "duree", "date_heure")
  
  m <- matrix(nrow=nrow(mat), ncol=ncol(mat))
  if (dir=="ligne") {
    for (i in 1:nrow(mat)) {
      m <- rbind(m, mat[i,]*coef + df[df$number==rownames(mat)[i],c(var)])
    }
  }
  else { # "colonne"
    for (i in 1:ncol(mat))
      m <- cbind(m, mat[i,]*coef + df[df$number==colnames(mat)[i],c(var)])
  }
    
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
    t <- tryCatch(
      dt <- drive_time(address=geoAdr, dest=sta[i,]$position, auth="standard_api",
                       privkey="", clean=FALSE, add_date='today',
                       verbose=FALSE, travel_mode="walking",
                       units="metric"),
      error=function(e) {
        message("!erreur dans l'accès à l'API Google drive_time")
        return(1)
      },
      warning=function(w) {message("!alerte dans l'accès à l'API Google drive_time")},
      finally = {
        message(paste(dt$status, dt$error_message))
        if (dt$status=="CONNECTION_ERROR") return(NA)
      }
    )
    res <- rbind(res, data.frame(number=sta[i,]$number, duree=dt$time_mins))
  }
  return(res)
}


#calcule en matrice la duréee du trajet en vélo entre chaque station
getTrajetsFromStationToStation <- function(sdep, sarr) {
  
  dt <- function(sd,sa) {
    t <- tryCatch(
      res <- drive_time(address=sd,
                        dest=sa,
                        auth="standard_api", privkey="", clean=FALSE, add_date='today',
                        verbose=FALSE, travel_mode="bicycling",
                        units="metric"),
      error=function(e) {
        message("!erreur dans l'accès à l'API Google drive_time")
        return(1)
      },
      warning=function(w) {message("!alerte dans l'accès à l'API Google drive_time")},
      finally = {
        message(paste(res$status, res$error_message))
        if (dt$status=="CONNECTION_ERROR") return(NA)
      }
    )
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
  res <- tryCatch(
    tm <- get_current_forecast(as.numeric(getLat(geoParis)),
                               as.numeric(getLon(geoParis)),
                               units="si",language = "fr"),
    error=function(e) {
      message("erreur dans l'accès à l'API météo")
      return(1)
      },
    warning=function(w) {message("alerte dans l'accès à l'API météo")}
  )
  
  if (res==1) meteo <- NA
  else {
    #filtrage sur l'heure courante ainsi que sur les deux premières heures disponibles
    v <- c("time","precipIntensity","temperature")
    meteo <- data.frame()
    meteo <- rbind.data.frame(meteo, as.data.frame(tm$currently[,v]))
    meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[1,v]))
    meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[2,v]))
    
    #on retient la météo la plus proche de l'heure souhaitée  
    meteo <- meteo[which.min(abs(meteo$time - dt)),]
  }
  
  #fixe variables globales
  meteoPrecipitations <<- ifelse(is.na(meteo),NA,meteo$precipIntensity)
  meteoTemperature <<- ifelse(is.na(meteo),NA,meteo$temperature)
  
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
  
  if(is.na(geoAdrDepart) || is.na(geoAdrArrivee)) return(1)
  
  #récupération des 5 stations les plus proches depuis l'adresse départ
  s_depart <- getProchesStations(getLat(geoAdrDepart), getLon(geoAdrDepart), "classement", 5)
  
  #idem pour l'adresse d'arrivée
  s_arrivee <- getProchesStations(getLat(geoAdrArrivee), getLon(geoAdrArrivee), "classement", 5)
  
  #récupération de la météo
  meteo <- getMeteo(dtTrajet)
  if (is.na(meteo)) message("!impossible de calculer la météo")
  
  #récupération heure de départ
  #dtTrajet
  
  #calcul de la durée du trajet pédestre entre l'adresse de départ et les stations proches
  duree_marche_depart <- getTrajetsFromAdrToStations(geoAdrDepart, s_depart)
if (all(is.na(duree_marche_depart$duree))) {
    message("!impossible de calculer les distances")
    return(1)
  }
  dt_stations_depart <- data.frame(number=duree_marche_depart$number, #date-heure de départ depuis les stations
                                   date_heure=duree_marche_depart$duree*60+dtTrajet) 
  
  #calcul des prévisions de vélos dispos sur les stations de départ à l'heure de départ
  velos_dispos <- getPrevDispo(s_depart, dt_stations_depart, meteo, "bike")
  if (is.na(velos_dispos)) {
    message("!impossible de calculer les prévisions")
    return(1)
  }
  
  #calcul de la durée des trajets en vélos entre les stations de départ et d'arrivée
  duree_velo_trajets <- getTrajetsFromStationToStation(s_depart, s_arrivee)
  if (all(is.na(duree_velo_trajets))) {
    message("!impossible de calculer les durées de trajet")
    return(1)
  }
  
  #calcul de la prévision des parkings dispos sur les stations d'arrivée
  dt_stations_arrivee <- getSommeDateDuree(dt_stations_depart, #date-heure d'arrivée aux stations
                                           duree_velo_trajets,
                                           "ligne", "date")
  parkings_dispos <- getPrevDispo(s_arrivee, dt_stations_arrivee, meteo, "stand")
  if (is.na(parkings_dispos)) {
    message("!impossible de calculer les prévisions")
    return(1)
  }
  
  #calcul de la durée du trajet pédestre entre l'adresse d'arrivée et les stations proches
  duree_marche_arrivee <- getTrajetsFromAdrToStations(geoAdrArrivee, s_arrivee)
  if (all(is.na(duree_marche_arrivee))) {
    message("!impossible de calculer les durées de trajet")
    return(1)
  }
  
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
  stationDepTrajet <<- rownames(duree_totale)[trajet_retenu$row] 
  stationArrTrajet <<- colnames(duree_totale)[trajet_retenu$col] 
  
  #màj les stations de départ et d'arrivée sur la carte
  setMapCircleDeparr(stationDepTrajet, "depart")
  setMapCircleDeparr(stationArrTrajet, "arrivee")
  
  #tracé du trajet sur la carte :
  traceTrajet()
  
}



