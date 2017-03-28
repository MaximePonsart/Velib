# calcul de la durée du trajet entre les adresses de départ (from) et d'arivée (to)
# les trajets résultent du produit cartésien entre départ et arrivée
getGoogleDistanceMatrix <- function (from, to, mode) {
  
  #chargement du résultat des précédents requêtages (if any)
  # mGoogleDistanceMatrix est un data frame avec pour schéma :
  # from : coordonnée de départ au format "lat,lon"
  # to: coordonnée d'arrivée au format "lat, lon"
  # mode : le type de déplacement : "walking" ou "bicycling"
  # time_mins : la durée du trajet en minutes (renvoyé par l'API)
  # origin : l'adresse de départ (idem)
  # destination : l'adresse d'arrivée (idem)
  if (nrow(mGoogleDistanceMatrix)==0 && file.exists(fGoogleDistanceMatrix)) {
    load(file=fGoogleDistanceMatrix)
    mGoogleDistanceMatrix <<- mGoogleDistanceMatrix
  }
    
  #restriction du requêtage aux seuls jeux non déjà requêtés
  df0 <- expand.grid(from, to, mode) #df0 : data frame des trajets à estimer
  df0 <- setNames(df0,c("from","to","mode"))
  df <- data.frame(from=character(0), to=character(0), mode=character(0)) #df : df0 moins les trajets déjà requêtés
  
  m <- mGoogleDistanceMatrix[,c("from","to","mode")]
  if (nrow(m)==0)
    df <- df0
  else {
    for (i in 1:nrow(df0)) {
      if (tail(duplicated(rbind(m,df0[i,])),1)>0)
        next #déjà requêté
      else
        df <- rbind(df,df0[i,]) #nouveau trajet à requêter
    }  
  }
  rm(m)
  
  df <- cbind(df,time_mins=rep(NA,nrow(df)),dist_num=rep(NA,nrow(df)),origin=rep(NA,nrow(df)),destination=rep(NA,nrow(df)))
  
  if (nrow(df)==0)
    return(df[,c("from","to","mode","time_mins")])
  else
    #appel de l'API Google sur le seul jeu de requêtage restreint
    #requêtage par pas de 25 max (limitation de l'API Google Distance Matrix)
    for (i in seq(1,nrow(df),by=25)) {
      t <- tryCatch({
        suivant <- seq(i,nrow(df),by=25)[2]
        if (is.na(suivant)) suivant <- nrow(df)+1
        dt <- drive_time(address=as.character(df[seq(i,suivant-1),]$from),
                         dest=as.character(df[seq(i,suivant-1),]$to),
                         auth="standard_api",
                         privkey=apiKeyGoogleDistanceMatrix, clean=FALSE, add_date='today',
                         verbose=FALSE, travel_mode=mode,
                         units="metric")
        },
        error=function(e) {
          message("!erreur dans l'accès à l'API Google drive_time")
          return(1)
        },
        warning=function(w) {message("!alerte dans l'accès à l'API Google drive_time")},
        finally = {
          message(paste(dt$status, dt$error_message))
          if (all(dt$status=="OK")) {
            #récupérer la valeur dt$time_mins
            df[seq(1,nrow(df)),c("time_mins","dist_num","origin","destination")] <- dt[,c("time_mins","dist_num","origin","destination")]
            mGoogleDistanceMatrix <<- rbind(mGoogleDistanceMatrix, df)
            save(mGoogleDistanceMatrix, file=fGoogleDistanceMatrix)
            return(df[,c("from","to","mode","time_mins")])
          }
          else return(1)
        }
      )    
    }
  
}


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


#additionne des délais de trajet ligne avec des délais de trajet matrice
# df : data frame (trajet ligne)
# mat : matrice de durées
# dir : direction d'appariement ("ligne" ou "colonne")
# dim : dimension de la somme ("duree" : durée + durée, "date" : date + durée)
# résultat : la matrice résultant de la somme
getSommeDateDuree <- function(df, mat, dir, dim) {
  
  coef <- ifelse(dim=="duree", 1, 60)
  var <- ifelse(dim=="duree", "time_mins", "date_heure")
  
  m <- matrix(nrow=nrow(mat), ncol=ncol(mat))
  dimnames(m) <- dimnames(mat)
  if (dir=="ligne") {
    for (i in 1:nrow(mat))
      m[i,] <- mat[i,]*coef + df[df$number==rownames(mat)[i],c(var)]
  }
  else { # "colonne"
    for (i in 1:ncol(mat))
      m[,i] <- mat[,i]*coef + df[df$number==colnames(mat)[i],c(var)]
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
  else # mode=="stand"
    res <- data.frame(number=character(0), available_bike_stands=numeric(0))
  
  for (i in 1:nrow(sta)) {
    p <- getPrev(sta[i,], dateheure, meteo)
    res <- rbind(res, c(sta[i,]$number, p))
  }
  
  if (mode=="bike")
    res <- setNames(res, c("number","available_bikes"))
  else
    res <- setNames(res,c("number","available_bike_stands"))
  
  return(res)
  
}


#calcule la durée du trajet à pieds entre une adresse et une suite de stations
getTrajetsFromAdrToStations <- function(geoAdr, sta) {

  getGoogleDistanceMatrix(from=geoAdr, to=sta$position, mode="walking")
  
  return(cbind(data.frame(number=sta$number),
               subset(mGoogleDistanceMatrix,from==geoAdr & to %in% (sta$position), select=time_mins)))
  
}


#calcule en matrice la durée du trajet en vélo entre chaque station
getTrajetsFromStationToStation <- function(sdep, sarr) {
  
  mat <- matrix(NA, nrow=nrow(sdep), ncol=nrow(sarr))
  dimnames(mat) <- list(sdep$number, sarr$number)
  
  for (i in 1:ncol(mat)) {
    getGoogleDistanceMatrix(from=sdep$position, to=sarr[i,]$position, mode="bicycling")
    mat[,i] <- subset(mGoogleDistanceMatrix, from %in% (sdep$position) & to==sarr[i,]$position, select=time_mins)$time_mins
  }
    
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
      return(NA)
      },
    warning=function(w) {message("alerte dans l'accès à l'API météo")}
  )
  
  if (all(is.na(res))) meteo <- NA
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
  meteoPrecipitations <<- ifelse(all(is.na(meteo)),NA,meteo$precipIntensity)
  meteoTemperature <<- ifelse(all(is.na(meteo)),NA,meteo$temperature)
  
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
  
  #tableau résultat
  dfParcours <<- data.frame(
    libemplacement=character(0),
    typemplacement=factor(levels=c("station","adresse")),
    idemplacement=character(0),
    dateheure=numeric(0),
    duree=numeric(0),
    mode=factor(levels=c("departure","walking","getbike","bicycling","releasebike","arrival")),
    available_bikes=numeric(0),
    available_bike_stands=numeric(0),
    idparcours=numeric(0), #concaténation geocodes depart-arrivée
    typligne=factor(levels=c("lieu-depart","marche-depart","station-depart","velo-trajet","station-arrivee","marche-arrivee","lieu-arrivee"))
  )
  
    
  #récupération de la météo
  meteo <- getMeteo(dtTrajet)
  if (all(is.na(meteo))) message("!impossible de calculer la météo")

  #initialisation du parcours avec le point de départ
  #---d1---
  if (is.na(geoAdrDepart)) {
    geoAdrDepart <<- stations[stationDepSel,]$position
    adrDepart <<- stations[stationDepSel,]$address
  }
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="lieu-depart",
                         libEmplacement=adrDepart,
                         typEmplacement="adresse",
                         idEmplacement=geoAdrDepart,
                         dateheure=dtTrajet,
                         duree=NA,
                         mode="departure",
                         available_bikes=NA,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  #---f1---
    
  #récupération des 5 stations les plus proches depuis l'adresse départ & calcul du trajet pédestre
  #---d2---
  s_depart <- getProchesStations(getLat(geoAdrDepart), getLon(geoAdrDepart), "classement", 5)
  duree_marche_depart <- getTrajetsFromAdrToStations(geoAdrDepart, s_depart)
  
  if (all(is.na(duree_marche_depart$time_mins))) {
    message("!impossible de calculer les durées de trajet")
    return(1)
  }
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="marche-depart",
                         libEmplacement=NA,
                         typEmplacement=NA,
                         idEmplacement=NA,
                         dateheure=NA,
                         duree=duree_marche_depart$time_mins,
                         mode="walking",
                         available_bikes=NA,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  #---f2---
    
  #idem pour l'adresse d'arrivée
  #---d3---
  if (is.na(geoAdrArrivee)) {
    geoAdrArrivee <<- stations[stationArrSel,]$position
    adrArrivee <<- stations[stationArrSel,]$address
  }

  s_arrivee <- getProchesStations(getLat(geoAdrArrivee), getLon(geoAdrArrivee), "classement", 5)
  duree_marche_arrivee <- getTrajetsFromAdrToStations(geoAdrArrivee, s_arrivee)
  
  if (all(is.na(duree_marche_arrivee$time_mins))) {
    message("!impossible de calculer les durées de trajet")
    return(1)
  }
  #---f3---
    
  #calcul des prévisions de vélos dispos sur les stations de départ à l'heure de départ
  #---d4---
  dt_stations_depart <- data.frame(number=duree_marche_depart$number, #date-heure de départ depuis les stations
                                   date_heure=duree_marche_depart$time_mins*60+dtTrajet)
  
  velos_dispos <- getPrevDispo(s_depart, dt_stations_depart, meteo, "bike")
  
  if (all(is.na(velos_dispos$available_bikes))) {
    message("!impossible de calculer les prévisions")
    return(1)
  }
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="station-depart",
                         libEmplacement=s_depart$name,
                         typEmplacement="station",
                         idEmplacement=dt_stations_depart$number,
                         dateheure=dt_stations_depart$date_heure,
                         duree=NA,
                         mode="getbike",
                         available_bikes=velos_dispos$available_bikes,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  #---f4---
  
  #calcul de la durée des trajets en vélos entre les stations de départ et d'arrivée
  #---d5---
  duree_velo_trajets <- getTrajetsFromStationToStation(s_depart, s_arrivee)
  if (all(is.na(duree_velo_trajets))) {
    message("!impossible de calculer les durées de trajet")
    return(1)
  }
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="velo-trajet",
                         libEmplacement=NA,
                         typEmplacement=NA,
                         idEmplacement=NA,
                         dateheure=NA,
                         duree=as.numeric(duree_velo_trajets),
                         mode="bicycling",
                         available_bikes=NA,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  #---f5---
  
  #calcul de la prévision des parkings dispos sur les stations d'arrivée
  #---d6---
  dt_stations_arrivee <- getSommeDateDuree(dt_stations_depart, #date-heure d'arrivée aux stations
                                           duree_velo_trajets,
                                           "ligne", "date")
  
  parkings_dispos <- getPrevDispo(s_arrivee, dt_stations_arrivee, meteo, "stand")
  
  if (all(is.na(parkings_dispos$available_bike_stands))) {
    message("!impossible de calculer les prévisions")
    return(1)
  }
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="station-arrivee",
                         libEmplacement=as.character(sapply(s_arrivee$name,FUN=function(x){rep(x,5)})),
                         typEmplacement="station",
                         idEmplacement=as.character(sapply(colnames(dt_stations_arrivee),FUN=function(x){rep(x,5)})),
                         dateheure=as.POSIXct(as.numeric(dt_stations_arrivee), origin="1970-01-01"),
                         duree=NA,
                         mode="releasebike",
                         available_bikes=NA,
                         available_bike_stands=as.character(sapply(parkings_dispos$available_bike_stands,FUN=function(x){rep(x,5)})),
                         idparcours=seq(1,25)                      
                       ))
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="marche-arrivee",
                         libEmplacement=NA,
                         typEmplacement=NA,
                         idEmplacement=NA,
                         dateheure=NA,
                         duree=as.numeric(sapply(duree_marche_arrivee$time_mins,FUN=function(x){rep(x,5)})),
                         mode="walking",
                         available_bikes=NA,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  #---f6---
  
  #calcul de la durée totale
  #---d7---
  duree_totale <<- getSommeDateDuree(duree_marche_depart,
                                    getSommeDateDuree(duree_marche_arrivee, duree_velo_trajets, "colonne", "duree"),
                                    "ligne", "duree")
  
  dfParcours <<- rbind(dfParcours,
                       data.frame(
                         typligne="lieu-arrivee",
                         libEmplacement=adrArrivee,
                         typEmplacement="adresse",
                         idEmplacement=geoAdrArrivee,
                         dateheure=dtTrajet+as.numeric(duree_totale)*60,
                         duree=NA,
                         mode="arrival",
                         available_bikes=NA,
                         available_bike_stands=NA,
                         idparcours=seq(1,25)                      
                       ))
  
  dfParcours$typligne <<- factor(dfParcours$typligne, levels=c("lieu-depart","marche-depart","station-depart","velo-trajet","station-arrivee","marche-arrivee","lieu-arrivee"))
  duree_parcours <- setNames(aggregate(duree~idparcours, dfParcours, sum), c("idparcours","dureeparcours")) #durée de chaque parcours
  dfParcours <<- dfParcours[order(dfParcours$idparcours,dfParcours$typligne),]
  dfParcours <<- merge(dfParcours, duree_parcours, by="idparcours")
  # parcours <- parcours[order(parcours$dureeparcours,parcours$idparcours),
  #                                 colnames(parcours)[!colnames(parcours) %in% c("dureeparcours")]]
  dfParcours <<- dfParcours[order(dfParcours$dureeparcours,dfParcours$idparcours),]
  dfParcours <<- dfParcours[order(dfParcours$idparcours, dfParcours$typligne),]
  #---f7---
  
  #retrait des stations dont les prévisions de dispo (vélo ou parking) sont nulles
  duree_totale[rownames(duree_totale)==velos_dispos[velos_dispos$available_bikes==0,]$number,] <<- NA
  duree_totale[,colnames(duree_totale)==parkings_dispos[parkings_dispos$available_stands==0,]$number] <<- NA
  
  #résultat final : le trajet de parcours dont la durée est la plus faible
  dureeTrajet <<- min(duree_totale, na.rm=T)
  trajet_retenu <- head(which(duree_totale==dureeTrajet, arr.ind=T),1) #on retient le premier en cas d'égalité
  stationDepTrajet <<- rownames(duree_totale)[trajet_retenu[,"row"]]
  stationArrTrajet <<- colnames(duree_totale)[trajet_retenu[,"col"]]
  
  
}



