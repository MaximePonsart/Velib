
# ---
# actualise les infos météo (précipitations, température en fonction de l'heure souhaitée)
# dt : l'heure souhaitée
# ---
# renvoit un data frame avec la précipitation et la température (et fixe les mêmes infos en variable globale)
# ---
getMeteo <- function(dt,mode) {
  
  Sys.setenv(DARKSKY_API_KEY = apiKeyDarksky)
  
  #récupération de la météo disponible
  res <- tryCatch(
    # tm <- get_current_forecast(as.numeric(getLat(geoParis)),
    #                            as.numeric(getLon(geoParis)),
    #                            dt,
    #                            units="si",language = "fr"),
    tm <- get_forecast_for(as.numeric(getLat(geoParis)),
                               as.numeric(getLon(geoParis)),
                               dt,
                               units="si",language = "fr"),
    error=function(e) {
      message("erreur dans l'accès à l'API météo")
      return(NA)
    },
    warning=function(w) {message("alerte dans l'accès à l'API météo")}
  )
  
  if (all(is.na(res))) meteo <- NA
  else {
    if (missing(mode) || mode!="full") {
      #filtrage sur l'heure courante ainsi que sur les deux premières heures disponibles
      v <- c("time","precipIntensity","temperature")
      meteo <- data.frame()
      meteo <- rbind.data.frame(meteo, as.data.frame(tm$currently[,v]))
      meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[1,v]))
      meteo <- rbind.data.frame(meteo, as.data.frame(tm$hourly[2,v]))
      
      #on retient la météo la plus proche de l'heure souhaitée  
      meteo0 <- meteo[which.min(abs(meteo$time - dt)),]
      
      #fixe variables globales
      meteoPrecipitations <<- ifelse(all(is.na(meteo0)),NA,meteo$precipIntensity)
      meteoTemperature <<- ifelse(all(is.na(meteo0)),NA,meteo$temperature)
      
      return(meteo)
    }
    else { #mode full
     return(tm) 
    }
  }

}


# #renvoit la température d'un objet météo
# getTemp <- function(meteo) {
#   return(meteo[1])
# }
# 
# 
# #renvoit les précipitations d'un objet météo
# getPrecip <- function(meteo) {
#   return(meteo[2])
# }
