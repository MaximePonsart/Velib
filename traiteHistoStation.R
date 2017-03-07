# renvoit intègre un fichier historique Velib déjà téléchargé et découpe le temps en pas de 20 min
# entrée : le fichier de données téléchargé
# sortie : le dataframe contenant les données


traiteHistoStation <- function(f) {

  install.packages("jsonlite")
  library(jsonlite)
  
  Histo_velib <- readLines(file(f))
  Histo_velib <- lapply(Histo_velib,jsonlite::fromJSON)
  
  HistoDT <- do.call('rbind',Histo_velib)
  
  HistoDT$date <- as.POSIXct(HistoDT$download_date, origin="1970-01-01")
  HistoDT$jour<- weekdays(as.Date(HistoDT$date))
  HistoDT$mois<- months(as.Date(HistoDT$date))
  
  HistoDT$heure <- as.numeric(format(HistoDT$date, "%H"))
  
  #20 minutes :
  HistoDT$quart <- paste(as.character(sprintf("%02d",HistoDT$heure)),
                         as.character(ceiling(as.numeric(format(HistoDT$date, "%M"))/20+0.1)),
                         sep="/")
  HistoDT$last_update <- NULL
  HistoDT$contract_name <- NULL
  HistoDT$download_date <- NULL
  
  round.mean <- function(x) {
    return(round(mean(x)))
  }
  
  df <- aggregate(HistoDT[,c("available_bike_stands","available_bikes")],
                  list(number=HistoDT$number,jour=HistoDT$jour,quart=HistoDT$quart),
                  round.mean)
  df <- df[order(df$number,df$jour,df$quart),]
  
  return(df)
  
}

  
  