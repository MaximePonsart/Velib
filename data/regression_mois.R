#prerequis: avoir les 2 tables 
#stations-velib-disponibilites-en-temps-reel.csv
#data_all_Paris.jjson.txt

#PROJET MONUMENTalVELIB
#Participants: Laurent Camus - Severine Castor - Sunil Pavitrin - Maxime Ponsart
#V1: atelier R 31/01
#humeur des participants: 
#

#installation des packages
install.packages("jsonlite")
install.packages("lubridate")
install.packages("Imap")
install.packages("gbm")
install.packages("darksky")
install.packages("purrr")
install.packages("RJSONIO")

library(gbm)
library(jsonlite)
library(dplyr)
library(lubridate)
library(Imap)
library(bestglm)
library(glmnet)
library(randomForest)
library(e1071)
library(kernlab)
library(jsonlite)
library(RCurl)
library(curl)
library(darksky)
library(purrr)
library(RJSONIO)
library(plyr)

#importation des données météo

#url <- 'https://api.darksky.net/forecast/9778cdc6ddc2eaf7b6854ad412c21eec/48.866667,2.333333'
#now <- get_current_forecast(48.866667, 2.333333)

#darksky_api_key <- "9778cdc6ddc2eaf7b6854ad412c21eec"

#tmp <- get_forecast_for(48.866667,2.333333,"2016-08-01T12:00:00",units="si",language = "fr")
#tmp1<-as.data.frame(tmp)


#lancer 1 prremiere fois. demande de clé dans console. rentrer clé et relancer
date.range <- seq.Date(from=as.Date('2017-01-01'), to=as.Date('2017-03-01'), by='1 day')
date.range


hdwd <- data.frame()

for(i in seq_along(date.range)) {
  tmp<-get_forecast_for(48.866667,2.333333,paste(date.range[i],'T12:00:00',sep=""),units="si",language = "fr")
  tmp1<-as.data.frame(tmp)
  tmp2<-data.frame(tmp1$hourly.time,tmp1$hourly.summary,tmp1$hourly.precipIntensity,tmp1$hourly.temperature)
  hdwd <- rbind(hdwd,tmp2)
}

hdwd$date <- as.POSIXct(hdwd$tmp1.hourly.time, origin="1970-01-01")
hdwd$jour<- weekdays(as.Date(hdwd$date))
hdwd$mois<- months(as.Date(hdwd$date))
hdwd$annee<- format(hdwd$date,"%Y")
hdwd$jour_num<-as.POSIXlt(hdwd$date)$mday
hdwd$heure <- as.numeric(format(hdwd$date, "%H"))

meteo<-hdwd

names(meteo)[2]<-"ciel"
names(meteo)[3]<-"precipitation"
names(meteo)[4]<-"temperature"
saveRDS(meteo,file="meteo.RDS")
meteo<-readRDS(file="meteo.RDS")
#rm(hdwd,tmp1,tmp2,tmp,serie)

#setwd("//telemaque/users/CEPE-S1-02/Bureau")
setwd("C:/Users/sevdr/Desktop/data scientist/Projet velib")

vacance<-read.table("vacance.csv",sep=";",header=T)

velib2<-read.csv("stations-velib-disponibilites-en-temps-reel.csv",
                 sep=";")
names(velib2)
hist(velib2$bike_stands,freq=FALSE,xlab="Taille",main="Hist. de la taille des stations")
lines(density(velib2$bike_stands),lwd=2,col=2)
position <- as.character(velib2$position)
vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
velib2$longitude <- as.numeric(vecteur[,2])
velib2$latitude <- as.numeric(vecteur[,1])
summary(velib2)
rm(data.range,i,position)
?readLines
# le document initial est compos? de plusieurs doc JSON

# on applique le fonction FROMJSON a chaque doc JSON, pour les transformer en DataFrame

#Histo_velib <- readLines(file("C:/Users/sevdr/Desktop/data scientist/data_all_Paris.jjson_2017-01-01-1483248351"))


Histo_velib <- readLines(file("C:/Users/sevdr/Desktop/data scientist/Projet velib/data_all_Paris.jjson_2017-01-01-1483248351"))


#Histo_velib<- readLines(file("//telemaque/users/CEPE-S1-02/Bureau/data_all_Paris.jjson_2017-01-01-1483248351"))

Histo_velib <- lapply(Histo_velib,jsonlite::fromJSON)

# pour obtenir un dataFrame de toutes les dataFrame
HistoDT <- do.call('rbind',Histo_velib)
rm(Histo_velib)

#creation d'une table unique 
#format date pour l'année 

HistoDT$date <- as.POSIXct(HistoDT$download_date, origin="1970-01-01")
HistoDT$jour<- weekdays(as.Date(HistoDT$date))
HistoDT$mois<- as.factor(months(as.Date(HistoDT$date)))
HistoDT$annee<- format(HistoDT$date,"%Y")
HistoDT$jour_num<-as.POSIXlt(HistoDT$date)$mday
HistoDT$mois_num<-as.POSIXlt(HistoDT$date)$m

HistoDT$heure <- as.numeric(format(HistoDT$date, "%H"))
HistoDT$minute <- as.numeric(format(HistoDT$date, "%M"))

HistoDT$heure_minute <- paste(HistoDT$heure,HistoDT$minute ,sep="_")

location_dispo<-HistoDT[,c("date","number","available_bikes",
                           "annee","mois","heure",
                           "jour_num","jour","heure_minute","minute","download_date")]

location_dispo<-location_dispo[order(location_dispo$number,location_dispo$date),]
#variable retard

############################################################################################################################
#introduction des données météo

location_dispo$fusion<-paste(location_dispo$annee,
                             location_dispo$mois,
                             location_dispo$heure,
                             location_dispo$jour_num,
                             sep='')

meteo$fusion<-paste(meteo$annee,
                    meteo$mois,
                    meteo$heure,
                    meteo$jour_num,
                    sep='')


locat_dispo<-left_join(location_dispo, 
                       meteo[,c("ciel","precipitation",
                                "temperature","fusion")], 
                                              by ="fusion")

#introductio d'une variable semaine/week end
locat_dispo$we<- "WE" 
locat_dispo$we[locat_dispo$jour%in% c("lundi","mardi","mercredi","jeudi","vendredi")] <- "Semaine" 


#selection des variables du modele
loc_dispo<-locat_dispo[,c("number","available_bikes",
                          "jour","heure","minute","temperature","precipitation","we")]

rm(HistoDT,location_dispo,hdwd)
#transformation des varaibles en facteur
loc_dispo$jour<-as.factor(loc_dispo$jour)
loc_dispo$heure<-as.factor(loc_dispo$heure)
loc_dispo$minute<-as.factor(loc_dispo$minute)
loc_dispo$we<-as.factor(loc_dispo$we)


#création d'une liste des stations
list_station <-c(unique(loc_dispo[,"number"]))


tt<-summary(loc_dispo)
attributes(tt)
tt<-loc_dispo[which(is.na(loc_dispo$temperature)),]

#l'ensemble
RES <- data.frame(list_station,  
                      mse=1,
                      risque=1,
                      biais=1,r2=1)
resultat<-as.list(list_station)
is.list(resultat)

for(i in 1 : length(list_station))
{
  station <-list_station[i]
  print(station)
  dispo<-loc_dispo[which(loc_dispo$number==list_station[i]),]
  dispo$number<-NULL

  
  n<-nrow(dispo)
  indextrain<-sample(1:n,size=2*n/3) 
  
  rf <- randomForest(available_bikes~.,data=dispo[indextrain,],ntree=250)
  
  biais = mean(abs(dispo[-indextrain,"available_bikes"]-
                     round(as.numeric(predict(rf,dispo[-indextrain,-1])),0)))
  biais
  risque <- mean((predict(rf,dispo[-indextrain,-1],type=)-dispo[-indextrain,"available_bikes"])^2)
  risque
  
  varImpPlot(rf)
  rf$importance[order(rf$importance[,1],decreasing=T),]
 # plot(predict(rf,dispo[-indextrain,-1]),dispo[-indextrain,"available_bikes"],
  #     xlab = "valeurs rélles",ylab= "valeurs predites")
  resultat[[station]] <-rf
  RES[which(RES$list_station==list_station[i]),"mse"] <- mean(rf$mse)
  RES[which(RES$list_station==list_station[i]),"risque"] <- risque
  RES[which(RES$list_station==list_station[i]),"biais"] <- biais  
  RES[which(RES$list_station==list_station[i]),"r2"] <- mean(rf$rsq)
  print(i)
  }

rm(resultat)
rm(rf)
rm(Histo_velib)

#Sauvegarde du modele
saveRDS(resultat,file="resultat.RDS")

#
tt<-readRDS(file="//telemaque/users/CEPE-S1-02/Bureau/resultat.RDS") #global dans shiny
