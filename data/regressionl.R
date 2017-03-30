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
install.packages("caret")
install.packages("doParallel")


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
library(RCurl)
library(curl)
library(darksky)
library(purrr)
library(RJSONIO)
library(plyr)
library(caret)
library(doParallel)
library(readr)
library(ggplot2)
library(plotly)


#importation des données météo

setwd("//telemaque/users/CEPE-S2-02/Bureau")
#setwd("C:/Users/sevdr/Desktop/data scientist/Projet velib")

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
Histo_velib <- readLines(file
                ("//telemaque/users/CEPE-S2-02/Bureau/data_all_Paris.jjson_2017-03-01-1488345976"))


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
HistoDT$mois_num<-months(as.Date(HistoDT$date))

HistoDT$heure <- as.numeric(format(HistoDT$date, "%H"))
HistoDT$minute <- as.numeric(format(HistoDT$date, "%M"))

HistoDT$heure_minute <- paste(HistoDT$heure,HistoDT$minute ,sep="_")

location_dispo<-HistoDT[,c("date","number","available_bikes",
                           "annee","mois","heure",
                           "jour_num","jour","heure_minute","minute","download_date")]
rm(HistoDT)
location_dispo<-location_dispo[order(location_dispo$number,location_dispo$date),]
#variable retard
meteo<-readRDS(file="//telemaque/users/CEPE-S2-02/Bureau/meteo.RDS")
tt<-meteo[which(meteo$mois=="février" & meteo$annee =="2017"),]

############################################################################################################################
#introduction des données météo
location_dispo$fusion<-paste(location_dispo$annee,
                             location_dispo$mois,
                             location_dispo$heure,
                             location_dispo$jour_num,
                             sep='_')

meteo$fusion<-paste(meteo$annee,
                    meteo$mois,
                    meteo$heure,
                    meteo$jour_num,
                    sep='_')


locat_dispo<-left_join(location_dispo, 
                       meteo[,c("ciel","precipitation",
                                "temperature","fusion")], 
                       by ="fusion")

locat_dispo$fusion<-NULL

############################################################################################################################
#introduction des données vacances

locat_dispo$mois_num<- as.numeric(format(locat_dispo$date, "%m"))
locat_dispo$fusion<-paste(locat_dispo$jour_num,
                          locat_dispo$mois_num,
                          locat_dispo$annee,
                          sep='_')

jour <- as.character(vacance$date)
vecteur <- do.call('rbind',strsplit(jour,'/',fixed=TRUE))

vacance$jour_num <- as.numeric(vecteur[,1])
vacance$mois <- as.numeric(vecteur[,2])
vacance$annee <- paste("20",as.numeric(vecteur[,3]),sep="")


vacance$fusion<-paste(vacance$jour_num,
                      vacance$mois,
                      vacance$annee,
                      sep='_')
vacance$mois<-NULL
vacance$jour_num<-NULL
vacance$annee<-NULL
vacance$date<-NULL

loca_dispo<-left_join(locat_dispo, 
                      vacance, 
                      by ="fusion")

locat_dispo<-loca_dispo

rm(loca_dispo,location_dispo)
#introductio d'une variable semaine/week end
locat_dispo$we<- "WE" 
locat_dispo$we[locat_dispo$jour%in% c("lundi","mardi","mercredi","jeudi","vendredi")] <- "Semaine" 


#introductio d'une variable jour/soir
locat_dispo$we<- "WE" 
locat_dispo$we[locat_dispo$jour%in% c("lundi","mardi","mercredi","jeudi","vendredi")] <- "Semaine" 

locat_dispo$atemp<- 0 
locat_dispo$atemp[locat_dispo$heure<8]=1
locat_dispo$atemp[locat_dispo$heure>=22]=2
locat_dispo$atemp[locat_dispo$heure>9 & locat_dispo$heure<18]=3
locat_dispo$atemp[locat_dispo$heure==8]=4
locat_dispo$atemp[locat_dispo$heure==9]=5
locat_dispo$atemp[locat_dispo$heure==20 | locat_dispo$heure==21]=6

#selection des variables du modele

loc_dispo<-locat_dispo[,c("number","available_bikes","date",
                          "jour","heure","minute","temperature",
                          "precipitation","vacance")]

#transformation des varaibles en facteur
loc_dispo$jour<-as.factor(loc_dispo$jour)
loc_dispo$heure<-as.factor(loc_dispo$heure)
loc_dispo$minute<-as.factor(loc_dispo$minute)
loc_dispo$vacance<-as.factor(loc_dispo$vacance)
levels(loc_dispo$vacance)[1]<-"notholidays"
levels(loc_dispo$vacance)[2]<-"holidays"

loc_dispo$atemp<-as.factor(loc_dispo$atemp)

#création d'une liste des stations
list_station <-c(unique(loc_dispo[,"number"]))

#Modele 1 : modele avec les variable initiale sans prise en compte des valeur décalées
RES <- data.frame(list_station,  
                  mse=1,
                  risque=1,
                  biais=1,
                  biaisprtc=1,
                  corre=1)

resultat<-as.list(list_station)

for(i in 1 : length(list_station))
{
  station <-list_station[i]
  print(station)
  dispo<-loc_dispo[which(loc_dispo$number==station),]
  
  #élaboration du modèle
  dispo$number<-NULL
  dispo$date<-NULL
  n<-2*nrow(dispo)/3
  indextrain<-1:n
  
  #modele random forest
  rf <- randomForest(available_bikes~.,data=dispo[indextrain,],ntree=250)
  rf

  biais = mean(abs(dispo[-indextrain,"available_bikes"]-
                     round(as.numeric(predict(rf,dispo[-indextrain,-1])),0)))
  biais
  
  biaisprtc = mean(abs(dispo[-indextrain,"available_bikes"]-
                         round(as.numeric(predict(rf,dispo[-indextrain,-1])),0)))*100/
                   mean(dispo[-indextrain,"available_bikes"])
  biaisprtc

  risque <- mean((predict(rf,dispo[-indextrain,-1],type=)
                  -dispo[-indextrain,"available_bikes"])^2)
  risque
  
  
  corre<-cor(dispo[-indextrain,"available_bikes"],predict(rf,dispo[-indextrain,-1]))
  corre 
  
  resultat[[station]] <-rf
  RES[which(RES$list_station==list_station[i]),"mse"] <- mean(rf$mse)
  RES[which(RES$list_station==list_station[i]),"risque"] <- risque
  RES[which(RES$list_station==list_station[i]),"biais"] <- biais  
  RES[which(RES$list_station==list_station[i]),"r2"] <- mean(rf$rsq)
  RES[which(RES$list_station==list_station[i]),"biaisprtc"] <- biaisprtc
  RES[which(RES$list_station==list_station[i]),"corre"] <- corre
  print(corre)
  print(biaisprtc)
  print(i)
}

#Sauvegarde du modele
saveRDS(resultat,file="resultat.RDS")
#recuperation du modele
resultat<-readRDS(file="resultat.RDS")

#
