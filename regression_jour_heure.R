
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

setwd("//telemaque/users/CEPE-S1-02/Bureau")
#setwd("C:/Users/sevdr/Desktop/data scientist/Projet velib")
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

#Histo_velib <- readLines(file("C:/Users/sevdr/Desktop/data scientist/data_all_Paris.jjson.txt"))

Histo_velib <- readLines(file("//telemaque/users/CEPE-S1-02/Bureau/data_all_Paris.jjson.txt"))


Histo_velib <- lapply(Histo_velib,jsonlite::fromJSON)
# pour obtenir un dataFrame de toutes les dataFrame
HistoDT <- do.call('rbind',Histo_velib)

#format date pour l'année 

HistoDT$date <- as.POSIXct(HistoDT$download_date, origin="1970-01-01")
HistoDT$jour<- weekdays(as.Date(HistoDT$date))
HistoDT$mois<- months(as.Date(HistoDT$date))
HistoDT$annee<- format(HistoDT$date,"%Y")
HistoDT$jour_num<-as.POSIXlt(HistoDT$date)$mday

HistoDT$heure <- as.numeric(format(HistoDT$date, "%H"))
HistoDT$minute <- as.numeric(format(HistoDT$date, "%M"))

HistoDT$heure_minute <- paste(HistoDT$heure,HistoDT$minute ,sep="_")


location_dispo<-HistoDT[,c("date","number","available_bikes",
                           "annee","mois","heure",
                           "jour_num","jour","heure_minute")]
location_dispo<-location_dispo[order(location_dispo$number,location_dispo$date),]
#variable retard

#calcul des stations les plus proches  (distance entre les stations)
ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else if (tolower(triangle.to.replace) == "upper") tri <- upper.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}

GeoDistanceInMetresMatrix <- function(df.geopoints){
  # Returns a matrix (M) of distances between geographic points.
  # M[i,j] = M[j,i] = Distance between (df.geopoints$lat[i], df.geopoints$lon[i]) and
  # (df.geopoints$lat[j], df.geopoints$lon[j]).
  # The row and column names are given by df.geopoints$name.
  
  GeoDistanceInMetres <- function(g1, g2){
    # Returns a vector of distances. (But if g1$index > g2$index, returns zero.)
    # The 1st value in the returned vector is the distance between g1[[1]] and g2[[1]].
    # The 2nd value in the returned vector is the distance between g1[[2]] and g2[[2]]. Etc.
    # Each g1[[x]] or g2[[x]] must be a list with named elements "index", "lat" and "lon".
    # E.g. g1 <- list(list("index"=1, "lat"=12.1, "lon"=10.1), list("index"=3, "lat"=12.1, "lon"=13.2))
    DistM <- function(g1, g2){
      require("Imap")
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=g1$lat, lon.1=g1$lon, lat.2=g2$lat, lon.2=g2$lon, units="m")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "lat", "lon")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$name
  colnames(mat.distances) <- df.geopoints$name
  
  return(mat.distances)
}

#### calcul de la matrice de distance entre les stations :

stations <- subset(velib2, select=c(number,latitude,longitude))
names(stations) <- c("name","lat","lon")
distance <- round(GeoDistanceInMetresMatrix(stations)/1000,3)




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


location_dispo<-left_join(location_dispo, 
                          meteo[,c("ciel","precipitation",
                                   "temperature","fusion")], 
                          by ="fusion")


#modele statistique 
loc_dispo<-location_dispo[,c("number","available_bikes",
                             "jour","heure","precipitation",
                             "temperature")]
loc_dispo$heure<-as.factor(loc_dispo$heure)
loc_dispo$jour<-as.factor(loc_dispo$jour)

list_station <-c(unique(loc_dispo[,"number"]))

RES <- data.frame(list_station,          
                  reslinaire=1,
                  resbestglm=1,
                  reslasso=1,
                  resridge=1,
                  reselastique=1,
                  ressvm=1,
                  ressvr=1,
                  resforet=1,
                  resboosting=1,
                  resboosting2=1)


for(i in 1 : length(list_station))
{
  dispo<-loc_dispo[which(loc_dispo$number==list_station[i]),]
  dispo$number<-NULL

  n<-nrow(dispo)
  indextrain<-sample(1:n,size=n/2) 
  

 #MODELE LINEAIRE
  dispo.lm=glm(available_bikes~.,data=dispo[indextrain,],family=poisson) 
  summary(dispo.lm)
  # Estimation du risque sur l'echantillon de validation
  # Creation d'un vecteur qui contiendra les risques estimes pour les differentes methodes testees
  risque=mean((predict(dispo.lm,dispo[-indextrain,-1],type="response")-dispo[-indextrain,"available_bikes"])^2)       
  RES[which(RES$list_station==list_station[i]),"reslinaire"] <- risque
    y=dispo$available_bikes
  x=dispo[,c("jour","heure","precipitation","temperature")]
  xy=cbind(x,y)
  bic=bestglm(xy[indextrain,],family=poisson,IC="BIC")
  # Estimation du risque sur l'echantillon de validation
  formula(bic$BestModel)
  names(xy)[ncol(xy)]="y"
  names(xy)
  cb.selB=lm(formula(bic$BestModel),data=xy[indextrain,])
  risque=mean((predict(cb.selB,xy[-indextrain,])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"resbestglm"] <- risque
  
  # Regression lineaire ridge
  # matrice de plan d'experience
  X=model.matrix(available_bikes~.-1,data=dispo)
  Xapp=X[indextrain,]
  Y=dispo$available_bikes[indextrain]
  dispo.ri=glmnet(Xapp,Y,alpha=)
  
  lopt=cv.glmnet(Xapp,Y,alpha=0)$lambda.min
  cv.glmnet(Xapp,Y,alpha=0)$cvm
  coef(dispo.ri,s=lopt)
  dispo.ri.opt=glmnet(Xapp,Y,alpha=0,lambda=lopt)
  risque=mean((predict(dispo.ri,newx=X[-indextrain,])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"resridge"] <- risque
  
  # Regression lineaire lasso
  dispo.la=glmnet(Xapp,Y,alpha=1)
  lopt=cv.glmnet(Xapp,Y,alpha=1)$lambda.min
  dispo.la.opt=glmnet(Xapp,Y,alpha=1,lambda=lopt)
  risque=mean((predict(dispo.la.opt,newx=X[-indextrain,])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"reslasso"] <- risque
  
  # Regression lineaire elastic net
  dispo.en=glmnet(Xapp,Y,alpha=0.5)
  lopt=cv.glmnet(Xapp,Y,alpha=0.5)$lambda.min
  coef(dispo.en,s=lopt)
  dispo.en.opt=glmnet(Xapp,Y,alpha=0.5,lambda=lopt)
  risque=mean((predict(dispo.en.opt,newx=X[-indextrain,])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"reselastique"] <- risque
  
  
  # SVR lineaire
  dispo.svm.opt <- tune.svm(available_bikes~.,data=dispo[indextrain,],cost=10^(-3:2),kernel="linear") 
  dispo.svm.opt$best.parameters$cost
  risque <- mean((predict(dispo.svm.opt$best.model,dispo[-indextrain,-1])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"ressvr"] <- risque
  # SVR non lineaire
  dispo.svm.radopt <- tune.svm(available_bikes~.,data=dispo[indextrain,],cost=10^(-3:2),gamma=10^(-3:2),kernel="radial") 
  copt <- dispo.svm.radopt$best.parameters$cost
  gopt <- dispo.svm.radopt$best.parameters$gamma
  risque <- mean((predict(dispo.svm.radopt$best.model,dispo[-indextrain,-1])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"ressvm"] <- risque
  
  #FORET ALEATOIRE
  rf <- randomForest(available_bikes~.,data=dispo[indextrain,],ntree=500)
  risque <- mean((predict(rf,dispo[-indextrain,-1],type=)-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"resforet"] <- risque
  
  
   varImpPlot(rf)
  rf$importance[order(rf$importance[,1],decreasing=T),]
  # Boosting
    boost <- gbm(available_bikes~.,data=dispo[indextrain,],distribution="poisson",
               n.trees=100,shrinkage=0.05,bag.fraction=1,cv.folds=10)
  
  risque<- mean((predict(rf,dispo[-indextrain,-1])-dispo[-indextrain,"available_bikes"])^2)
  RES[which(RES$list_station==list_station[i]),"resboosting"] <- risque
  # Boosting
  
  boost <- gbm(available_bikes~.,data=dispo[indextrain,],
               distribution="poisson",
               n.trees=100,shrinkage=0.05,bag.fraction=1,cv.folds=10)
  
  Bopt<-gbm.perf(boost,method="cv")
  fitted<-predict(boost,newdata=dispo[-indextrain,],n.trees=Bopt)
  risque <- mean((fitted-dispo[-indextrain,"available_bikes"])^2)
    RES[which(RES$list_station==list_station[i]),"resboosting2"] <- risque
}



