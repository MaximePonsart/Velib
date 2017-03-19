### Calcul temps de trajet minimum marche + velo 

require(ggmap)
#install.packages("placement")
library(placement)
library(geosphere)
library(plyr)


depart<-geocode("60 rue etienne dolet,92240 Malakoff")
depart

arrivee<-geocode("37 rue du louvres,75000 Paris")
arrivee


####  positions stations

pos1 <-read.csv("stations-velib-disponibilites-en-temps-reel.csv",sep=";")
names(pos1)
pos2<-pos1[,c(1,4)]
position <- as.character(pos2$position)
vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
pos2$lon <- as.numeric(vecteur[,2])
pos2$lat <- as.numeric(vecteur[,1])
pos2$position<-NULL



#  distance depart - stations depart les plus proches

test <- data.frame()

for(i in 1:nrow(pos2)) {
  hist <-distm (c(depart$lon, depart$lat), c(pos2$lon[i], pos2$lat[i]), fun = distHaversine)
  test <- rbind(test,hist)
}
result<-cbind(pos2,test)
result1<-result[result$V1<500,]



####  temps marche depart - stations de depart les plus proches


start <- paste(depart$lat,depart$lon, sep=",")
result2<-paste(result1$lat,result1$lon, sep=",")

marche1<- data.frame()

for(i in 1:nrow(result1)) {
   howfar <- drive_time(address=start, dest=result2[i], auth="standard_api",
                        privkey="", clean=FALSE, add_date='today',
                        verbose=FALSE, travel_mode="walking",
                        units="metric")
  
    howfar <- howfar[,c("origin","destination","time_mins")]
  
    station<-result1$number[i]
    
    howfar<-cbind(station,howfar)
    marche1<-rbind(howfar,marche1) 
}
names(marche1)[1]<-paste("num_depart")
names(marche1)[2]<-paste("point_depart")
names(marche1)[3]<-paste("station_depart")
names(marche1)[4]<-paste("marche_depart")



#distance point arrivee - stations arrivée les plus proches

testarr <- data.frame()

for(i in 1:nrow(pos2)) {
  hist1 <-distm (c(arrivee$lon, arrivee$lat), c(pos2$lon[i], pos2$lat[i]), fun = distHaversine)
  testarr <- rbind(testarr,hist1)
}
resultarr<-cbind(pos2,testarr)
resultarr1<-resultarr[resultarr$V1<400,]


#### temps marche arrivee - stations arrivées les plus proches

end <- paste(arrivee$lat,arrivee$lon, sep=",")
resultarr2<-paste(resultarr1$lat,resultarr1$lon, sep=",")

marchearr1<- data.frame()

 for(i in 1:nrow(resultarr1)) {
        howfararr <- drive_time(address=end, dest=resultarr2[i], auth="standard_api",
                      privkey="", clean=FALSE, add_date='today',
                      verbose=FALSE, travel_mode="walking",
                      units="metric")
       
        howfararr <- howfararr[,c("origin","destination","time_mins")]
         
        stationarr<-resultarr1$number[i]
        howfararr<-cbind(stationarr,howfararr)
        marchearr1<-rbind(howfararr,marchearr1) 
 }

names(marchearr1)[1]<-paste("num_arrivee")
names(marchearr1)[2]<-paste("point_arrivee")
names(marchearr1)[3]<-paste("station_arrivee")
names(marchearr1)[4]<-paste("marche_arrivee")



# CALCUL TEMPS VELO entres stations de depart et stations d arrivée


velo1<-data.frame()

for(i in 1:nrow(result1)) {
      for(j in 1:nrow(resultarr1)) {
                howf <- drive_time(address=result2[i], dest=resultarr2[j], auth="standard_api",
                       privkey="", clean=FALSE, add_date='today',
                       verbose=FALSE, travel_mode="bicycling",
                       units="metric")
                howf <- howf[,c("origin","destination","time_mins")]
                stationarr<-resultarr1$number[j]
                station<-result1$number[i]
                howf<-cbind(howf,stationarr,station)
                velo1<-rbind(howf,velo1)
      }

}

names(velo1)[4]<-paste("num_arrivee")
names(velo1)[5]<-paste("num_depart")
names(velo1)[1]<-paste("station_depart")
names(velo1)[2]<-paste("station_arrivee")
names(velo1)[3]<-paste("temps_velo")



marche2<-marche1[,c("point_depart","num_depart","marche_depart")]
velo2<-velo1[,c("num_depart","num_arrivee","temps_velo")]
marchearr2<-marchearr1[,c("num_arrivee","point_arrivee","marche_arrivee")]

temps<-merge(marche2,velo2, by = c("num_depart"))
temps1<-merge(temps,marchearr2,by = c("num_arrivee"),all=TRUE)


#calcul temps de trajet
temps1$duree<-temps1$marche_depart+temps1$temps_velo+temps1$marche_arrivee

#determination trajet minimum
est<-temps1[which.min(temps1$duree),]
