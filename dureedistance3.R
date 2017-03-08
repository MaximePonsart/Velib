require(ggmap)
#install.packages("placement")
library(placement)
library(geosphere)



depart<-geocode("60 rue etienne dolet,92240 Malakoff")
depart

arrivee<-geocode("37 rue du louvres,75000 Paris")
arrivee


######pos stations

pos1 <-read.csv("stations-velib-disponibilites-en-temps-reel.csv",sep=";")
names(pos1)
pos2<-pos1[,c(1,4)]
position <- as.character(pos2$position)
vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
pos2$lon <- as.numeric(vecteur[,2])
pos2$lat <- as.numeric(vecteur[,1])
pos2$position<-NULL



#  distance depart stations

test <- data.frame()

for(i in 1:nrow(pos2)) {
  hist <-distm (c(depart$lon, depart$lat), c(pos2$lon[i], pos2$lat[i]), fun = distHaversine)
  test <- rbind(test,hist)
}
result<-cbind(pos2,test)
result1<-result[result$V1<500,]


####    marche depart


start <- paste(depart$lat,depart$lon, sep=",")
result2<-paste(result1$lat,result1$lon, sep=",")

marche1<- data.frame()

for(i in 1:nrow(result1)) {
   howfar <- drive_time(address=start, dest=result2[i], auth="standard_api",
                        privkey="", clean=FALSE, add_date='today',
                        verbose=FALSE, travel_mode="walking",
                        units="metric")
  
    howfar <- howfar[,c("origin","destination","time_txt")]
  
    station<-result1$number[i]
    
    howfar<-cbind(station,howfar)
    marche1<-rbind(howfar,marche1) 
}
names(marche1)[1]<-paste("num_depart")
names(marche1)[2]<-paste("point_depart")
names(marche1)[3]<-paste("station_depart")
names(marche1)[4]<-paste("marche_depart")



#distance arrivee stations

testarr <- data.frame()

for(i in 1:nrow(pos2)) {
  hist1 <-distm (c(arrivee$lon, arrivee$lat), c(pos2$lon[i], pos2$lat[i]), fun = distHaversine)
  testarr <- rbind(testarr,hist1)
}
resultarr<-cbind(pos2,testarr)
resultarr1<-resultarr[resultarr$V1<400,]


####    marche arrivee

end <- paste(arrivee$lat,arrivee$lon, sep=",")
resultarr2<-paste(resultarr1$lat,resultarr1$lon, sep=",")

marchearr1<- data.frame()

 for(i in 1:nrow(resultarr1)) {
        howfararr <- drive_time(address=end, dest=resultarr2[i], auth="standard_api",
                      privkey="", clean=FALSE, add_date='today',
                      verbose=FALSE, travel_mode="walking",
                      units="metric")
       
        howfararr <- howfararr[,c("origin","destination","time_txt")]
         
        stationarr<-resultarr1$number[i]
        howfararr<-cbind(stationarr,howfararr)
        marchearr1<-rbind(howfararr,marchearr1) 
 }

names(marchearr1)[1]<-paste("num_arrivee")
names(marchearr1)[2]<-paste("point_arrivee")
names(marchearr1)[3]<-paste("station_arrivee")
names(marchearr1)[4]<-paste("marche_arrivee")


# CALCUL TEMPS VELO


velo1<-data.frame()

for(i in 1:nrow(result1)) {
      for(j in 1:nrow(resultarr1)) {
                howf <- drive_time(address=result2[i], dest=resultarr2[j], auth="standard_api",
                       privkey="", clean=FALSE, add_date='today',
                       verbose=FALSE, travel_mode="bicycling",
                       units="metric")
                howf <- howf[,c("origin","destination","time_txt")]
                stationarr<-resultarr1$number[j]
                howf<-cbind(howf,stationarr)
                velo1<-rbind(howf,velo1) 
      }
#  stationdep<-result1$number[i]
}

names(velo1)[4]<-paste("num_arrivee")
names(velo1)[1]<-paste("station_depart")
names(velo1)[2]<-paste("station_arrivee")
names(velo1)[3]<-paste("temps_velo")



temps<-merge(marche1,velo1, by = c("station_depart"))
temps1<-merge(temps,marchearr1,by = c("station_arrivee","num_arrivee"),all.x=TRUE,all.y=TRUE)
