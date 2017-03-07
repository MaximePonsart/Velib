require(ggmap)

depart<-geocode("60 rue etienne dolet,92240 Malakoff")
depart

arrivee<-geocode("37 rue du louvres,75000 Paris")
arrivee


install.packages("gmapsdistance")
library("gmapdistance", lib.loc="\\\\telemaque/users/CEPE-S1-04/R/win-library/3.3")


time <- gmapsdistance(origin = paste(depart$lat, depart$lon, sep = "+"),destination = paste(arrivee$lat, arrivee$lon, sep = "+"), mode="bicycling")
time
