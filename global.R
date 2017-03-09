#configuration globale
library(leaflet)
#library(shiny)

print("***debut")

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

#get stations
stations<-read.csv("stations-velib-disponibilites-en-temps-reel.csv",
                   row.names=1,sep=";")
position <- as.character(stations$position)
v <- do.call('rbind',strsplit(position,',',fixed=TRUE))
stations$longitude <- as.numeric(v[,2])
stations$latitude <- as.numeric(v[,1])
stations_nom <- levels(stations$name)
v <- NULL

print("***fin")
