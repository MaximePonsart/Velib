velib2<-read.csv("stations-velib-disponibilites-en-temps-reel.csv",
                 row.names=1,sep=";")
names(velib2)
hist(velib2$bike_stands,freq=FALSE,xlab="Taille",main="Hist. de la taille des stations")
lines(density(velib2$bike_stands),lwd=2,col=2)
position <- as.character(velib2$position)
vecteur <- do.call('rbind',strsplit(position,',',fixed=TRUE))
velib2$longitude <- as.numeric(vecteur[,2])
velib2$latitude <- as.numeric(vecteur[,1])
plot(latitude~longitude,data=velib2,cex=.2)
library(ggmap)
map.Decaux <- get_map(c(lon=2.35,lat=48.86), zoom =12,  maptype = "roadmap")
ggmap(map.Decaux)
map.Decaux <- ggmap(map.Decaux, extent = "device")
map.Decaux + geom_point(data = velib2, aes(x = longitude, y = latitude))

map.Decaux + geom_point(data = velib2, aes(x = longitude,
                            y = latitude,col=bonus,size=bike_stands))


library(leaflet)

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
m <- leaflet(data = velib2) %>%
  addTiles() %>%
  addCircles(~ longitude, ~ latitude, popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),
                   radius = ~ sqrt(bike_stands),
            color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
            stroke = TRUE, fillOpacity = 0.75)

m



