
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(leaflet)

shinyServer(function(input, output) {
  
  ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
  
  #get stations
  
  stations<-read.csv("stations-velib-disponibilites-en-temps-reel.csv",
                     row.names=1,sep=";")
  position <- as.character(stations$position)
  v <- do.call('rbind',strsplit(position,',',fixed=TRUE))
  stations$longitude <- as.numeric(v[,2])
  stations$latitude <- as.numeric(v[,1])
  
  
  source("server_carte.R", local = TRUE)
})
