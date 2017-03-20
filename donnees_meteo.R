require(jsonlite)
require(RCurl)
require(curl)
library(RCurl)
install.packages("darksky")
library(darksky)
install.packages("purrr")
library(purrr)
install.packages("RJSONIO")
library(RJSONIO)
library(plyr)


url <- 'https://api.darksky.net/forecast/9778cdc6ddc2eaf7b6854ad412c21eec/48.866667,2.333333'
Sys.setenv(DARKSKY_API_KEY = "9778cdc6ddc2eaf7b6854ad412c21eec")



#meteo actuelle
tm <- get_current_forecast(48.866667,2.333333,date,units="si",language = "fr")
tm1<-tm$hourly[1,c("time","summary","precipIntensity","temperature")]



########      historique météo

date.range <- seq.Date(from=as.Date('2016-07-01'), to=as.Date('2016-08-01'), by='1 day')

hdwd <- data.frame()

for(i in seq_along(date.range)) {
  tmp<-get_forecast_for(48.866667,2.333333,paste(date.range[i],'T12:00:00',sep=""),units="si",language = "fr")
  tmp1<-as.data.frame(tmp)
  tmp2<-data.frame(tmp1$hourly.time,tmp1$hourly.summary,tmp1$hourly.precipIntensity,tmp1$hourly.temperature)
  hdwd <- rbind(hdwd,tmp2)
}


#mise en forme histo  
hdwd$date <- as.Date(hdwd$tmp1.hourly.time,tz="CET")
hdwd$jour<- weekdays(as.Date(hdwd$date))
hdwd$mday<-mday(hdwd$date)
hdwd$mois<- format(as.Date(hdwd$date,format = "%Y-%m"),"%m")

hdwd$heure<- as.numeric(substr(hdwd$tmp1.hourly.time,12,13))
hdwd$time <- strftime(hdwd$date, format="%H:%M")

names(hdwd)[2]<-paste("ciel")
names(hdwd)[3]<-paste("precipitation")
names(hdwd)[4]<-paste("temperature")

hdwd1<-hdwd[,c(2,3,4,7,8,9)]
