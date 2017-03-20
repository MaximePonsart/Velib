


date<-format(Sys.time(), "%a %d %b %X %Y")
date

url <- 'https://api.darksky.net/forecast/9778cdc6ddc2eaf7b6854ad412c21eec/48.866667,2.333333'
tm <- get_current_forecast(48.866667,2.333333,date,units="si",language = "fr")
tm1<-tmp$hourly[1,c("summary","precipIntensity","temperature")]
