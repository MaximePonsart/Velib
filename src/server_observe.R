#stationDepBak <<- "0"

setMapCircleDeparr <- function (idStation, deparr) {
  leafletProxy("carteGeo") %>%
    addCircles(
      color=ifelse(deparr=="depart","orange","green"),
      lng=stations[idStation,]$longitude,
      lat=stations[idStation,]$latitude,
      layerId=as.vector(deparr),
      radius=input$stationsProx)
}

# clearPopup <- function(idStation) {
#   print("hi")
#   print(idStation)
#   if (idStation != "0") {
#     s <- stations[idStation,]
#     print("****************************")
#     print(s)
#     leafletProxy("carteGeo") %>%
#       addCircles(
#         lng=s$longitude,
#         lat=s$latitude,
#         #layerId=as.vector(s$number),
#         #popup = ~ sprintf("%s<br>%s vélos disponibles",name,as.character(s$available_bikes)),
#         radius = ~ sqrt(s$bike_stands)*10,
#         #color = ~ ColorPal(s$available_bikes / (s$available_bikes + s$available_bike_stands)),
#         stroke = TRUE, fillOpacity = 0.75) 
#   }
# }

observeEvent(input$carteGeo_shape_click,{
  
  print("***1")
  event <- input$carteGeo_shape_click
  #print(event$id)
  #print(input$stationArrivee)
  
  eventIsArrivee <- event$id==input$stationArrivee || event$id=="arrivee"
  
  cible <- ifelse(eventIsArrivee, input$stationArrivee, event$id)
  setMapCircleDeparr(cible, "depart")
  updateSelectInput(session, inputId = "stationDepart", selected=cible)
  #stationDepBak <<- event$id
  #print("***1bis")
  #print(event$id)
  #print(input$stationArrivee)
  if (eventIsArrivee) {
    #print("goooooooooooooooooo")
    leafletProxy("carteGeo") %>%
      removeShape("arrivee")
    updateSelectInput(session, inputId = "stationArrivee", selected="0")
  }
  
})

observeEvent(input$stationsProx,{
  print("***2")
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart, "depart")
  if (input$stationArrivee != "0") setMapCircleDeparr(input$stationArrivee, "arrivee")
})

observeEvent(input$stationDepart,{
  print("***3")
  if (input$stationDepart != "0") {
    #clearPopup(stationDepBak)
    setMapCircleDeparr(input$stationDepart, "depart")
  }
  #stationDepBak <<- input$stationDepart
})

observeEvent(input$stationArrivee,{
  print("***4")
  if (input$stationArrivee != "0") setMapCircleDeparr(input$stationArrivee, "arrivee")
})
