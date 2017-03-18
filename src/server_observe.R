reactive({input$stationsProx})

observeEvent(input$carteGeo_shape_click,{
  
  event <- input$carteGeo_shape_click
  print(event$id)
  
  updateSelectInput(session, inputId = "stationDepart", selected=event$id)
  
  #deparr <- "depart"
  #i <- ifelse(deparr=="depart", input$stationDepart, input$stationArrivee)
  proxy <- leafletProxy("carteGeo")
  proxy %>%
  addCircles(
    color="blue",
    lng=stations[event$id,]$longitude,
    lat=stations[event$id,]$latitude,
    layerId=as.vector("depart"),
    radius=input$stationsProx)
  
})

# observeEvent(input$stationsProx,{
#   
#   proxy <- leafletProxy("carteGeo")
#   proxy %>%
#     addCircles(
#       color="blue",
#       lng=stations[input$stationDepart,]$longitude,
#       lat=stations[input$stationDepart,]$latitude,
#       layerId=as.vector("depart"),
#       radius=input$stationsProx)
#   
# })

# session$onSessionEnded(function(){
#   obs$suspend()
# })
