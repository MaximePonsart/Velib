#proxy <- reactive({leafletProxy("carteGeo")})
#sprox <- reactive({input$stationsProx})

setMapCircleDeparr <- function (idStation) {
  print("hello")
  leafletProxy("carteGeo") %>%
    addCircles(
      color="blue",
      lng=stations[idStation,]$longitude,
      lat=stations[idStation,]$latitude,
      layerId=as.vector("depart"),
      radius=input$stationsProx)
}

observeEvent(input$carteGeo_shape_click,{
  
  print("***1")
  event <- input$carteGeo_shape_click
  print(event$id)
  
  setMapCircleDeparr(event$id)
  updateSelectInput(session, inputId = "stationDepart", selected=event$id)
  
})

observeEvent(input$stationsProx,{
  print("***2")
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart)
})

observeEvent(input$stationDepart,{
  print("***3")
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart)
})
# session$onSessionEnded(function(){
#   obs$suspend()
# })
