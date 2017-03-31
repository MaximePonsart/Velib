output$stationsSelect <- renderTable({
  selection <- c()
  if (input$stationDepart != "0") selection <- append(selection, input$stationDepart)
  if (input$stationArrivee != "0") selection <- append(selection, input$stationArrivee)
  stations[selection,
           c("number",names(stations)[-which(names(stations) %in% c("number","contract_name","bonus","position","longitude","latitude"))])]
})

# output$distanceGeoStationsSelect <- renderText({
#   if (input$stationDepart != "0" && input$stationArrivee != "0")
#       getDistanceGeoStations(input$stationDepart, input$stationArrivee)
# })

output$stationsProches <- renderTable({
  if (input$choixStationDepartArrivee == "from" && input$stationDepart != "0") {
    s <- stations[input$stationDepart,]
    res <- getProchesStations(s$latitude, s$longitude, "distance", input$stationsProx)
  } else if (input$choixStationDepartArrivee == "to" && input$stationArrivee != "0") {
    s <- stations[input$stationArrivee,]
    res <- getProchesStations(s$latitude, s$longitude, "distance",input$stationsProx)
  }
  if (exists("res"))
    res[, c("number","dist",names(res)[-which(names(res) %in% c("number","dist","contract_name","bonus","position","longitude","latitude"))])]
})