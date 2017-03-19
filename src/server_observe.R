
setMapCircleDeparr <- function (idStation, deparr) {
  leafletProxy("carteGeo") %>%
    addCircles(
      color=ifelse(deparr=="depart","orange","green"),
      lng=stations[idStation,]$longitude,
      lat=stations[idStation,]$latitude,
      layerId=as.vector(deparr),
      radius=input$stationsProx)
}


observeEvent(input$carteGeo_shape_click,{
  
  print("***1")
  event <- input$carteGeo_shape_click
  
  eventIsArrivee <- event$id==input$stationArrivee || event$id=="arrivee"
  
  cible <- ifelse(eventIsArrivee, input$stationArrivee, event$id)
  setMapCircleDeparr(cible, "depart")
  updateSelectInput(session, inputId = "stationDepart", selected=cible)
  
  if (eventIsArrivee) {
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
  if (input$stationDepart != "0") setMapCircleDeparr(input$stationDepart, "depart")
})

observeEvent(input$stationArrivee,{
  print("***4")
  if (input$stationArrivee != "0") setMapCircleDeparr(input$stationArrivee, "arrivee")
})

observeEvent(input$go,{
  # print("***5")
  # if(input$go > 0){
  #   print("***5bis")
  #     #acquisition des positions géo des adresses de départ & arrivée + positionnement carte :
  #     if (input$adresseDepart != "") {
  #       adresseDepartGeo <- geocode(input$adresseDepart)
  #       leafletProxy("carteGeo") %>%
  #         addMarkers(lng=adresseDepartGeo[1,"lon"], lat=adresseDepartGeo[1,"lat"],
  #                    icon=greenLeafIcon)
  #     }
  #     if (input$adresseArrivee != "") {
  #       adresseArriveeGeo <- geocode(input$adresseArrivee)
  #       leafletProxy("carteGeo") %>%
  #         addMarkers(
  #           lng=adresseArriveeGeo[1,"lon"], lat=adresseArriveeGeo[1,"lat"],
  #           icon=redLeafIcon)
  #     }
  # }
})

observeEvent(input$goCtrlDep,{
  if(input$goCtrlDep > 0){
    #acquisition de la position géo + positionnement carte :
    if (input$adresseDepart != "") {
      adresseDepartGeo <- geocode(input$adresseDepart)
      leafletProxy("carteGeo") %>%
        addMarkers(lng=adresseDepartGeo[1,"lon"], lat=adresseDepartGeo[1,"lat"],
                   icon=greenLeafIcon)
    }
  }
})

observeEvent(input$goCtrlArr,{
  if(input$goCtrlArr > 0){
    #acquisition de la position géo + positionnement carte :
    if (input$adresseArrivee != "") {
      adresseArriveeGeo <- geocode(input$adresseArrivee)
      leafletProxy("carteGeo") %>%
        addMarkers(
          lng=adresseArriveeGeo[1,"lon"], lat=adresseArriveeGeo[1,"lat"],
          icon=redLeafIcon)
    }
  }
})