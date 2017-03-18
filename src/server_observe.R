obs <- observe({
  event <- input$carteGeo_shape_click
  if (is.null(event))
    return()
  #print(event) 
  #input$stationDepart <- event$id
  updateSelectInput(session, inputId = "stationDepart", selected=event$id)
})

session$onSessionEnded(function(){
  obs$suspend()
})
