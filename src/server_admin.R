
filePath <- eventReactive(input$browse, {
  file.choose()
})

output$chosenPath <- renderText({

})

output$log <- renderText({
  winput <<- filePath()
  newModelRF(input$periodeRF)
})