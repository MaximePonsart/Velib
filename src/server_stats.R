output$main_plot <- renderPlot({
  
  hist(stations$bike_stands,
       freq=FALSE,
       xlab="capacité",
       ylab="densité",
       main="capacité des stations")
  
})

