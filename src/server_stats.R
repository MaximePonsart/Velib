output$main_plot <- renderPlot({
  
  hist(stations$bike_stands,
       freq=FALSE,
       xlab="capacit�",
       ylab="densit�",
       main="capacit� des stations")
  
  lines(density(stations$bike_stands),lwd=2,col=2)
  
})

