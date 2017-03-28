
shinyUI(
  
  fluidPage(
    
    theme = shinythemes::shinytheme("united"),
    
    titlePanel("adopte un velib", windowTitle = "adopte un velib"),
    
    fluidRow(
      
      column(width = 4, source("src/ui_sidebar.R", local = TRUE)$value),

      column(width = 8,
             
             absolutePanel(top=-30, right=20,
                           sliderInput("stationsProx", "Stations + proches (en mètres)",
                                       min = 200, max = 1000, value = 600, step=200, ticks=FALSE)),
             
             tabsetPanel(
               
               source("src/ui_carte.R", local = TRUE)$value,
               source("src/ui_parcours.R", local = TRUE)$value,
               source("src/ui_detail.R", local = TRUE)$value,
               source("src/ui_stats.R", local = TRUE)$value,
               source("src/ui_admin.R", local = TRUE)$value
               
             )
             
      )
    )
    
  )
  
)
