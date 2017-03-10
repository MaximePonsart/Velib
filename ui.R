
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(
  
  fluidPage(
    
    
    titlePanel("adopte un velib"),
    
    
    fluidRow(
      
      column(width = 4, source("ui_sidebar.R", local = TRUE)$value),

      
      column(width = 8, 
             
             tabsetPanel(
               
               source("ui_carte.R", local = TRUE)$value,
               source("ui_detail.R", local = TRUE)$value,
               source("ui_admin.R", local = TRUE)$value
               
             )
             
      )
    )
    
  )
  
)
