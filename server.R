
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {
  
  source("src/server_carte.R", local = TRUE)
  source("src/server_parcours.R", local = TRUE)
  source("src/server_detail.R", local = TRUE)
  source("src/server_admin.R", local = TRUE)
  source("src/server_observe.R", local = TRUE)
  
})
