
shinyServer(function(input, output, session) {
  
  source("src/server_carte.R", local = TRUE)
  source("src/server_parcours.R", local = TRUE)
  source("src/server_detail.R", local = TRUE)
  source("src/server_stats.R", local = TRUE)
  source("src/server_admin.R", local = TRUE)
  source("src/server_observe.R", local = TRUE)
  
})
