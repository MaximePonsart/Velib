
tabPanel("carte",
         leafletOutput("carteGeo"),
         #filtre les stations avec les vélos disponibles
         radioButtons("allstation", "stations disponibles :",
                      c("seulement" = "only", "toutes" = "all"),
                      inline = T)
)





