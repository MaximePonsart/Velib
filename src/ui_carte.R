
tabPanel("carte",
         leafletOutput("carteGeo", height="550"),
         #filtre les stations avec les vélos disponibles
         radioButtons("allstation", "stations disponibles :",
                      c("seulement" = "only", "toutes" = "all"),
                      inline = T)
)





