tabPanel("détail",
         
         h2("Stations sélectionnées"),
         tableOutput(outputId = "stationsSelect"),
         
         h2("Distance géo entre stations sélectionnées"),
         textOutput(outputId = "distanceGeoStationsSelect"),
         
         h2("Stations proches des stations sélectionnées"),
         fluidRow(
           column(width=6,
                  radioButtons("choixStationDepartArrivee", "Station :",
                               c("départ" = "from", "arrivée" = "to"),
                               inline = T)
           ),
           column(width=6,
                  sliderInput("stationsProx", "Stations + proches (en mètres)", min = 200, max = 1000, value = 1, step=200))
         ),
         tableOutput(outputId = "stationsProches")
)