tabPanel("détail", icon=icon("info-circle"),
         
         h2("Stations sélectionnées"),
         tableOutput(outputId = "stationsSelect"),
         
         # h2("Distance géo entre stations sélectionnées"),
         # textOutput(outputId = "distanceGeoStationsSelect"),
         
         h2("Stations proches des stations sélectionnées"),
         fluidRow(
           column(width=12,
                  radioButtons("choixStationDepartArrivee", "Station :",
                               c("départ" = "from", "arrivée" = "to"),
                               inline = T)
           )
         ),
         tableOutput(outputId = "stationsProches")
)