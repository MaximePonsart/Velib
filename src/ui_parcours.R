tabPanel("trajet", icon=icon("bicycle"),
         
  fluidRow(
   column(width=6, h2("Temps")),
   column(width=6, h2("Météo"))
  ),
  
  fluidRow(
   column(width=6, verbatimTextOutput(outputId = "parcoursDateHeure")),
   column(width=6, verbatimTextOutput(outputId = "parcoursMeteo"))
  ),

         
  fluidRow(
    column(width=6, h2("Départ")),
    column(width=6, h2("Arrivée"))
  ),
  
  fluidRow(
    column(width=6, verbatimTextOutput(outputId = "parcoursDepart")),
    column(width=6, verbatimTextOutput(outputId = "parcoursArrivee"))
  ),

  h2("Détail"),
  verbatimTextOutput(outputId = "parcoursDetail")
         
)