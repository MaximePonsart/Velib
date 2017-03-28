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
  
  fluidRow(
    column(width=8, h2("Parcours retenu")),
    column(width=4, h2("Matrice"))
  ),

  fluidRow(
    column(width=6, verbatimTextOutput(outputId = "parcoursDetail")),
    column(width=6, uiOutput('matrix'))
  ),
  
  fluidRow(
    column(width=12, h2("Parcours"))
  ),
  
  fluidRow(
    column(width=12, dataTableOutput(outputId = "tabParcours"))
  )
         
)