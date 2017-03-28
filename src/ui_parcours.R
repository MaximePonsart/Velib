tabPanel("trajet", icon=icon("bicycle"),
         
  fluidRow(
   column(width=6, h3("Temps")),
   column(width=6, h3("Météo"))
  ),
  
  fluidRow(
   column(width=6, verbatimTextOutput(outputId = "parcoursDateHeure")),
   column(width=6, verbatimTextOutput(outputId = "parcoursMeteo"))
  ),

  fluidRow(
    column(width=6, h3("Départ")),
    column(width=6, h3("Arrivée"))
  ),
  
  fluidRow(
    column(width=6, verbatimTextOutput(outputId = "parcoursDepart")),
    column(width=6, verbatimTextOutput(outputId = "parcoursArrivee"))
  ),
  
  fluidRow(
    column(width=6, 
           verticalLayout(
             h3("Trajet"),
             verbatimTextOutput(outputId = "parcoursDetail")
           )
    ),
    column(width=6,
#           conditionalPanel("input.go>0",uiOutput('matrix'))
            uiOutput('matrix')
    ),
    
    fluidRow(
      column(width=3, h2("Parcours")),
      column(width=9, selectInput(inputId="choixParcours", label="", choices=c("(aucun)")))
    ),
    
    fluidRow(
      column(width=12, 
             conditionalPanel("input.go>0",DT::dataTableOutput(outputId = "tabParcours"),
                              tags$style(type="text/css", "#tabParcours td:first-child {font-weight:bold;}"))
             )
    )
    
  )
  
)