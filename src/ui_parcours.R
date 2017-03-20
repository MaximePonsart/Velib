tabPanel("trajet", icon=icon("bicycle"),
         
         h2("Temps"),
         verbatimTextOutput(outputId = "parcoursDateHeure"),
         
         h2("Départ"),
         verbatimTextOutput(outputId = "parcoursDepart"),
         
         h2("Arrivée"),
         verbatimTextOutput(outputId = "parcoursArrivee"),
         
         h2("Détail"),
         verbatimTextOutput(outputId = "parcoursDetail")
         
)