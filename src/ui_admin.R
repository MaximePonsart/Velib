tabPanel("admin",
         #choix du modèle statistique
         selectInput("modele_stat", "Modèle statistique :",
                     choices = c("A","B","C")),
         #zone de trace pour debug
         h2("debug"),
         verbatimTextOutput("debug")
)