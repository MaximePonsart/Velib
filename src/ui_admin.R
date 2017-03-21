tabPanel("admin", icon=icon("lock"),
         #choix du modèle statistique
         h2("stat"),
         selectInput("modele_stat", "Modèle statistique :",
                     choices = setNames(c("aléatoire","happy","serious"),c("alea","happy","serious"))),
         #zone de trace pour debug
         h2("debug"),
         verbatimTextOutput("debug")
)