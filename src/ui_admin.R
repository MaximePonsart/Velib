tabPanel("admin", icon=icon("lock"),
         
  #Choix du jour courant
  h2("Retour vers le futur"),
  fluidRow(
    column(width=4, dateInput("dateSimulee", "Date simulée", value=dateSimulee, language="fr")),
    column(width=4, selectInput("hSimulee", "Heure", choices=sprintf("%02d",c(5:23,0:4)), selected=strsplit(heureSimulee,":")[[1]][1])),
    column(width=4, selectInput("mSimulee", "Minute", choices=sprintf("%02d",seq(from=0, to=59, by=5)), selected=strsplit(heureSimulee,":")[[1]][2]))
  ),
  
  #choix du modèle statistique
  h2("Prévision stat"),
  selectInput("modele_stat", "Modèle statistique :",
             choices = setNames(c("random","happy","serious"),c("aléatoire","happy","serious")),
             selected="happy")
  
  # h2("Activation API Google drive_time"),
  # checkboxInput("activeAPIGoogleDT","activation", value=F)
  
  #zone de trace pour debug
  # h2("debug"),
  # verbatimTextOutput("debug")
         
)