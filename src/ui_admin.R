tabPanel("admin", icon=icon("lock"),
         
  #Choix du jour courant
  h2("Retour vers le futur"),
  fluidRow(
    column(width=4, dateInput("dateSimulee", "Date simulée", value=dateSimulee, language="fr")),
    column(width=4, selectInput("hSimulee", "Heure", choices=sprintf("%02d",c(5:23,0:4)), selected=strsplit(heureSimulee,":")[[1]][1])),
    column(width=4, selectInput("mSimulee", "Minute", choices=sprintf("%02d",seq(from=0, to=59, by=5)), selected=strsplit(heureSimulee,":")[[1]][2]))
  ),
  
  #choix du modèle statistique
  h2("Calcul de prévisions"),
  selectInput("modele_stat", "Modèle statistique",
             choices = setNames(c("none","randomforest"),c("(aucun)","Random forest")),
             selected="randomforest"),
  
  h2("Nouveau modèle Random forest"),
  fluidRow(
    column(width=4,
           verticalLayout(
             dateInput("periodeRF", "Période mensuelle",language="fr", format="mm/yyyy", min="2015-02-01", max=Sys.time()),
             fluidRow(
               column(width=8,HTML("données historiques à charger")),
               column(width=4,actionButton('browse', 'Browse'))
             )
           )),
    column(width=8,
           verticalLayout(
             #verbatimTextOutput("chosenPath", placeholder=T),
             verbatimTextOutput("log", placeholder=T)
             )        
           )
     
    )
    
  # h2("Activation API Google drive_time"),
  # checkboxInput("activeAPIGoogleDT","activation", value=F)
  
  #zone de trace pour debug
  # h2("debug"),
  # verbatimTextOutput("debug")
         
)