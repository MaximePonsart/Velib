wellPanel(
  
  #lieu de départ
  fluidRow(
    column(width=10,textInput("adresseDepart","Adresse de départ : ")),
    column(width=2,actionButton("goCtrlDep","",icon=icon(name="search",lib="font-awesome"))),
    tags$style(type='text/css', "#goCtrlDep { width:100%; margin-top: 25px;}")
  ),
  
  #adresse d'arrivée
  fluidRow(
    column(width=10,textInput("adresseArrivee","Adresse d'arrivée : ")),
    column(width=2,actionButton("goCtrlArr","",icon=icon(name="search",lib="font-awesome"))),
    tags$style(type='text/css', "#goCtrlArr { width:100%; margin-top: 25px;}")
  ),
  
  #station de départ
  selectInput("stationDepart", "Station de départ : ",
              choices = setNames(stations_actives_nom$number, stations_actives_nom$name), selected=1),
  
  #station d'arrivée
  selectInput("stationArrivee", "Station d'arrivée : ",
              choices = setNames(stations_actives_nom$number, stations_actives_nom$name), selected=1),
  
  #parcours touristique
  selectInput("monument", "Visite touristique :",
              choices = c("(non)", monuments)),
  
  #nombre de vélos a réserver
  #sliderInput("equipe", "Combien êtes vous?", min = 1, max = 5, value = 1, step=1),
  
  #délai de départ
  selectInput("horizon", "Je pars...",
              choices = c("tout de suite!","dans 20 min", "dans 40 min", "dans 1 h"), selected = "tout de suite!"),
  
  p(),
  
  #soumission formulaire
  actionButton("go", "Go !", class="btn-primary")

)
