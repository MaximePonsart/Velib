wellPanel(
  
  #lieu de départ
  textInput("adresseDepart","Adresse de départ : "),
  
  #adresse d'arrivée
  textInput("adresseArrivee","Adresse d'arrivée : "),
  
  #station de départ
  selectInput("stationDepart", "Station de départ : ", choices = c("(aucune)",stations_nom), selected=1),
  
  #station d'arrivée
  selectInput("stationArrivee", "Station d'arrivée : ", choices = c("(aucune)",stations_nom), selected=1),
  
  #parcours touristique
  selectInput("monument", "Visite touristique :",
              choices = c("(non)", monuments)),
  
  #nombre de vélos a réserver
  sliderInput("equipe", "Combien êtes vous?", min = 1, max = 5, value = 1, step=1),
  
  #délai de départ
  selectInput("horizon", "Je pars...",
              choices = c("tout de suite!","dans 20 min", "dans 40 min", "dans 1 h"), selected = "tout de suite!"),
  actionButton("go", "Go !")
)
