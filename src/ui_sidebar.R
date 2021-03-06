verticalLayout(

wellPanel(
  
  #lieu de d�part
  fluidRow(
    column(width=10,textInput("adresseDepart","Adresse de d�part")),
    column(width=2,actionButton("goCtrlDep","",icon=icon(name="search"))),
    tags$style(type='text/css', "#goCtrlDep { width:100%; margin-top: 25px;}")
  ),
  
  #station de d�part
  selectInput("stationDepart", "Station de d�part",
              choices = setNames(stations_actives_nom$number, stations_actives_nom$name), selected=1),
  
  #d�lai de d�part
  selectInput("horizon", "Je pars...",
              choices = setNames(as.character(seq(from=0, to=60, by=20)),c("tout de suite!","dans 20 min", "dans 40 min", "dans 1 h")))
 
),

wellPanel(
  
  #adresse d'arriv�e
  fluidRow(
    column(width=10,textInput("adresseArrivee","Adresse d'arriv�e")),
    column(width=2,actionButton("goCtrlArr","",icon=icon(name="search"))),
    tags$style(type='text/css', "#goCtrlArr { width:100%; margin-top: 25px;}")
  ),
  
  #station d'arriv�e
  selectInput("stationArrivee", "Station d'arriv�e",
              choices = setNames(stations_actives_nom$number, stations_actives_nom$name), selected=1),
  
  #parcours touristique
  selectInput("monument", "Visite touristique",
              choices = c("(non)", monuments))
  
),

p(),

#soumission formulaire
fluidRow(
  column(width=10, 
  actionButton("go", "", icon=icon(name="bicycle"), class="btn-primary", style="float:right")),
  column(width=2,p())
)


)

