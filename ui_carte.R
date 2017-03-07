fluidPage(
  
  # Application title
  titlePanel("adopte un velib"),
  
  # ouvrir une ligne horizontale
  fluidRow(
    # premiere colonne pour les entrees utilisateurs
    column(width = 2, 
           wellPanel(
             numericInput("bins", "Nombre de stations proches :", min = 1, max = 50, value = 30),
             #detail d'affichage
             radioButtons("couleur", "Detail :", c("d1" = "red", "d2" = "blue", "d3" = "green"),
                          inline = T),
             #colonne
             selectInput("var", "Ouverture :", choices = c("OPEN","CLOSED"), selected = "OPEN"),
             
             actionButton("go", "Go !")
           )
    ),
    # deuxieme colonne avec les sortiesw
    column(width = 8, 
           
           # DEUX ONGLETS, UN GRAPHIQUE UN TABLEAU
           # AVEC NAVBARPAGE OU TABSETPANEL
           
           leafletOutput("carteGeo", height="1000px")
           #,
           # centrer un/plusieurs element
           #div(
             #textOutput("textbins"), 
             #dataTableOutput("table"),
             #verbatimTextOutput("resume"),
             #align = "center")
           
    )
  )
  
)
