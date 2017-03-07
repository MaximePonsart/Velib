fluidPage(
  
  
  titlePanel("adopte un velib"),
  
  
  fluidRow(
    
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
    
    column(width = 8, 
           

           leafletOutput("carteGeo", height="1000px")
           
    )
  )
  
)
