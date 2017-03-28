
tabPanel("stats", icon=icon("bar-chart"),
         fluidRow(
           column(width = 12,
                  plotOutput(outputId = "main_plot", height = "300px")
           )
         )
)
