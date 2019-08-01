##############################
### minimal example - ui.R ###
##############################
library("shiny") # load shiny at beginning at both scripts
library("pixels")

shinyUI(fluidPage(
    titlePanel("Pixels"),
    
    fluidRow(        
        column(5,
               titlePanel("Hairo Ulises Miranda Belmonte"),
               shiny_pixels_output(outputId="pixels"),
               actionButton("captureDigit", "Capturar")
               ),
  
        column(5,
               tableOutput(outputId="table1")
               
               ##plotOutput(outputId="plot1")
               )
    )  
))

