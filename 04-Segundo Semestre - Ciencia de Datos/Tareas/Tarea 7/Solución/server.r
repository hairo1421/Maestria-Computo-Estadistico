###############################################################
## Ejemplo que captura un numero y lo proyecta en los primeros
## 2 componentes principales previamente calculados en un
## conjunto de datos de entrenamiento y guardados en pc.rds
###############################################################
library("shiny") 
library("pixels")
library("OpenImageR")
##library(ripa)

shinyServer(function(input, output) {
  output$pixels <- shiny_render_pixels(
    show_pixels(grid=c(28,28),                    
                brush = matrix(c(0, 0.5, 0.8, 0.5, 0,
                                                0.5, 1, 1, 1, 0.5,
                                                  0.8, 1, 1, 1, 0.8,
                                                  0.5, 1, 1, 1, 0.5,
                                                  0, 0.5, 0.8, 0.5, 0), 5, 5))  
                  #matrix(c(1,1,1,
                            #     1,1,1,
                             #    1,1,1),3,3))
    ## puedes probar on otro brush, por ejemplo este:
    ##      
    ##brush=matrix(c(1,1,1,1),2,2))
  )
  output$prompt <- renderText("Dibuja un numero")
  
  observeEvent(input$captureDigit, {
    dig <<- NormalizeObject(as.numeric(input$pixels))
    #dig <<- NormalizeObject(as.numeric(Xtest[1,]))
    ## realiza ciertas rotaciones al digito escaneado,
    ## porque note que al graficarlo, esta en otra
    ## orientacion
    data.digit <- matrix(dig,ncol=28,nrow=28,byrow=T)
    temp.dig <- flipImage(rotateFixed(data.digit,180))
    tr.dig <- as.numeric(temp.dig)
    ##plot.img(tr.dig)
    
    ## limpiar el grid
    output$pixels <- shiny_render_pixels(
      show_pixels(grid=c(28,28),
                  brush = matrix(c(0, 0.5, 0.8, 0.5, 0,
                                   0.5, 1, 1, 1, 0.5,
                                   0.8, 1, 1, 1, 0.8,
                                   0.5, 1, 1, 1, 0.5,
                                   0, 0.5, 0.8, 0.5, 0), 5, 5))  
    )
    
    ## usare solo algunos digitos para este ejemplo...

    
    ## reescala el digito escrito
    tt <- scale.default(t(tr.dig),pc$center,pc$scale)
    ## obtiene scores (proyecciones en los componentes prncipales)
    proj <- tt%*%load
    #
   # pruebaY <- proj%*% entrenamientoCof # coeficientesde entrenamiento
    pruebaYmayor <- predict(clasificador,proj,decision.values=FALSE,probability=FALSE)
    
    #pruebaYmayor <- apply(pruebaY, 1, which.max)
    
    ## graficar
output$table1 <- renderTable(data.frame(Prediccion = isolate(pruebaYmayor)),bordered = T,
                              striped = T, hover = T,
                             spacing =  "l", 
                             rownames = F, caption = paste0("Con ",j," componentes"))
  }) 
})
