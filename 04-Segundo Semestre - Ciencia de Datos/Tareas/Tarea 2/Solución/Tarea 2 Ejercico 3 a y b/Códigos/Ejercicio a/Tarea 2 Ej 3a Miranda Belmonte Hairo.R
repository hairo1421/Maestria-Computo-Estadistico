###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Preguta 3 Ejercicio a
# Versión 1
#
###############################

######################
## Librerias
######################
library("tidyverse")
library("magrittr")
library("gridExtra")
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 2-20190215/Ejercicio 3")
getwd()

######################
## Importando datos
######################
Xtrain <- read.table("mnistXtrain.dat", header = T, sep = "")
Xtrain %>% dim
Xtest <- read.table("mnistXtest.dat", header = T, sep = "")
Xtest %>% dim
Ytrain <- read.table("mnistYtrain.dat", header = T, sep = "")
Ytrain %>% dim
Ytest <- read.table("mnistYtest.dat", header = T, sep = "")
Ytest %>% dim

#################################
## Scores datos de entrenamiento
#################################
entrenar <- princomp(Xtrain)
entrenarScores <- entrenar$scores


#################################
## Scores datos de prueba
#################################
prueba <- princomp(Xtest)
pruebaScores <- prueba$scores


#################################
## Entrenando modelo
#################################
ceroMatrix <- matrix(0L, dim(Xtrain)[1],10)  # matriz de ceros
ceroMatrix %>% dim
ceroMatrix %>%  head

# clasifica vector respuestas a matriz de ceros y unos
for (i in 1:dim(Xtrain)[1]){
  ceroMatrix[i,Ytrain[i,1]+1] <-1 
} # end for
sum(ceroMatrix[,10])
View(ceroMatrix)
View(Xtrain)


##############################
## Función: regresión
##
## Realiza regresión PCR de
## predicción de modelo de
## entrenamiento y de prueba;
## calcula poporción
## de errores en predicicón
##
##
## Input: números enteros 
## Output: errores de
## predicción de modelo de
## entrenamiento y de prueba
##
##############################

regresion <- function(j){
entrenamientoReg <-lm(ceroMatrix ~ entrenarScores[,1:j]-1, data=as.data.frame(entrenarScores)) # regresión sin constanate
entrenamientoCof <- entrenamientoReg$coefficients
#entrenamientoCof %>%  dim
# realiza predicción
entrenamientoY <- entrenarScores[,1:j] %*% entrenamientoCof 
# toma valor mayor en la fila
entrenamientoYmayor <- apply(entrenamientoY, 1, which.max)
# convertir a matriz
entrenamientoYmayor <- entrenamientoYmayor %>%  as.matrix
#entrenamientoYmayor %>%  dim
# Proporción de erro, datos de entrnamiento
entrenamientoError <- 1-mean((entrenamientoYmayor-1)==Ytrain)

# datos de prueba 
loadin <- entrenar$loadings[,1:j]
xtest_loading <- as.matrix(Xtest)%*%loadin
pruebaY <- xtest_loading%*% entrenamientoCof # coeficientesde entrenamiento
#pruebaY %>% dim

# selecciona el valor mayor
pruebaYmayor <- apply(pruebaY, 1, which.max)
#head(pruebaY)
pruebaYmayor <- pruebaYmayor %>%  as.matrix
pruebaYmayor %>%  dim
a <-Ytest$x
# proporción de error
pruebaError <- 1-mean((pruebaYmayor-1)==a)

return(c(entrenamientoError,pruebaError,j))
}# end regresion

# vector de errores
errores <- numeric(0)
# enteros definidos
k <- 1:100
# errores con "p" componentes
errores <- sapply(k, function(k) regresion(k))
# gráfico 




ggplot() +  geom_line(aes(y=errores[1,], x=1:length(k),colour="Entrenamiento") )+ 
  labs(title="",
       y="error",
       x="componente (p)") +geom_line(aes(y=errores[2,], x=1:length(k),colour="Prueba")) +
  labs(title="Proporción de erro en predicción", 
       caption="Azul error entrenamiento, Rojo error de prueba") 


######################
## Fin
######################
