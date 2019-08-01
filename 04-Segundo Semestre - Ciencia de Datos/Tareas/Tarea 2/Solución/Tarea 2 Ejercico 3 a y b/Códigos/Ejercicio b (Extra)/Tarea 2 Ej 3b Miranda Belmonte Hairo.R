###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Preguta 3 Ejercicio b
# Ejercicio Extra
# Versión 1
#
###############################

######################
## Librerias
######################
library("tidyverse")
library("magrittr")
library("shiny")

setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 2-20190215/Ejercicio 3")
getwd()

########################################
## Nota, el clasificador es del 0 al 9
########################################


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
######################
## Guardando PCA
######################
datos <- prcomp(Xtrain) # componentes principales
saveRDS(datos, file = "my_data.rds")

######################
## Clasificador
######################

# Matriz clasificadora
ceroMatrix <- matrix(0L, dim(Xtrain)[1],10)
for (i in 1:dim(Xtrain)[1]){
  ceroMatrix[i,Ytrain[i,1]+1] <-1 
} # end for

# rexportar pca
pc <- readRDS("my_data.rds")

# Seleccionar número de componentes
j <- 100
# scores
entrenarScores <- pc$x
# regresión
entrenamientoReg <-lm(ceroMatrix ~ entrenarScores[,1:j]-1, data=as.data.frame(entrenarScores)) # regresión sin constanate
# coeficientes
entrenamientoCof <- entrenamientoReg$coefficients
# loadings
load <- pc$rotation[,1:j]                    

## incluye la ruta donde esta la carpeta con archivos server y ui
# corra aplicación
runApp(appDir="C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 2-20190215/Ejercicio 3")
