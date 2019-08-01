############################################
############################################
############################################
#####   Tarea 5. Ciencia de datos
#####   Ejercicio 2. ´Perceptron online
#####   Miranda Belmonte Hairo Ulise
#####   22 de Marzo del 2019
############################################
############################################
############################################


#################
## Librerias
#################
library("mlbench")
library("ggplot2")
library("magrittr")
library("factoextra")
# Para el perceptron, se toma el pseudocódigo realizado en:
# Cristianini N, Shawe-Taylor J. An Introduction to Support Vector Machines and Other Kernel-Based Learning Methods. Cambridge University Press; 2000.




# Generando datos que sean linealmente separables
obs <- mlbench.2dnormals(100,2,5)
clases <- obs$classes %>% as.numeric
p1 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1) +
  labs(title = "Datos de prueba",
       x="X1",y="X2")
# observaciones
datos <- obs$x
# etiquetas
Y <- obs$classes # factor
Y <- Y %>% as.numeric # núerica
Y[Y==2] <- -1 # binarias nueva etiqueta (-1,1)

################################################
#
# Función: Percptron
#
# input: x: covariables
#        y: etiquetas  
#        paso: tamaño de paso
#
# output: pesos, sesgo, intercepto, 
#         pendiente, # de errores en iteración
#
################################################
perceptron <- function(x, y, paso) {
  # valores iniciales
  n = ncol(x) #dimensiones
  R <- max(x)  # Norma maxima
  b0 <- 0 # sesgo
  k <- 0 # contador
  beta <- rep(0, n) # pesos
  error <- TRUE
 
  # algoritmo corre hasta no equivocarse
  while (error == TRUE){
    for (i in 1:nrow(x)){
      if (y[i] * ((t(beta)%*%x[i,]) + b0) <= 0){
        # Actualiza
        beta <- beta + as.matrix(paso*y[i]*x[i,])
        b0 <- b0 + paso*y[i]*R^2
        k <- k +1
        error = TRUE
      } else{
        # No Actualiza
        error = FALSE
      }
    }
  } 
  
  # componentes del perceptor
  pendiente = -beta[1]/beta[2]
  intercepto = -b0/beta[2]

  # Resultados 
  resultados = as.data.frame(t(rbind(b0, beta, intercepto, pendiente, k)))
  colnames(resultados) <- c('sesgo', 'beta1', 'beta2', 'intercepto', 'pendiente', 'errores')
  
  return(resultados)
} # end perceptron


resultado <- perceptron(x=datos, y=Y, paso=1)

p2 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1, pch = ifelse(Y > 0, 22, 16)) +
  labs(title = "Algoritmo Perceptron",
       x="X1",y="X2", subtitle="Datos de prueba", caption="Número de Obs. n=50") +
  geom_abline(intercept =  resultado$intercepto, slope = resultado$pendiente, col="blue")

x11()
gridExtra::grid.arrange(p1,p2, nrow=1)

# Se generan datos de prueba más cercanos
obs2 <- mlbench.2dnormals(200,2,2)
clases2 <- obs2$classes %>% as.numeric
p3 <- obs2$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases2+1) +
  labs(title = "Datos de prueba",
       x="X1",y="X2")
# observaciones
datos2 <- obs2$x
# etiquetas
Y2 <- obs2$classes # factor
Y2 <- Y2 %>% as.numeric # núerica
Y2[Y2==2] <- -1 # binarias nueva etiqueta (-1,1)

resultado2 <- perceptron(x=datos2, y=Y2, paso=1)

p4 <- obs2$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases2+1, pch = ifelse(Y2 > 0, 22, 16)) +
  labs(title = "Algoritmo Perceptron",
       x="X1",y="X2", subtitle="Datos de prueba", caption="Número de Obs. n=200") +
  geom_abline(intercept =  resultado2$intercepto, slope = resultado2$pendiente, col="blue")

x11()
gridExtra::grid.arrange(p3,p4,nrow=1)

####################################
####################################
######## FIN INCISO A ##############
####################################
####################################

####################################
####################################
########  INCISO B  ################
####################################
####################################


# Cargue y cambie la dirección donde contiene los archivos
getwd()
setwd("C:/Users/h_air/Downloads/Tarea 5-20190404")
# extrae los archivos
data.tr <- read.table("pima.tr", header=TRUE)
data.te <- read.table("pima.te", header=TRUE)
# se separa covariables de variable respuesta
X <- data.tr[1:7] %>% as.matrix
Y <- data.tr[8]
# re-etiquetamos variable respuesta
Y2 <- ifelse(Y=="Yes", 1, -1)
Y <- Y2
Y <- Y %>% as.numeric %>% as.matrix
################################################
#
# Función: Perceptron
#
# input: x: covariables
#        y: etiquetas  
#        paso: tamaño de paso
#
# output: pesos, sesgo, intercepto, 
#         pendiente, # de errores en iteración
#
################################################
perceptron <- function(x, y, paso) {
  # valores iniciales
  n = ncol(x) #dimensiones
  R <- max(x)  # Norma maxima
  b0 <- 0 # sesgo
  k <- 0 # contador
  beta <- rep(0, n) # pesos
  error <- TRUE
  
  # algoritmo corre hasta no equivocarse
  while (error == TRUE){
    for (i in 1:nrow(x)){
      if (y[i] * ((t(beta)%*%x[i,]) + b0) <= 0){
        # Actualiza
        beta <- beta + as.matrix(paso*y[i]*x[i,])
        b0 <- b0 + paso*y[i]*R^2
        k <- k +1
        error = TRUE
      } else{
        # No Actualiza
        error = FALSE
      }
    }
  } 
  
  # componentes del perceptor
  pendiente = -beta[1]/beta[2]
  intercepto = -b0/beta[2]
  
  # Resultados 
  resultados = as.data.frame(t(rbind(b0, beta, intercepto, pendiente, k)))
  colnames(resultados) <- c('sesgo', 'beta1', 'beta2','beta3', 'beta4','beta5', 'beta6','beta7',  'intercepto', 'pendiente', 'errores')
  
  return(resultados)
} # end perceptron

#########################
##
## Se entrena perceptron
##
#########################

# se implementa el algoritmo perceptron a los datos escalados
# muchas variable tienen unidades de medidas distintas
plaResult2 <- perceptron(x=scale(X), y=Y, paso=1)

# Se realiza pca para realizar PCR con el fin de reducir dimensiones
# y visualizar
datos <- princomp(scale(X)) # se escalan las observaciones
                            # tienen distintas unidades de medida

# en base al screeplot se toman las dos primeras componentes
g1 <- fviz_screeplot(datos, addlabels = TRUE, ylim = c(0, 50))
g2 <- fviz_pca_ind(datos,
             label = "none", 
             habillage = as.factor(Y),
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, 
             caption="1: Yes;  0: No"
)


x11()
gridExtra::grid.arrange(g1,g2,nrow=1)
# se toman las dos primeras componentes principales
obs <- datos$scores[,1:2]
# se visualizan los componentes separados por el hiperplano
# generado por el perceptrón
x11()
Respuesta <- Y %>% as.factor
obs %>% as.data.frame %>% ggplot() +
   scale_color_manual(values = c("-1" = 'green','1' = 'black')) +
  aes(x=Comp.1, Comp.2,col= Respuesta) + geom_point(pch = ifelse(Y > 0, 17, 16),size=4) +
  labs(title = "",
       x="Componente 1",y="Componente 2", subtitle="Perceptrón", caption="YES: 1: NO: -1") +
  geom_abline(intercept = plaResult2$intercepto, slope=plaResult2$pendiente, col="red")


#######################################################
##
## Perceptron  datos de prueba
##
#######################################################
# NOTA: El sesgo es cero, i.e., el intercepto, dado que se escalaron las
# observaciones

# Se realiza tabla de confución con datos de prueba
Yestimada <- as.matrix(scale(data.te[1:7]))%*%t(as.matrix(plaResult2[2:8]))
Y_test <- ifelse(data.te[8]=="Yes", 1, -1)
Y_test <- Y_test %>% as.numeric %>% as.matrix
table(Y_test, ifelse(Yestimada>0,1,-1))
158+19
####################################
####################################
######## FIN INCISO A ##############
####################################
####################################