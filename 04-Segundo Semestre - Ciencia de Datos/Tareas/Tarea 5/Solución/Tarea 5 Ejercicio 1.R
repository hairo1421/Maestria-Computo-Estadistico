############################################
############################################
############################################
#####   Tarea 5. Ciencia de datos
#####   Ejercicio 1. FDA
#####   Miranda Belmonte Hairo Ulise
#####   04 de Marzo del 2019
############################################
############################################
############################################

################
##
## Librerías
##
################
library("ggplot2")
library("magrittr")
library("mlbench")



###################
# Primer Ejemplo
###################

#####################
# Ejercico E
#####################


# Generar datos sinteticos en 2D
set.seed(1) # fija semilla
obs <- mlbench.2dnormals(100,2,6) # mezcla de gaussiana
clases <- obs$classes %>% as.numeric
p1 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1) +
  labs(title = "Datos de prueba",x="X1",y="X2")

#Generando etiquetas de respuesta conforme a la función que se nos da
Y <- obs$classes %>% as.vector %>% as.numeric
# re etiquetando
Y[Y==2] <- -1
# valores de tetha
theta1 <- length(Y)/length(Y[Y==1])
theta2 <- -length(Y)/length(Y[Y==-1])
Y[Y==-1] <- theta2
Y[Y==1] <-theta1
# Ahora utilizamos función lm para ajustar los parámetros
  # En base a las demostraciones se concluye que realizar FDA es aproximado
    # a utilizar una solución de mpinimos cuadrados.
resultado <- lm(Y~obs$x)

# observamos hiperplano separador en datos de prueba
p2 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1, pch = ifelse(Y > 0, 22, 16)) +
  labs(title = "Datos de prueba",
       x="X1",y="X2", subtitle="FDA", caption="Número de Obs. n=100") +
  geom_abline(intercept = resultado$coefficients[1], slope=resultado$coefficients[2], col="blue")

predecir <- predict(resultado,as.data.frame(obs$x))
# retiquetando
Y2 <- clases
Y2[Y2==2] <- 0
# tabla de confución
Tabla1 <- table(Y2, ifelse(predecir> 0, 1, 0))


#####################
# Ejericio F
#####################

# se busca detetar outliers; entonces se toma al peso como el inverso
# proporcional de la distancia de la observación a su centroide
# se utiliza la distancia euclidiana al cuadrado
meanK1 <- obs$x[Y>0] %>% mean
meanK2 <- obs$x[Y<0] %>% mean
n <- (obs$x %>% length)
i <- 1:100
D <- sapply(i, function(i) ifelse(obs$x[Y>0][i], sqrt(sum((as.matrix(meanK1-obs$x[i,]))^2)), 
                                   sqrt(sum((as.matrix(meanK2-obs$x[i,]))^2))))


# pesos
w <- 1/D
# inverso de la distancia, más peso los outliers
# menos cercano más peso
resultado2 <- lm(Y~obs$x, weights=w)
# observamos hiperplano separador en datos de prueba
plot1 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1, pch = ifelse(Y > 0, 22, 16)) +
  labs(title = "Datos de prueba",
       x="X1",y="X2", subtitle="FDA con pesos", caption="Número de Obs. n=100") +
  geom_abline(intercept = resultado2$coefficients[1], slope=resultado2$coefficients[2], col="blue")

predecir <- predict(resultado2,as.data.frame(obs$x))
# retiquetando
Y2 <- clases
Y2[Y2==2] <- 0
# tabla de confución
Tabla2 <- table(Y2, ifelse(predecir> 0, 1, 0))
X11()
gridExtra::grid.arrange(p1,p2,plot1,nrow=1)

###################
# Segundo Ejemplo
###################

#####################
# Ejercico E
#####################

# Generar datos sinteticos en 2D
set.seed(1) # fija semilla
obs <- mlbench.2dnormals(100,2,2) # mezcla de gaussiana
clases <- obs$classes %>% as.numeric
p1_2 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1) +
  labs(title = "Datos de prueba",x="X1",y="X2")

#Generando etiquetas de respuesta conforme a la función que se nos da
Y <- obs$classes %>% as.vector %>% as.numeric
# re etiquetando
Y[Y==2] <- -1
# valores de tetha
theta1 <- length(Y)/length(Y[Y==1])
theta2 <- -length(Y)/length(Y[Y==-1])
Y[Y==-1] <- theta2
Y[Y==1] <-theta1
# Ahora utilizamos función lm para ajustar los parámetros
# En base a las demostraciones se concluye que realizar FDA es aproximado
# a utilizar una solución de mpinimos cuadrados.
resultado <- lm(Y~obs$x)
x11()
# observamos hiperplano separador en datos de prueba
p2_2 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1, pch = ifelse(Y > 0, 22, 16)) +
  labs(title = "Datos de prueba",
       x="X1",y="X2", subtitle="FDA", caption="Número de Obs. n=100") +
  geom_abline(intercept = resultado$coefficients[1], slope=resultado$coefficients[2], col="blue")

predecir <- predict(resultado,as.data.frame(obs$x))
# retiquetando
Y2 <- clases
Y2[Y2==2] <- 0
# tabla de confución
table(Y2, ifelse(predecir> 0, 1, 0))


#####################
# Ejercico F
#####################

# calculando distancia generalizada para el vector de pesos
meanK1 <- obs$x[Y>0] %>% mean
meanK2 <- obs$x[Y<0] %>% mean
n <- (obs$x %>% length)
i <- 1:100
D <- sapply(i, function(i) ifelse(obs$x[Y>0][i], sqrt(sum((as.matrix(meanK1-obs$x[i,]))^2)), 
                                  sqrt(sum((as.matrix(meanK2-obs$x[i,]))^2))))


# pesos
w <- (1/D)
# inverso de la distancia, más peso los outliers
# menos cercano más peso
resultado2 <- lm(Y~obs$x, weights=w)
# observamos hiperplano separador en datos de prueba
plot1_2 <- obs$x %>% as.data.frame %>% ggplot() +
  aes(x=V1,V2) + geom_point(col=clases+1, pch = ifelse(Y > 0, 22, 16)) +
  labs(title = "Datos de prueba",
       x="X1",y="X2", subtitle="FDA con pesos", caption="Número de Obs. n=100") +
  geom_abline(intercept = resultado2$coefficients[1], slope=resultado2$coefficients[2], col="blue")

x11()
gridExtra::grid.arrange(p1_2,p2_2,plot1_2,nrow=1)

predecir <- predict(resultado2,as.data.frame(obs$x))

# retiquetando
Y2 <- clases
Y2[Y2==2] <- 0
# tabla de confución
table(Y2, ifelse(predecir> 0, 1, 0))

#######################################################
#######################################################
############ FIN DEL EJERCICIO 1 ######################
#######################################################
#######################################################
