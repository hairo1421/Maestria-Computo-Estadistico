library("tidyverse")
library("ggplot2")
library("knitr")
library("kableExtra")
library("stats")
library("corrplot")
library("DataExplorer")
library("ggpubr")
library("gridExtra")
library("plotly")
library("ggfortify")
library("magrittr")
######## HAIRO ULISES MIRANDA BELMONTE EXAMEN MULTIVARIADO

  
################ Ejercicio 3

ones <- function(X){
  unos <- X[,1] %>% length  %>% 
    diag %>% diag %>% as.matrix
  return(unos)
}

  # variables respuestas  
y1 <- matrix(c(5,3,4,2,1))
y2 <- matrix(c(-3,-1,-1,2,3))
y <- cbind(y1,y2)
  # covariable e intercepto
z1 <- matrix(c(-2,-1,0,1,2))
unos <- ones(z1)
z <- cbind(unos,z1)
###############
# a) determine los estimadores de mínimos cuadrados de los parámetros
# en el modelo de regresión lineal bivariada
################
# Parametros modelos de regresión lineal bivariada

b_multiple <- function(Z,y){
  return(solve(t(Z)%*%Z)%*%t(Z)%*%y)
}

a <-lm(y~z1)
# multiple
beta <-solve(t(z)%*%z)%*%t(z)%*%y
beta<-round(beta,1)
# de uno por uno
solve(t(z)%*%z)%*%t(z)%*%y[,1]
solve(t(z)%*%z)%*%t(z)%*%y[,2]
################
# b Calcule las matrices de los valores Yestimada y de los residuales Epsilon estimada con
# y verifica el calculo
################
yest <- z%*%beta

esiduales <- y - yest
RSS <- t(residuales)%*%residuales
# dos formas de verlo que debe cumplirse la igualdad
YY <- t(yest)%*%yest + RSS
YY <- t(y)%*%y
################
# c usando los resultados anteriores calcula intervalo de confianxa del 95%
# para la respuesta media
################
z0 <- .5
z0 <- as.matrix(c(1,.5))

# INTERVALOS DE CONFIANZA PARA LA FUNCIÓN DE REGRECIÓN ASOCIADAS CON beta'z0

# Intervalos de confianz asimultaneos para beta11

i <- 1
m <- dim(y)[2] 
n <- length(z[,1])
r <- dim(z)[2]
# sigma
Sigma <- RSS/length(z[,1])
# quiro solo para la respuesta uno 
a <- t(z0)%*%beta[,1]
b <-   sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))
c <- sqrt((t(z0)%*%solve(t(z)%*%z)%*%z0))*sqrt((n/(n-r))*Sigma[i,i])

# respuesta media 
Y01 <- 3-.9*(.5)
# INTERVALOS DE CONFIANZA PARA RESPUESTA MEDIA
superior <- a+b*c # 15.61034
inferior <- a-b*c # -10.51034
################
#d)################
# Intervalos de confianza para la predicción

################

i <- 1
upper <- t(z0)%*%beta[,i]+sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt(1+t(z0)%*%solve(t(z)%*%z)%*%z0)*sqrt((n/(n-r))*Sigma[i,i])
lower <- t(z0)%*%beta[,i]-sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt(1+t(z0)%*%solve(t(z)%*%z)%*%z0)*sqrt((n/(n-r))*Sigma[i,i])
# upper:33.02413
# lower: -27.92413


################ EJERCICIO 4
# 4 dadas las siguientes muestra de observaciones

x <- matrix(c(2,8,6,8,12,9,9,10),4,2)
mu <- matrix(c(7,11))


#T2 hotelling
ones <- function(X){
  unos <- X[,1] %>% length  %>% 
    diag %>% diag %>% as.matrix
  return(unos)
}
media <- function(X) return((1/length(X[,1]))*(t(X)%*%ones(X)))
t2hotelling <- function(X,mu) length(X[,1])*(t(media(X)-mu)%*%solve(cov(X))%*%(media(X)-mu))
# a evalua  T2 con el vector de media que se te da
T2 <-t2hotelling(x,mu)
# b especifica la distribución de T2 (verificando la normalidad de los datos)

# vemos si probienen de una normal bivariada al 95% de significancia
# al 95%
x %>% as.data.frame %>% ggplot() + aes(x = x[,1],y = x[,2]) +
  geom_point() + labs(title="Ellipse",
                      x = "x1",
                      y = "x2",
                      caption = "Normalida bivariada",
                      subtitle ="95% de significancia") +
  stat_ellipse(level=0.95, color="blue",linetype = 2) 

# al 90%
x %>% as.data.frame %>% ggplot() + aes(x = x[,1],y = x[,2]) +
  geom_point() + labs(title="Ellipse",
                      x = "x1",
                      y = "x2",
                      caption = "Normalida bivariada",
                      subtitle ="90% de significancia") +
  stat_ellipse(level=0.90, color="blue",linetype = 2) 

# c. probar la Hipotesis nula 
Fcritico <- function(X,alpha) ((dim(X)[2]*(length(x[,1])-1))/(length(x[,1])-dim(X)[2]))*qf(alpha, dim(X)[2],length(x[,1]) -dim(X)[2], lower.tail = F) 

HoHotelling <- function(X,mu,alpha){
  
  T2 <-t2hotelling(X,mu)
  Fest <- Fcritico(X,alpha) 
  test <- T2 <=Fest # No se rechaza Ho si es menor

  return(list(T2=T2,Fcritico=Fest,CaeEnRegion=test))
}

Resultado <- HoHotelling(x,mu,.05)
Resultado
# no se tiene evidencia para rechazar la Ho de que mu=[7,11] pertenecen  a esa población




# d) evalue la lambda de wilks
ones <- function(X){
  unos <- X[,1] %>% length  %>% 
    diag %>% diag %>% as.matrix
  return(unos)
}
media <- function(X) return((1/length(X[,1]))*(t(X)%*%ones(X)))

matrizCentrado <- function(X) return(x-ones(x)%*%t(media(x)))

A <- t(matrizCentrado(x))%*%matrizCentrado(x)/length(x[,1])
B <- t(x-ones(x)%*%t(mu))%*%(x-ones(x)%*%t(mu))/length(x[,1])



# lambda 2/n Aproximación T de hotelling
# B matriz de covarianzas estimada sobre la nula
# A matriz de covarianza completa
# n tamaño
# alpha significancia
lamdadeWilksT2 <- function(X,A,B,n,alpha){
  numerador <- (n-1)*det(B)
  t2 <- (numerador/det(A))-(n-1) # estadístico
  lambdaW <- (1+(t2/(n-1)))^(-1) # estadistico
  
  critico <- Fcritico(X,alpha) # valor critico T2
  lambdaW_critico <- (1+(critico/(n-1)))^(-1) # valor crpitico razón verosimilitud
  test <- lambdaW >= lambdaW_critico # No rechazo
  return(list(T2Prueba=t2,LWPrueba=lambdaW, 
              T2CriticoF=critico, LWCritico=lambdaW_critico,NoRechaza=test))
}
n <- length(x[,1])

lamdadeWilksT2(x,A,B,n,.05)
# por lo tanto no rechazo


################
######## ejercicio 5
getwd()
datos <- read.table("bird_data.dat", header = F)
#x1 longitud de la cola en mm
# x2 longitud de la ala en mm
n <- 45
X <- data.frame(x1=datos[,1], x2=datos[,2])
X <- X  %>% as.matrix 
#a construya y muestra una región de confianza (elipse), del 95%
# para m1 y m2
mu1 <- 190
mu2 <- 275
mu <- matrix(c(mu1,mu2))

#a)
Fcritico <- function(X,alpha) ((dim(X)[2]*(length(X[,1])-1))/(length(X[,1])-dim(X)[2]))*qf(alpha, dim(X)[2],length(X[,1]) -dim(X)[2], lower.tail = F) 

RegionConfianza <- function(X,mu,alpha){
  
  T2 <-t2hotelling(X,mu)
  Fest <- Fcritico(X,alpha) 
  test <- T2 <=Fest 
  # equivale a decir que la media nula está dentro de la region
  return(list(T2=T2,Fcritico=Fest,CaeEnRegion=test))
}

Resultado <- RegionConfianza(X,mu,.05)
# estos son los resultados
# T2 de hotellin = 5.54313
# F critico = 6.578471
Resultado
# el vectore de medias mu1 y mu2 caen dentro de la región de confianza al 95%

# vemos el gráfico
X %>% as.data.frame %>% ggplot() + aes(x = X[,1],y = X[,2]) + geom_point() +
  geom_point(aes(mu1,mu2), col="red") + labs(title="Ellipse",
                      x = "x1",
                      y = "x2",
                      caption = "Puntos rojos son las medias HO",
                      subtitle ="95% de confianza") +
  stat_ellipse(level=0.95, color="blue",linetype = 2) 

# como se observa caen dentro de la región de confianza

# b) Construye los intervalos de confianza T2 simultaneos 


# T2 Hotelling al cuadrado simultaneo 
# se mantienen simultaneamente pero no son ajustados

T2_intervalo_simultaneo <- function(X,a,alpha){
  n <- length(X[,1]) 
  p <- dim(X)[2]
  S <- X %>% cov
  Xmean <- X %>% colMeans %>% as.matrix
  Fdistribucion <- qf(alpha,p, (n-p), lower.tail = F )
  critico <- (((n-1)*p)/(n*(n-p)))*Fdistribucion
  lower <- t(a)%*%Xmean - sqrt(critico*(t(a)%*%S%*%a))
  upper <- t(a)%*%Xmean + sqrt(critico*(t(a)%*%S%*%a))
  return(list(Upper=upper,Lower=lower))
}

# INTERVALOS DE CONFIANZA SIMULTANEOS
  # PARA MU1
a <-matrix(c(1,0)) 
T2_intervalo_simultaneo(X,a,.05)
# mu1 sobre Ho = 190
# IC Lower =189.4217
# IC Upper =197.8227
# intervalos si contienen a mu 1

# PARA MU1
a <-matrix(c(0,1)) 
T2_intervalo_simultaneo(X,a,.05)
# mu2 sobre Ho = 275
# IC Lower =274.2564
# IC Upper =285.2992
# intervalos si contienen a mu 2
Bonferroni <- function(X,a,alpha){
  n <- length(X[,1]) 
  p <- dim(X)[2]
  S <- X %>% cov
  Xmean <- X %>% colMeans %>% as.matrix
  t<- qt(alpha/(2*p),(n-1))
  lower <- t(a)%*%Xmean - t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  upper <- t(a)%*%Xmean + t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  return(list(Upper=upper,Lower=lower))
}

# INTERVALOS DE CONFIANZA SIMULTANEOS
# PARA MU1
a <-matrix(c(1,0)) 
Bonferroni(X,a,.05)
# mu1 sobre Ho = 190
# IC Lower =189.8216
# IC Upper =197.4229
# si contienen a mu1

# PARA MU1
a <-matrix(c(0,1)) 
Bonferroni(X,a,.05)
# mu2 sobre Ho = 275
# IC Lower = 274.7819
# IC Upper =284.1163
# si contienen a mu 2


# intervalos de confianza bonferroni más ajustados menos angostos


1/3
a <- matrix(c(0.3333333,0.3333333,0.3333333))
mu <- matrix(c(0.7,0.8,0.9))
t(a)%*%mu
sig2 <- matrix(c(2,-1,0,-1,2,0,0,0,1),3,3)
t(a)%*%sig2%*%a
1/3


((1/4)^2)+((1/2)^2)+((-1/4)^2)+((-1/4)^2)
fractions((-1/8)+(1/16))
