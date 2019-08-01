# # # # # # # # # # # # # # # # # # # # # # # #  # #
# 
#  Programación y Análisis de Algoritmos
#
# PROYECTO FINAL: Rcpp y C. APLICACIÓN
# A TÉCNICA DE REMUESTREO BOOTSTRAP
#
# HAIRO ULISES MIRANDA BELMONTE
# VERSION 1.0
# 20 DE NOVIEMBRE DEL 2018
#
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE I     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 


# LIBRERIAS NECESARIAS
rm(list = ls())

library("Rcpp") # Instalar y abrir libreria
library("fBasics") # Pruebas Estadísticas
library("ggplot2") # Gráficas
library("boot") # Paqueteria  Bootstrap
library("microbenchmark") # Medición de tiempos
# Paralelo OpenMp-Rcpp
#Sys.setenv("PKG_CXXFLAGS" = "-fopenmp") 
#Sys.setenv("PKG_LIBS" = "-fopenmp")


# COMPILADO

getwd() # Localizar dirección
setwd("C:/Users/h_air/Desktop/Proyecto final/Final") # Modificar dirección
sourceCpp('ProyectoFinal.cpp') # Compilar archivo .cpp 


# I.1 FUNCIONES COMPILADAS

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : bootstrapMeadia(x, B)
#        Realiza el método boostrap para estimar la
#        Desviación estandar del estadístico de la media
# Input: Vector con Datos
#        Tamaño de la simulación
# Outpu: Desviación estandar de la Media 
#        Estadísticos bootstrap
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : bootstrapMeadiana(x, B)
#        Realiza el método boostrap para estimar la
#        Desviación estandar del estadístico de la mediana
# Input: Vector con Datos
#        Tamaño de la simulación
# Outpu: Desviación estandar de la Mediana 
#        Estadísticos bootstrap
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : bootstrapVarianza(x, B)
#        Realiza el método boostrap para estimar la
#        Desviación estandar del estadístico de la varianza
# Input: Vector con Datos
#        Tamaño de la simulación
# Outpu: Desviación estandar de la Varianza 
#        Estadísticos bootstrap
#        Estadísticos bootstrap
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : bootstrapSD(x, B)
#        Realiza el método boostrap para estimar la
#        Desviación estandar del estadístico de la desviación
#        estándar
# Input: Vector con Datos
#        Tamaño de la simulación
# Outpu: Desviación estandar de la Desviación estándar 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# Funciones compiladas (complemento) para realizar bootstrap

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : media(xx1)
#        Realiza la media muestral
# Input: Vector con Datos
# Outpu: Media muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : mediana(xx1)
#        Realiza la mediana muestral
# Input: Vector con Datos
# Outpu: Mediana muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : varianza(xx1)
#        Realiza la varianza muestral con n-1 grados 
#        de libertad
# Input: Vector con Datos
# Outpu: Varianza muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : sd(xx1)
#        Realiza desviación estándar muestran
#        calculando varianza con función varianza(xx1)
# Input: Vector con Datos
# Outpu: Desviación estándar muestral 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : quicksort(NumericVector xx1, int l, int r)
#        Algoritmo de ordenamiento Quicksort
#     	 Ordenamiento de menos a mayor
# Input: Vector Númerico Double (Datos)
#        Entero que recibe el 0
#        Entero que recibe el 0
# Outpu: Tamaño de muestra menos uno.
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE II     # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# La siguiente sección se realiza con el fin de que el usuario 
# replique el documento de trabajo. Asimismo, pude hacer uso
# de otros datos para comprobar la funciones compiladas de C.

# II.1 FUNCIONES NECESARIAS PARA LA FUCNION BOOT DE R
# NOTA: CORRER FUNCIONES

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : meanfun(data, i)
#        Estima media muestral con el formato
#        necesario para la función boot.
# Input: Vector con Datos
# Outpu: Media muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
} # end meanfun

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : medianfun(data, i)
#        Estima mediana muestral con el formato
#        necesario para la función boot.
# Input: Vector con Datos
# Outpu: mediana muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

medianfun <- function(data, i){
  d <- data[i, ]
  return(median(d))   
}# end medianfun

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : varfun(data, i)
#        Estima varianza muestral con el formato
#        necesario para la función boot.
# Input: Vector con Datos
# Outpu: Varianza muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

varfun <- function(data, i){
  d <- data[i, ]
  return(var(d))   
}# end varfun

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Función : sdfun(data, i)
#        Estima desviación estandar  muestral con el formato
#        necesario para la función boot.
# Input: Vector con Datos
# Outpu: Varianza muestral
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

sdfun <- function(data, i){
  d <- data[i, ]
  return(stdev(d))   
}# end sdfun 

# II.2 PARTE EMPIRICA

iris <- datasets::iris # Datos pre-cargados en R Nota: (pueden ser cualquiera set)
Slength <- iris$Sepal.Length # Medición en centimetros  longitud del sépalo
dataSummary <- summary(Slength) # Descriptivos simples
sizeSaple <- length(Slength) # Tamaño de la muestra

# II.3 ANÁLISIS DE NORMALIDAD A LA MUESTRA

qplot(sample = Slength, data = iris)  +
  labs(title="Longitud Sépal QQ-Norm", y="Count")

ggplot(iris, aes(Slength) ) + geom_histogram(bins=5, col="white")   +
  labs(title="Histograma", x="Longitud Sépal", y="Count")

jq <- jarqueberaTest(Slength)
pvalJQ <- jq@test$p.value
st <- shapiro.test(Slength)
pvalST<- st$p.value
statisticST <- st$statistic

# II.4 COMPARACION FUNCIONES AUXILIARES

# Comparación de medias R vs C
meanR <- mean(Slength)
meanC <-media(Slength)
# Comparación de medianas R vs C
medianR <- median(Slength)
medianC <- mediana(Slength)
# Comparación de varianzas R vs C
varR <- var(Slength)
varC <- varianza(Slength)
# Comparación de desviación estándar R vs C
DesvEstR <- stdev(Slength)
DesvEstC <- sd(Slength)


# II.5 SECCION ESTIMACIÓN BOOTSRAP. ESTADÍSTICO : MEDIA
# COMPARACIÓN DE TIEMPOS. FUNCIÓN bootstrap vs boot

SlengthDataFrame <- as.data.frame(Slength) # Necesario para función boot de R

B <- 10^4 # Tamaño de simulación

# II.5.1 Tiempos

#Comparación en tiempos
moMedia <- microbenchmark(
  BootMediaC = bootstrapMedia(Slength,  B),
  BootMediaR =  boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=meanfun, R=B),
  times = 100
)
# Gráfico de tiempos
autoplot(moMedia)

# II.5.2 Estimación

# Bootstrap R
boMedia <- boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=meanfun, R=B)
boMediaIC<-boot.ci(boMedia, .95,  type="all")


# Bootstrap C
z <- c(qnorm(1-.1/2), qnorm(1-.05/2), qnorm(1-.01/2)) # nivel de confianza
bSeMediaC <-  bootstrapMedia(Slength, B) #Bootstrap

normal.CI.Media <- sapply(z, function(z){
  ICupper <<- meanC + z*(bSeMediaC$se) # IC superior
  IClower <<- meanC - z*(bSeMediaC$se) # IC inferior
  IC <- c(IClower, ICupper) # Intervalos de Confianza Normal
  return(IC)
} )
# Intervalos de Confianza Percentil
percentil.CI.Media <- c(quantile(bSeMediaC$tboot, .025), quantile(bSeMediaC$tboot, .975))
# Intervalos de Confianza Pivotal
pivotalCI.Media <- c( 2*media(Slength) - quantile(bSeMediaC$tboot, 1-.05/2), 2*media(Slength) - quantile(bSeMediaC$tboot, .05/2))

# II.6 SECCION ESTIMACIÓN BOOTSRAP. ESTADÍSTICO : MEDIANA
# COMPARACIÓN DE TIEMPOS. FUNCIÓN bootstrap vs boot

B <- 10^4 # Tamaño de simulación

# II.6.1 Tiempos

#Comparación en tiempos
moMediana<-microbenchmark(
  BootMedianaC = bootstrapMediana(Slength, B),
  BootMedianaR = boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=medianfun, R=B),
  times = 100
)
# Gráfico de tiempos
autoplot(moMediana, ylab ="a")

# II.6.2 Estimación

# Bootstrap R
boMediana <-boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=medianfun, R=B)
boMedianaIC<-boot.ci(boMediana, .95, type="all")


# Bootstrap C
z <- c(qnorm(1-.1/2), qnorm(1-.05/2), qnorm(1-.01/2))
bSeMedianaC <-  bootstrapMediana(Slength, B)

normal.CI.Mediana <- sapply(z, function(z){
  ICupper <<- medianC + z*(bSeMedianaC$se) # IC superior
  IClower <<- medianC - z*(bSeMedianaC$se) # IC inferior
  IC <- c(IClower, ICupper) # Intervalos de Normal
  return(IC)
} )

# Intervalos de Confianza Percentil
percentil.CI.Mediana <- c(quantile(bSeMedianaC$tboot, .025), quantile(bSeMedianaC$tboot, .975))
# Intervalos de Confianza Pivotal
pivotalCI.Mediana <- c( 2*median(Slength) - quantile(bSeMedianaC$tboot, .975), 2*median(Slength) - quantile(bSeMedianaC$tboot, .025))

# II.7 SECCION ESTIMACIÓN BOOTSRAP. ESTADÍSTICO : VARIANZA
# COMPARACIÓN DE TIEMPOS. FUNCIÓN bootstrap vs boot



B <- 10^4 # Tamaño de simulación

# II.7.1 Tiempos

#Comparación en tiempos
moVar <- microbenchmark(
  BootVarC =  bootstrapVarianza(Slength, B),
  BootVarR =  boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=varfun, R=B),
  times = 100
)

# Gráfico de tiempos
autoplot(moVar)

# II.7.2 Estimación

# Bootstrap R
boVarianza <-boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=varfun, R=B)
boVarianzaIC <-boot.ci(boVarianza, .95, type="all")


# Bootstrap C
z <- c(qnorm(1-.1/2), qnorm(1-.05/2), qnorm(1-.01/2))
bSeVarianzaC <-  bootstrapVarianza(Slength, B)

normal.CI.Varianza <- sapply(z, function(z){
  ICupper <<- varC + z*(bSeVarianzaC$se) # IC superior
  IClower <<- varC - z*(bSeVarianzaC$se) # IC inferior
  IC <- c(IClower, ICupper) # Intervalos de Confianza Normal
  return(IC)
} )

# Intervalos de Confianza Percentil
percentil.CI.Varianza<- c(quantile(bSeVarianzaC$tboot, .025), quantile(bSeVarianzaC$tboot, .975))
# Intervalos de Confianza Pivotal
pivotalCI.Varianza <- c( 2*var(Slength) - quantile(bSeVarianzaC$tboot, .975), 2*var(Slength) - quantile(bSeVarianzaC$tboot, .025))


# II.8 SECCION ESTIMACIÓN BOOTSRAP. ESTADÍSTICO : VARIANZA
# COMPARACIÓN DE TIEMPOS. FUNCIÓN bootstrap vs boot

B <- 10^4 # Tamaño de simulación

# II.8.1 Tiempos

#Comparación en tiempos
moSD <- microbenchmark(
  BootSDC = bootstrapSD(Slength, B),
  BootSDR =   boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=sdfun, R=B),
  times = 100
)
# Gráfico de tiempos
autoplot(moSD)

# II.8.2 Estimación

# Bootstrap R
boSD <-boot(SlengthDataFrame[, "Slength", drop = FALSE], statistic=sdfun, R=B)
boSDIC <-boot.ci(boSD, .95, type="all")

# Bootstrap C
z <- c(qnorm(1-.1/2), qnorm(1-.05/2), qnorm(1-.01/2))
bSeSDC <-  bootstrapSD(Slength, B)

normal.CI.SD <- sapply(z, function(z){
  ICupper <<- DesvEstC + z*(bSeSDC$se) # IC superior
  IClower <<- DesvEstC - z*(bSeSDC$se) # IC inferior
  IC <- c(IClower, ICupper) # Intervalos de Confianza
  return(IC)
} )

# Intervalos de Confianza Percentil
percentil.CI.SD <- c(quantile(bSeSDC$tboot, .025), quantile(bSeSDC$tboot, .975))
# Intervalos de Confianza Pivotal
pivotalCI.SD <- c( 2*sqrt(var(Slength)) - quantile(bSeSDC$tboot, .975), 2*sqrt(var(Slength)) - quantile(bSeSDC$tboot, .025))


# II.9 SECCION EXTRA. TIEMPOS PARA FUNCIONES AUXILIARES

# Algoritmos de ordenamiento R vs C
microbenchmark(
  quicksort(Slength, 0, 150-1),
  order(Slength)
)
# Medias R vs C
microbenchmark(
  media(Slength),
  mean(Slength)
)
# Medianas R vs C
microbenchmark(
  mediana(Slength),
  median(Slength)
)
# Varianzas R vs C
microbenchmark(
  varianza(Slength),
  var(Slength)
)
# Desviaciones estándar R vs C
microbenchmark(
  sd(Slength),
  stdev(Slength)
)

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #     FIN DEL DOCUMENTO     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 