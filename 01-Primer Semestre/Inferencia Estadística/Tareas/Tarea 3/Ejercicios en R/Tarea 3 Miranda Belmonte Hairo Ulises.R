#Pregunta 1
#Simule una muestra {x1, . . . , xn} de una v.a. Normal() de tama�o n = 10^5
#. Defina ym = sumatoria xi/m y grafique esta cantidad. �Que observa? �Como est�a esto relacionado
#con la LGN?
   

# variables auxiliares
n <- 10^5
mu <- pi
sd <- sqrt(2)

#Generando la muestra de la variable aleatoria 10^5 veces
set.seed(100) # fijando semilla
ym <- cumsum((rnorm(n, pi, sd)))/1:n


plot(c(1, n), c(mu-sd, mu+sd), type = "n",
     xlab = "Tama�o de muestra x", main = "Simulaci�n x~Normal(pi, sqrt(2))", 
     ylab = "" )
lines(ym, type="l")

#El siguiente plot solamente reduce el grill del anterior
plot(c(1, n), c(mu-sd, mu+sd), type = "n",
     xlab = "Tama�o de muestra x", main = "Simulaci�n x~Normal(pi, sqrt(2))", 
     ylab = "" , xlim = c(1,1000))
lines(ym, type="l", lwd=2)
abline(pi, 0, col ="red")

# Se observa que cuando n -el numero de simulaciones- es grande, 
# la media mustral se aproxima a la media poblacional (pi). 
# A su vez, lo anterior se puede relacionar con la convergencia en probabilidad,
# dado a que una secuencia de variables aleatorias se aproxima 
# a una variable aleatoria -(i.e, la media muestral es una variable aleatoria enerada
# por una combinaci�n lineal de una secuencia de variables aleatorias)- cuando 
# el tama�o de muestra tiende a infinito.



#Repita el procesoanterior 100 veces y grafique  y de cada iteraci�n
#sobre una misma gr�fica

# Define l�mites del gr�fico
plot(c(1, n), c(mu-sd, mu+sd), type = "n",
     xlab = "Tama�o de la muestra n", ylab = "", main = "Simulaci�n Normal")
#Se repite el proceso 100 veces, gr�ficando para cada iteraci�n
set.seed(100) # fijando semilla
replicate(100, lines(1:n, (cumsum((rnorm(n, mu, sd)))/1:n), 
                             type = "l", col="blue"))# en cada simulacion se realiza el plot
# Retirar # para ver la gr�fica con su media polacional
# y sus respectivos intervalos de confianza

#abline(pi, 0, col="red") # media poblacional
# intervalos de confianza
#lines(1:n, mu + 2*sqrt(sd/1:n), col="red" )
#lines(1:n, mu - 2*sqrt(sd/1:n), col="red")




#Repita los dos incisos anteriores para una distribuci�n cauchi �Qu� observa?
set.seed(100) # fijando semilla
Cauchy <- cumsum((rcauchy(n, pi, sd)))/1:n

ggplot(as.data.frame(Cauchy))+aes( as.data.frame(1:n), as.data.frame(Cauchy))+  geom_line()

plot(Cauchy, type="l", xlab = "Tama�o de la muestra x", ylab = "", 
     main = "Simulaci�n x~Cauchy(pi, sqrt(2))")
abline(pi, 0, col="red")

# Caso contrario a la media muestral generada con variables aleatorias normales,
# al tener un conjunto de variables aleatorias con distribuci�n cauchy,
# el estadistico -media muestral-, no coverge en �probabilidad (i.e, conjunto
# de variables aleatorias tienden a una variable aleatoria). Lo que sucede, al contrario
# de las normales, es que al incrementar el n�mero de muestras, la media 
# muestral no se aproxima a su media poblacional. Esto se puede explicar, dado
#  que la distribuci�n cauchy se encuentra ausente de momentos. Otro determinante, 
# es el no cumplimiento de la ley de los grandes n�meros ( debil ), dado a que no s�lo
# se es necesario un conjunto de datos iid, tambi�n se require que las variables aleatoria
# presenten media y varianza finitas, (que existan primer y segundo momento).


#Repita el procesoanterior 100 veces y grafique  y de cada iteraci�n
#sobre una misma gr�fica

#Simulaci�n del proceso anterior (100 veces).
set.seed(100) # fijando semilla
plot(c(1, n), c(-100, 100), type = "n",
     xlab = "Tama�o de la muestra n", ylab = "", main = "Simulaci�n Normal")

replicate(100, lines(1:n, (cumsum((rcauchy(n, mu, sd)))/1:n), 
                                  type = "l", col="blue",
                                  xlab = "Tama�o de la muestra n", ylab = "",
                                  main = "Simulaci�n x~Ccuchy(pi, sqrt(2))"))# en cada simulacion se realiza un plot
abline(pi, 0, col="red")


# Pregunta 3
#Sea a = 0.05 y p = 0.4. Mediante simulaciones, realice un estudio para ver que tan
#a menudo el intervalo de confianza contiene a p (la cobertura). Haga esto para n =
 # 10, 50, 100, 250, 500, 1000, 2500, 5000, 10000. Grafique la coberturacontra n


rm(list =ls()) #limpiando enviroment
N <- 10^4 #N�mero de simulaciones 
a <- 0.05 # alpha
p <- 0.4 # Bernoulli(p) and also the mean
set.seed(100) # fijando semilla
n <- c(10, 50, 100, 250, 500, 1000, 2500, 5000, 10000) # Tama�o de la muestra

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : IC
# Calcula una simulaci�n en la cual 
# contabiliza el n�mero de veces en el que
# el intervalo de confianza contiene al 
# al valor de la media teorica (o poblacional)
# El intervalo de confianza se realiza con la 
# desigualdad de Hoeffdinf
#
# Input: N�mero de la muestra
# Outpu: N�mero de veces que cae en el intervalo 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
IC <- function(n){
hits <- 0
epsilon <- sqrt((1/(2*n))*(log(2/a))) 
pest <- mean(rbinom(n, 1, p))
if(abs(pest-p) < epsilon  ){hits <- hits + 1}
return(hits)
}

# Matriz que almacena el n�mero de veces que p cae en el intervalo
Simulacion <- matrix(0, 9, 2) 

for(i in seq(1, 9, 1)){ # For para cada muestra n
Simulacion[i, ]<-table(replicate(N, IC(n[i])))
} # end for



colnames(Simulacion) <-  c("Fuera IC", "Dentro IC")
rownames(Simulacion) <-  c("n = 10", "n = 50", "n = 10" ,"n = 250", 
                           "n = 500", "n = 1000", "n = 2500", "n = 5000",
                           "n = 10000")
Simulacion #proporci�n de veces que cae dentro del intervalo de confianza

# Se gr�fica  la cobertura contra el numero de muestra "n"
Cn <- matrix(0, 9,2) # Se introduce valores en matriz

for(i in seq(1, 9, 1)){ #realiza la simulaci�n 
epsilon[i] <- sqrt((1/(2*n[i]))*(log(2/a))) 
set.seed(100) # fijando semilla
pest[i] <- mean(rbinom(n[i], 1, p))
Cn[i, ] <- c(pest[i] - epsilon[i], pest[i] + epsilon[i]); 
if(Cn[i, 1] < 0){Cn[i, 1] <- 0}
if(Cn[i, 2] > 1){Cn[i, 2] <- 1}
} # end for

conIntL <-Cn[, 1] # intervalo superior
conIntU <-Cn[,2] # intervalo inferior

plot(n, rep(.4, 9), type="l", ylim=c(0.3,0.5),
     xlab = "Tama�o de la muestra", main = "Intervalo de confianza", 
     ylab="p = 0.4")

lines(n, conIntU,  type="l", col="red")
lines(n, conIntL,  type="l", col="red")


#c) Grafique la longitud del intervalo contra n. Suponga que deseamos que la longitud del
# intervalo sea menor que 0.05. �Qu�e tan grande debe ser n?
Dist<-numeric(0)
for(i in seq(1, 9, 1)){ # calcula distancia entre intervalo sup. e inf.
Dist[i] <- dist(Cn[i, ])
} # end for

# Este plot se encuentra sin delimitar el eje de las abscisas
plot(n, Dist, type="l",ylim=c(0,.06), xlab = "Tama�o de la muestra",
     ylab = "Longitid del intervalo")
abline(.05, 0, col="red")
# Para observar que tan grande debe ser n se realiza por medio de
# un an�lisis visual. El siguiente plot se le delimit� el eje de las abcisas
# para observal el tama�o de muestra necesaria para que la longitud
# del intervalo sea menor a 0.05. 
plot(n, Dist, type="l",ylim=c(0,.06), xlab = "Tama�o de la muestra",
     ylab = "Longitid del intervalo")
abline(.05, 0, col="red")
# Como se observa una muestra cercana a los 4 mil, genera una longitud
# de los intervalos de confianza de un 0.05.


# Pregunta 5
# El siguiente conjuntos de datos contiene mediciones del diametro de un agave, medido en
# decmetros, en distintas localizaciones no cercanas.

#a) Escriba una funcion en R que calcule la funcion de distribucion emprica para un conjunto
#de datos dado. La funcion debe tomar como parametros al punto x donde se evalua y al
#conjunto de datos D. Utilizando esta funcion graque la funcion de distribucion emprica
#asociada al conjunto de datos de lluvias. Ponga atencion a los puntos de discontinuidad.

diamAgave <- c(23.37, 21.87, 24.41, 21.27, 23.33, 15.20, 24.21, 27.52, 15.48, 27.19,
25.05, 20.40, 21.05, 28.83, 22.90, 18.00, 17.55, 25.92, 23.64, 28.96,
23.02, 17.32, 30.74, 26.73, 17.22, 22.81, 20.78, 23.17, 21.60, 22.37)
# puntos a evaluar, se pude modificar segun el usuario
x <- seq(from = min(diamAgave) - 1, to = max(diamAgave) + 1, by = 1)
n <- length(diamAgave)
# De acuerdo con Gramacki, A. (2018),  la densidad gaussiana genera estimaciones m�s suavizadas
# que otros tipos de kernels, es por eso, que se utiliza un kernel gaussiano.

kernelDistribution <-function(x, h, data){
gauss <- sapply(diamAgave, function(a) ((1/sqrt(2*pi))*exp((-((x-a)^2)/(2*h^2))))/(n*h)) # a cada fila se lo calcula

plot(x, cumsum(rowSums(gauss)), 
     type = "S", xlab = "x", ylab = "F(x)", 
     main = " Funci�n de distribuci�n", lwd = 2) #suma de cada bumps

}

kernelDistribution(x, 1.37, diamAgave)
�Que observa?

kernelDensity <-function(x, h, data){
  gauss <- sapply(diamAgave, function(a) ((1/sqrt(2*pi))*exp((-((x-a)^2)/(2*h^2))))/(n*h)) # a cada fila se lo calcula
  
  plot(x, rowSums(gauss), 
       type = "l", xlab = "x", ylab = "f(x)", 
       main = " Densidad di�metro de un agave", lwd = 2) #suma de cada bumps
  rug(x, lwd = 2)
  out <- apply(gauss, 2, function(b) lines(x, b))
  lines(density(diamAgave), lty = 3)
  
}

kernelDensity(x, 1.37, diamAgave)

# la linea con dash es la que calcula R con density. Se traslapa dicha linea
# para observar como nuestra funci�n se aproxima a density.

# �Que observa?

#b) Escriba una funcion en R que determine la graca Q-Q normal de un conjunto de datos.
#La funcion debe tomar como parametro al conjunto de datos. Usando esta funcion,
#determine la graca Q-Q normal. >Que observa?


# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : qqplotFunction
# Gr�fica qq-plot
#
# Input: Datos
# Outpu: QQ-PLOT gr�fica
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
#qqplot
qqplotFunction<-function(data){
sort(data)
n <- length(data)
x <- 1:length(data)
p <- x/(n+1)
print(cbind(data, x, p))
teorico <-qnorm(c(0.25, 0.75))
estimado <- quantile(data, c(0.25, 0.75)) 
slope <- diff(estimado) / diff(teorico)      
int <- estimado[1] - slope * teorico[1]
plot(qnorm(p), sort(diamAgave),  type = "p",
     ylab = "Observaciones", xlab ="Cuantil te�rico",
     main = "Normal QQ- plot")
abline(int, slope)

}

qqplotFunction(diamAgave)
# �Qu� observa?

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : IC
# Calcula una simulaci�n en la cual 
# contabiliza el n�mero de veces en el que
# el intervalo de confianza contiene al 
# al valor de la media teorica (o poblacional)
# El intervalo de confianza se realiza con la 
# desigualdad de Hoeffdinf
#
# Input: N�mero de la muestra
# Outpu: N�mero de veces que cae en el intervalo 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
#qqplot con intervalos de confianza
qqplotFunctionIC<-function(data, d, CI){

  n <- length(data)
  x <- 1:length(data)
  p <- x/(n+1)
  print(cbind(data, x, p))
  teorico <-qnorm(c(0.25, 0.75))
  estimado <- quantile(data, c(0.25, 0.75)) 
  slope <- diff(estimado) / diff(teorico)      
  int <- estimado[1] - slope * teorico[1]
  Fest <- p
  Fn <- pnorm(sort(diamAgave), mean(diamAgave), sd(diamAgave))
  lowF <- Fn - d
  lowF[which(lowF < 0)] <- 0
  upperF <- Fn + d
  upperF[which(upperF > 1)] <- 1
  if(CI == 1){
  plot(qnorm(Fest), sort(diamAgave),  type = "p",
       ylab = "Observaciones", xlab ="Cuantil te�rico",
       main = "Normal QQ- plot")
  lines(qnorm(lowF), sort(diamAgave), col = "red")
  lines(qnorm(upperF), sort(diamAgave), col = "red")
  lines(qnorm(pnorm(sort(diamAgave), mean(diamAgave), sd(diamAgave))), sort(diamAgave),  type = "l")
  } else if (CI == 0){
    plot(qnorm(p), sort(diamAgave),  type = "p",
         ylab = "Observaciones", xlab ="Cuantil te�rico",
         main = "Normal QQ- plot")
    abline(int, slope)
}
 }

qqplotFunctionIC(diamAgave, .05, 0)
qqplotFunctionIC(diamAgave, .01, 0)

# �Qu� observa?

#d) Escriba una funcion en R que determine el graco de probabilidad normal. La funcion
#debe tomar como parametro al conjunto de datos. >Que observa?
Normal <- function(data) {
  x <- seq(from = min(data) - 1, to = max(data) + 1, by = 1)
  xmean <- mean(data)
  xvar <- var(data)
  xsd <- sd(data)
  normal <- (1/(sqrt(2*pi)*xsd))*exp(-((x-xmean)^2)/(2*xvar))
  plot(x, normal, type ="l", xlab = "Tama�o de muestra",
       ylab = "f(x)", main = "Probabilidad Normal")
}
Normal(diamAgave)
# �Qu� observa?

#Bibliograf�a 

 # Pregunta 6.
# a) Escriba una funcion en R que calcule el estimador de la densidad por el metodo de
#kerneles. La funcion debera recibir al punto x donde se evalua al estimador, al parametro
#de suavidad h, al kernel que se utilizara en la estimacion y al conjunto de datos.

rm(list = ls()) # limpiando environment

# Cargar el archivo correspondiente "Tratamiento.csv"
dataTratamiento <- read.csv(file.choose(), header=TRUE)

# Observando  los datos
head(dataTratamiento)
summary(dataTratamiento)

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : kernelDensity
# Calcula estimador de la densidad mediante
# el m�todo de kernels
#
# Input: 
#          x: Puntos a evaluar
#          h: el par�metro de suavidad
#          kernel: tipo de kernel para la estimaci�n
#                  1 -> Gaussiano
#                  2 -> Triangular
#                  3 -> Uniforme
#                  4 -> Epanechnikov
#                  5 -> Biweight
#                  6 -> Triweight
#                  
#          data: conjunto de datos
#
# Outpu: Gr�fic� con la densidad estimada de los datos,
#          traslapando las densidades por cada punto evaluado y
#          traslapando la densidad de los datos con la funci�n
#          density, la cual utiliza una banda �ptima.
## # # # # # # # # # # # # #  # # # # # # # # # # # # 

kernelDensity <-function(x, h, kernel, data){
  # Kernel Gaussiano
  if(kernel==1){
    print("Seleccion� Un kernel Normal para la Estimaci�n")
    gauss <- sapply(data, function(a) ((1/sqrt(2*pi))*exp((-((x-a)^2)/(2*h^2))))/(n*h)) # a cada fila se lo calcula
    plot(x, rowSums(gauss), 
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(gauss, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
  }else if(kernel==2){
    # Kernel Triangular
    print("Seleccion� Un kernel Triangular para la Estimaci�n")
    tri <- function(x) (abs(x) < 1) * (1 - abs(x))
    triangular <- sapply(data, function(a) tri((x - a)/h)/(n * h)) # a cada fila se lo calcula
    plot(x, rowSums(triangular), 
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(triangular, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
    
  }else if(kernel==3){
    # Kernel Uniforme
    print("Seleccion� Un kernel Uniforme para la Estimaci�n")
    unif <- function(x) (abs(x) < 1) * 0.5
    uniforme <- sapply(data, function(a) unif((x - a)/h)/(n * h)) # a cada fila se lo calcula
    plot(x, rowSums(uniforme), 
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(uniforme, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
  }else if(kernel==4){
    # Kernel Epanechnikov
    print("Seleccion� Un kernel Epanechnikov para la Estimaci�n")
    epanech <- function(x) {(abs(x) < 1) * ((3/4*(1-x^2)))}
    epanechnikov <- sapply(data, function(a) epanech((x - a)/h)/(n * h)) # a cada fila se lo calcula
    plot(x, rowSums(epanechnikov), 
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(epanechnikov, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
    
  }else if(kernel==5){
    # Kernel Biweight 
    print("Seleccion� Un kernel Biweight para la Estimaci�n")
    Biw <- function(x) {(abs(x) < 1) * ((15/16*(1-x^2)^2))}
    Biweight <- sapply(data, function(a) Biw((x - a)/h)/(n * h)) # a cada fila se lo calcula
    plot(x, rowSums(Biweight),
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(Biweight, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
  }else if(kernel==6){
    # Kernel Triweight
    print("Seleccion� Un kernel Triweight para la Estimaci�n")
    Triw <- function(x) {(abs(x) < 1) * ((35/32*(1-x^2)^3))}
    Triweight <- sapply(data, function(a) Triw((x - a)/h)/(n * h)) # a cada fila se lo calcula
    plot(x, rowSums(Triweight),
         type = "l", xlab = "x", ylab = "density", lwd = 2) #suma de cada bumps
    rug(x, lwd = 2)
    out <- apply(Triweight, 2, function(b) lines(x, b))
    lines(density(data), lty = 3)
  }
} # end kernleDensity


# El archivo contiene la duraci�n de los per�odos de tratamineto
# (en d�as) de los pacientes de control en un estudio de suicidio
tratamiento <- data.matrix(dataTratamiento)
# puntos a evaluar, se pude modificar segun el usuario
x <- seq(from = min(tratamiento) - 1, to = max(tratamiento) + 1, by = .1)

# Caso 1: con h igual a 20
h <- 20 # par�metro de suavizamiento
n <- length(tratamiento) # n�mero de observaciones

kernelDensity(x, 20, 1, tratamiento) # estimaci�n, karnel gaussiano
kernelDensity(x, 20, 2, tratamiento) # estimaci�n, karnel Triangular
kernelDensity(x, 20, 3, tratamiento) # estimaci�n, karnel Uniforme
kernelDensity(x, 20, 4, tratamiento) # estimaci�n, karnel Epanechnikov
kernelDensity(x, 20, 5, tratamiento) # estimaci�n, karnel Biweight
kernelDensity(x, 20, 6, tratamiento) # estimaci�n, karnel Triweight

#Antes del an�lisis, se vuelve a recordad que la densidad contra la que se traslapa
#la estimaci�n kernel, es contra la funci�n density, la cual calcula un h �ptimo.
#Esto con el fin de saber que la funci�n que se program� se aproxima a  una ya
#realizada en R.
# La comparaci�n de pr�metro de suavidad se centrar� en los Kernel
# gaussianos. Sin embargo, se discute un poco sobre la estimaci�n 
# con los distintos tipos de kernel para cada valor de h.
# A manera muy general, entre los kernels con los que se puede estimar la densidad,
#se observa, que el gaussiano con un suavizador de 20 se aproxim� m�s que los
# otros posibles tipos de kernel. No obstante, con un h igual a 20 
# se puede observar que con ese valor a�n se observa un poco de variabilidad.
# mostrando algunos intervalos con algunos picos.


# Caso 2: con h igual a 30
h <- 30
kernelDensity(x, 30, 1, tratamiento) # estimaci�n, karnel gaussiano
kernelDensity(x, 30, 2, tratamiento) # estimaci�n, karnel Triangular
kernelDensity(x, 30, 3, tratamiento) # estimaci�n, karnel Uniforme
kernelDensity(x, 30, 4, tratamiento) # estimaci�n, karnel Epanechnikov
kernelDensity(x, 30, 5, tratamiento) # estimaci�n, karnel Biweight
kernelDensity(x, 30, 6, tratamiento) # estimaci�n, karnel Triweight
# Con un h igual a 30, la dessidad estimada con un kernel gaussiano parece
# lo suficientemente suavizada.
# A lo que respectan los otros tipos de kernel, no onstante, la variabilidad es muy marcada
# a ese valor del par�metro, har�a falta un h m�s grande para que tendieran a la gaussiana. 

# Caso 3: con h igual a 60
h <- 60
kernelDensity(x, 60, 1, tratamiento) # estimaci�n, karnel gaussiano
kernelDensity(x, 60, 2, tratamiento) # estimaci�n, karnel Triangular
kernelDensity(x, 60, 3, tratamiento) # estimaci�n, karnel Uniforme
kernelDensity(x, 60, 4, tratamiento) # estimaci�n, karnel Epanechnikov
kernelDensity(x, 60, 5, tratamiento) # estimaci�n, karnel Biweight
kernelDensity(x, 60, 6, tratamiento) # estimaci�n, karnel Triweight
# Un valor del par�metro de suavidad de 60, genera que la estimaci�n
# con el kernel gaussiano se encuentra muy suavizado. El tener una h muy
# grande implica el trade off que ha mayor h, nayor sesgo, y a menor h, mayor variabilidad
# En este sentido, la densidad estimada se encuentra muy suavizada.
# Para los otros tipos de kernel, lo interesante es que presentan un comportamiento aproximado
# a un kernel gaussiano, sin embargo, no tan suavizado con un h de 60 como el 
# kernel gaussiano.

# Conclusion, el par�metro de suavizamiento de 30, se va a considerar
# el mejor entre el de 20 y el de 60. Con el de 20, h es muy chico,lo cual 
#deja ver curvas no lo suficientemente suavizadas, y con ello muchas curvas espurias. Con un h de 60, por otro lado, 
#genera una suavizaci�n de m�s, ocultando de m�s informaci�n sobre los datos. Recordando, el trade off entre el sesgo y 
#la variabilidad, que a mayor h tenemos m�s sesgo, y a menor h mayor variabilidad.
#De esta forma, con un par�metro de suavidad de 30, la densidad estimada
#parecer�a est�r mejor estimada que con los anteriores valores de h, dejandola lo suficientemente
#suave para obtener una buena estructura de informaci�n sobre los datos.

#7. Cargue en R al conjunto de datos "Ma�iz.csv", el cual contiene el precio mensual de la tonelada
#de ma�iz y el precio de la tonelada de tortillas en USD. En este ejercicio tendr�a que estimar
#los coeficientes de una regresi�on lineal simple.
#a) Calcule de forma expl�icita la estimaci�on de los coeficientes via m�inimos cuadrado y ajuste
#la regresi�on correspondiente. Concluya.

rm(list = ls()) # limpiando environment

#Cargue el archivo correspondiente (Maiz)
data <- read.csv(file.choose(), header=TRUE)

#Conociendo los datos
head(data)
cor.test(data$P..Tonelada.Tortilla, data$P..Tonelada.Ma�z) #datos  orrelacionados
# como se esperaba una coeficiente de corelaci�n  positiva mayor al .5, 
# expresando la relaci�n lineal entre ambas variables, la cual es
# prudente por ser ma�z insumo de algun tipo de tortillas

plot(data$P..Tonelada.Ma�z, data$P..Tonelada.Tortilla ,
     ylab = "Tonelada Tortillas",
     xlab = "Tonelada Maiz", col="red")# la relaci�n de dependencia lineal positiva

# Con el fin de contrastar mi estimaci�n explicita de los
# coeficientes de regresi�n 

# Se ajusta una regresi�n en la cual modela
# la esperanza condicional de las toneladas de tortilla dado
# las toneladas de m��z
lm(data$P..Tonelada.Tortilla~data$P..Tonelada.Ma�z)

# Variable Dependiente de la regresi�n lineal 
# Media
mediaTortilla<-sum(data$P..Tonelada.Tortilla)/length(data$P..Tonelada.Tortilla)
# Variable Independiente de la regresi�n lineal
# Media
mediaMaiz <-sum(data$P..Tonelada.Ma�z)/length(data$P..Tonelada.Ma�z)
# Varianza
varMaiz <-sum((data$P..Tonelada.Ma�z - mediaMaiz)^2)/(length(data$P..Tonelada.Ma�z)-1)
# covarianza entre toneladas de tortilla y maiz
cov <- sum(((data$P..Tonelada.Ma�z - varMaiz))*((data$P..Tonelada.Tortilla - mediaTortilla)))/(length(data$P..Tonelada.Ma�z)-1)
#coeficiente de regresi�n
best <- cov/varMaiz # beta estimada (pendiente de la regresi�n)
aest <- mediaTortilla-best*mediaMaiz # (constante de la regresi�n)

best;aest
# se observa que son los mismos que los que genera R con la funci�n lm

# Interpretaci�n, por cada incremento de una tonelada en el ma�z
# genera un incremento de .4600, m�s su valor constante de 684.9545, en el caso de que 
# querer hacer predicci�n sobre alg�n valor en especifico de x.






# Estimaci�n propia, en rojo la linea de regresi�n estimada 
# con los coeficientes previamente obtenidos.
plot(data$P..Tonelada.Ma�z, data$P..Tonelada.Tortilla,
     ylab = "Tonelada Tortillas",
     xlab = "Tonelada Maiz")
lines(100:200, aest+best*(100:200), type="l", col = "red")

#b) Calcule de forma explcita la estimacion de los coecientes via regresion no-parametrica
#tipo kernel (ver Nadaraya, E. A. (1964). \On Estimating Regression". Theory of Probability
# and its Applications. 9 (1): 141{2. doi:10.1137/1109020) y ajuste la regresion
# correspondiente. Concluya.


KernelRgression<- function(x, y, h) {
xgril <- seq(from = min(x) - 1, to = max(x) + 1, by = .1) # puntos a evaluar
n <- length(x) # n�mero de observaciones

gauss <- sapply(x, function(x) (((1/sqrt(2*pi))*exp((-((xgril-x)^2)/(2*h^2))))/(n*h))) # kernel gaussiano

Y<-matrix(0L, 462, 200) # generando matriz para guardar resultados de kernel ponderado
  for (i in 1:200){
    Y[ ,i] <- ((((1/sqrt(2*pi))*exp((-((xgril-x[i])^2)/(2*h^2))))*y[i])/(n*h))
  } # end for 

KernelXY <-rowSums(Y)
kernelX <-rowSums(gauss)

X <- KernelXY/kernelX

plot( x,y, main=paste("Regresi�n por kernel h =", h), 
     xlab="Precio Tonelada de Maiz (Dolares)", ylab="Precio Tonelada de Tortillas (Dolares)", pch=20) 
lines(xgril, X, type = "l",col="steelblue", pch=3, lwd = 2) #contra el grill

}


x<- data$P..Tonelada.Ma�z
y <- data$P..Tonelada.Tortilla

KernelRgression(x,y,1)
KernelRgression(x,y,1.37)
KernelRgression(x,y,2)
KernelRgression(x,y,3)
KernelRgression(x,y,4)

# comparando con Con un h = 4
#Kernel regresi�n
KernelRgression(x,y,4) 

#MCO
plot(data$P..Tonelada.Ma�z, data$P..Tonelada.Tortilla,
     ylab = "Tonelada Tortillas",
     xlab = "Tonelada Maiz")
lines(100:200, aest+best*(100:200), type="l", col = "red")

Concluya. Cuando se utiliza la regresi�n con el m�todo de kernel
el par�metro de suavidad al modificarlo, puede encontrar un mejor ajuste que 
la estimaci�n con m�nimos cuadrados ordinarios. En cuanto la comparaci�n, se puede
observar, que si se utiliza un par�metro de suavidad de cuatro (h = 4), 
la regresi�n por kernel ajusta de una forma similar a la regresi�n con MCO.
