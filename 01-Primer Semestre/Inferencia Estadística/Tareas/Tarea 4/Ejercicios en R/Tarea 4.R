#a) Escriba la siguiente funci�on en R. Simule una muestra de tama~no n de una variable
#aleatoria Exponencial(??) y calcule el estad�istico Zn ??????n(Xn????????1) ?????1 . Repita lo anterior
#m veces. La funci�on deber�a tomar como par�ametros n, m y ?? y regresar un vector de
#tama~no n conteniendo la muestra de Zn.


Zn <- function(n, lambda, m) {
set.seed(0)
Z <- rep(0, m)
Z <- replicate(m, (sqrt(n)*(mean(rexp(n, lambda))-(lambda)^(-1)))/(lambda)^(-1))
return(Z)
}

# b) Para n = 5, 10, 100, 500, 1000, 10000, m = 1000 y ?? = 1, utilice la funci�on del inciso
# anterior para obtener muestras de Zn. Graf�ique las muestras anteriores en un histograma
# (un histograma para cada n). �Qu�e observa? �Qu�e tiene que ver su resultado con el
# TCLC?
  
n <- c(5, 10, 100, 500, 1000, 10000)
i <- 1:6
Muestra <<- matrix(0L, 1000, 6)
auxiliar <- sapply(i, function(i) hist(Muestra[, i] <<- Zn(n[i], 1, 1000), main = paste("St normal distribution n =", n[i]),
     xlab = "Zn",  col ="darkviolet", border = "black") )
View(Muestra)
#hist(Zn(10, 1, 1000),main = paste("St normal distribution m =", 1000),
 #    xlab = "Zn",  col ="darkviolet", border = "black")
#hist(Zn(100, 1, 1000), main = paste("St normal distribution m =", 1000),
  #   xlab = "Zn",  col ="darkviolet", border = "black")
#hist(Zn(500, 1, 1000), main = paste("St normal distribution m =", 1000),
   #  xlab = "Zn",  col ="darkviolet", border = "black")
#hist(Zn(1000, 1, 1000), main = paste("St normal distribution m =", 1000),
    # xlab = "Zn",  col ="darkviolet", border = "black")
#hist(Zn(10000, 1, 1000), main = paste("St normal distribution m =", 1000),
     #xlab = "Zn",  col ="darkviolet", border = "black")

�Qu� observa? 
Se observa que al repetir el calculo del estad�stico 1000 veces, y el valor
de n se vuelve cada vez m�s grande, la variable aleatoria Zn, presenta una distribuci�n limite
parecida a la normal estandar.
�Qu� tiene que ver su resultado con TCL?
El teorema nos dice que una secuencia de variables aleatorias independientes e indenticamentes 
distribuidas - en nuestro caso las exponenciales con par�metro lambda-, cuya generadora de momentos
exista, y con ello media y varianza sea finita. Entonces, al normalizar la media de la secuencia 
de variables aleatorias e incrementar n, la variable aleatoria estandar tiende a una
distribuci�n l�mite, la cual es una normal estandar.
Por lo tanto, al normalizar cualquier variable aleatoria , pero que cumpla con lo necesarios previamente 
mencionado, entonces, dicha sucesi�n de variables estadarizadas converger� en distribuci�n
a una normal estandar.

c) Para cada una de las muestras generadas en el inciso anterior, encuentre el Q-Q plot y
el P-P plot normales. Comente sus resultados.

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : qqplotFunction
# Gr�fica qq-plot
#
# Input: Datos
# Outpu: QQ-PLOT gr�fica
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
#qqplot
qqplotFunction<-function(data, sizeSample){
  sort(data)
  n <- length(data)
  x <- 1:length(data)
  p <- x/(n+1)
  
  teorico <-qnorm(c(0.25, 0.75))
  estimado <- quantile(data, c(0.25, 0.75)) 
  slope <- diff(estimado) / diff(teorico)      
  int <- estimado[1] - slope * teorico[1]
  plot(qnorm(p), sort(data),  type = "p",
       ylab = "Observaciones", xlab ="Zi",
       main = paste("Normal QQ- plot   n =", sizeSample), pch=20)
  abline(int, slope)
  
}

qqplotFunction(Muestra[ ,6], n[6]) 
i <- 1:6
auxiliar2 <- sapply(i, function(i) qqplotFunction(Muestra[ ,i], n[i]) )

Cuando la muestra es de n = 5, claramente se observa que presenta sesgo a la derecha (izquierda para el que observa). y por ende,
no aproxima correctamente a una normal. Dicho sesgo, se va corrigiendo. Para una
muestra de 10000 observaciones, la distribuci�n mmuestral converge en distribuci�n
a una normal. Lo cual, se ve representado por el qq-plor (los puntos sobre la gr�fica
se encuntran sobre la recta).

En conclusi�n, bajo ciertas condiciones, al estandarizar las variables aleatorias cuya
distribuci�n es una exponencial, cuando la muestra (n) tiende a infinito (sea grande), entonces,
la variable aleatoria estandarizada tiende a converger en distribuci�n  a una distribuci�n
l�mite normal estandar.

Caso en el que presenta sesgo a la derecha (izquierda para el que observa)
par(mfrow=c(1,2))
qqplotFunction(Muestra[ ,1], n[1]) 
hist(Muestra[ ,1])
Caso en el que aproxima a una normal
par(mfrow=c(1,2))
qqplotFunction(Muestra[ ,6], n[6]) 
hist(Muestra[ ,6])

#qqplotFunction(Muestra[ ,2]) 
#qqplotFunction(Muestra[ ,3])
#qqplotFunction(Muestra[ ,4])
#qqplotFunction(Muestra[ ,5])
#qqplotFunction(Muestra[ ,6])

pp_plotFunction<-function(data, sizeSample){
  n <- length(data)
  x <- 1:length(data)
  pest <- x/(n+1)
  teorico <-pnorm(c(0.25, 0.75))
  estimado <- c(0.25, 0.75) 
  
  slope <- diff(estimado) / diff(teorico)      
  int <- estimado[1] - slope * teorico[1]
  
  Fest <- pest
  Fn <- pnorm(sort(data), mean(data), sd(data))
  
  plot(Fest, Fn,  type = "p",
       ylab = "Observaciones", xlab ="zi",
       main = paste("Normal PP- plot n = ", sizeSample), pch=20)
}

pp_plotFunction(Muestra[ , 1], n[1])
i <- 1:6
auxiliar3 <- sapply(i, function(i) pp_plotFunction(Muestra[ ,i], n[i]) )

Comente sus resultados.


3. En este ejercicio volver�a a trabajar con el TCLC.
a) Escriba una funci�n an�loga a la pedida en el inciso 2a) para una distribuci�on Binomial(p, N).
La funci�on deber�a tomar los mismos par�ametros a los pedidos en el inciso 2a), con excepci�on
al par�ametro ?? que tendr�a que ser sustituido p y N.


Zn2 <- function(n, p, N, m) {
  set.seed(0)
  Z <- rep(0, m)
  Z <- replicate(m, (sqrt(n)*(mean(rbinom(n, size = N, prob = p))-(N*p))/(sqrt(N*p*(1-p)))))
  return(Z)
}

b) Para p = 1/2 y N = 15, repita los incisos 2b) y 2c) para el caso Binomial de este
ejercicio.
n <- c(5, 10, 100, 500, 1000, 10000)
i <- 1:6
Muestra <<- matrix(0L, 1000, 6)
auxiliar <- sapply(i, function(i) hist(Muestra[, i] <<- Zn2(n[i], .5, 15,1000), main = paste("St normal distribution n =", n[i]),
                                       xlab = "Zn",  col ="darkviolet", border = "black") )
View(Muestra)


qqplotFunction(Muestra[ ,6], n[6]) 
i <- 1:6
auxiliar2 <- sapply(i, function(i) qqplotFunction(Muestra[ ,i], n[i]) )

Al realizar el QQ-plot de la distribuci�n binomial, se observa que con un tama�o de
muestra peque�o (n = 5), la distribuci�n estimada se aproxima al centro de la teor�ca.
Una vez incrementando el n�mero de muestra, se puede ver que el efecto de escalonamiento -debido a que 
es una variable aleatoria discreta- pasa a haacer algo m�s apriximado a lo continuo, 
dejando m�s apreciable como el teor�ma del l�mite central se cumple, haciendo que
la sucesi�n de binomiales al estandarizarlas y tomar l�mite llevando la muesra al infinito
converge en distribuci�n a una normal estandar.

pp_plotFunction(Muestra[ , 1], n[1])
i <- 1:6
  auxiliar3 <- sapply(i, function(i) pp_plotFunction(Muestra[ ,i], n[i]) )

En el caso del PP- plot, presenta un comportamiento similar a lo presentado 
con el gr�fico de QQ-plot. Est� es, que bajo muestras peque�as se observa que
el contraste de las probabilidades empiricas y teor�cas rondan sobre la normal, peros
es hasta  que el n�mero de muestra incrementa cuando se aprecia lo anterior mencionado.







c) Para p = 0.1, N = 15, n = 5; 10; 20; 100 y m = 1000, genere muestras de Zn y grafque
est�s muestras en un histograma (un histograma para cada n). >Que observa? Explque.


n <- c(5, 10, 20, 100)
i <- 1:4
Muestra <- matrix(0L, 1000, 4)
auxiliar <- sapply(i, function(i) hist(Muestra[, i] <<- Zn2(n[i], 0.1, 15,1000), main = paste("St normal distribution n =", n[i]),
                                       xlab = "Zn",  col ="darkviolet", border = "black") )
View(Muestra)
hist(Muestra[, 4])

�Qu� observa? Explique
Cuando el n�mero de mmuestra es de 5 y diez observaciones, la funci�n de densidad se aproxima a la de una distribuci�n normal estandar, 
m�s sin embargo sigue sin ser la funci�n de densidad de una normal estandar. Esto debido a que sigue presentando inconsistencias
(i.e quiebres) en el centro de la distribuci�n. Se aproxima cada vez m�s en las colas, pero
no en el centro. La aproximaci�n en el centro de la suceci�n estandarizada de binomiales
mejora conforme el tama�o de la muestra crece. Para un tama�o de muestra de cien, se puede apreciar
como la distribuci�n presentada se aproxima en tanto colas y centro de densidad de una  distribuci�n 
a una normal estandar. En conclusi�n, bajo las condiciones de que la suceci�n de binomiales
tiene momentos finitos, y dado a que la esperanza es finita existe una generadora de momentos,
entonces, aplicando l�mite central, se observa como la media de la sucesi�n de variables aleatorias estandarizada
converge en distribucion a una distribuci�n l�mite normal estandar

d) Repita el inciso anterior para p = 0:99. Compare su resultado con lo obtenido en el inciso
anterior.

n <- c(5, 10, 20, 100)
i <- 1:4
Muestra <- matrix(0L, 1000, 4)
auxiliar <- sapply(i, function(i) hist(Muestra[, i] <<- Zn2(n[i], 0.99, 15,1000), main = paste("St normal distribution n =", n[i]),
                                       xlab = "Zn",  col ="darkviolet", border = "black") )
View(Muestra)
hist(Muestra[, 4])

Con una probabilidad de 0.99, de una distribuci�n binomial, y con un tama�o de muestra
peque�o (i.e de 5 a 20 observaciones), su funci�n de probabilidad presenta sesgo a la izquierda
(derecha para el quien lo observa), es decir, la forma de los datos se cargan hacia la izquierda (a la derecha de quien lo ve)
entendiendo que la funci�n de probabilidad no es simpetrica (as�metrica).
No obstante, cuando el tama�o de la muestra incrementa se puede observar como la distribuci�n
tiene a reducir dicho sesgo. Por lo tanto, bajo el teorema del l�mite central, bajo ciertas
condiciones al incrementar el tama�o de muestra y  estandarizar los eventos binomiales, dicha distribuci�n
converge a la funci�n de densidad de una distribuci�n normal estandar. El sesgo desaparece en su gr�n mayoria, pero a�n
las observaciones del centro no logran aproxximar del todo bien a la normal estandar.


b) Simule una sucesion de n = 1000 v.a. como arriba y calcule S1000 para p = 0:4. Repita
este proceso 100 veces y grafque la distribucion empirica de S1000 que se obtiene de la
simulacion y empalmela con la distribucion asintotica teorica que obtuvo. Comente sus
resultados.
library("Rlab", lib.loc="~/R/win-library/3.3")

SnSimulation <-function() { 
Sn <- rep(0, 1000)
i<- 1: 1000
sapply(i, function (i) {x<-rbern(1, 0.4); x2<-rbern(1, 0.4);
       if (x ==1 && x2 ==1){
         Sn[i] <<- 1 } else { Sn[i] <<- 0}})
return(apply(as.matrix(Sn), 2, sum))
}


n<- 1000
p<- .4
mean2 <- n*(p^2)
sd2<-sqrt(n*((p*(1-p))*(p*(1-p)))) 
FnEmpirica<- replicate(100, SnSimulation())

hist(FnEmpirica, main = "Distribuci�n empirica S = 100",
     xlab = "Sn",  col ="darkviolet", border = "black")

FnTeorica <- replicate(100,rnorm(1, mean2 , sd2))
hist(FnTeorica,  main = "Distribuci�n te�rica S = 100",
     xlab = "Sn",  col ="darkviolet", border = "black")

library("ggplot2", lib.loc="~/R/win-library/3.3")

# Pasando a formato data frame
geom1 <- data.frame(x = FnEmpirica, group="Empirica")
geom2 <- data.frame(x = FnTeorica, group="Teorica")


graf <- rbind(geom1, geom2)

# Histogramas de las tres simulaciones
ggplot(graf, aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..count..), breaks=seq(130,200,5), alpha=.5, 
                 position="identity", lwd=0.2) +
  ggtitle("Distribuci�n asint�tica Sn") 

 


# Ejercicio: 552 morales
x <- c(5,10,100,500,1000)
i<-1:5



sapply(i, function (i) hist(replicate(500,mean(rnorm(x[i], 0, 1)))))
sapply(i, function (i) qqplotFunction(replicate(500,mean(rnorm(x[i], 0, 1))), x[i]) )


sapply(i, function (i) hist(replicate(500,sqrt((cumsum(rnorm(x[i], 0, 1)-mean(rnorm(x[i], 0, 1)))^2)/(x[1]-1)))))
sapply(i, function (i) qqplotFunction(replicate(500,sqrt((cumsum(rnorm(x[i], 0, 1)-mean(rnorm(x[1], 0, 1)))^2)/(x[i]-1))), x[i]) )

