EJERCICIO 6 

c) Determine m� de MV n�mericamente para el caso de que la media muestral sea 3.2
mt <- .0001 # incrementos
m0 <- 0 # valor inicial
y <- numeric(1000) #n�mero de simulaciones
xbarra <- 3.2 # media muestral 
# Forma iterativa ( recursiva)
for(i in 1:1000){
y[i] <- xbarra - xbarra*exp(-(m0+mt))
m0 <- y[i]
} # end for

El estimador de maxima verosimilitud tiene unvalor de: 
print(kable(
  cbind('Estimador MV'=c('Mu estimado'= mean(y)))))


EJERCICIOS DIAPOSITIVAS

Diapositiva 145, inclusi�n a la variabilidad

Ejemplo: En una rama de la industria alimentaria, se realizan en forma
rutinaria mediciones del contenido de calcio en comida para animales
(mascotas). El metodo estandar utiliza precipitacion de oxalato de calcio
seguida de tritanio; es una tecnica que consume tiempo. Los resultados de
118 muestras (Heckman 1960), se dan en el archivo calcio.txt (columna
 1). Ahora, con lo que hemos discutido hasta aqu, >que podemos decir a
partir del comportamiento de estos valores muestrales?}
# Tomando los datos 
calcio <- read.delim("C:/Users/h_air/Desktop/Clase 21-20181027/calcio.txt")

Para iniciar, observaremos la distribuci�n empirica contra una distribuci�n 
normal teor�ca. Para esto, utilizamos un qqplot.

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : qqplot.data
# Gr�fica qq-plot
#
# Input: Datos
# Outpu: QQ-PLOT gr�fica
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
#qqplot



qqplot.data <- function (vec) # funci�n qqplot
  {
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
} # end qqplot

Medici�n de contenido de calcio en comida de mascotas con el m�todo
est�ndar el cual utiliza precipitacion de oxalato de calcio
seguida de tritanio.

qqplot.data(calcio$oxalate) 

Como se observa en la gr�fica, la distribuci�n muestral respecto
a una teorica normal, tiene una distribuci�n sesgada a la derecha (izquierda 
para quien observa), con algunos valores a tipicos, y colas pesadas. Por ultimo,
la distribuci�n empirica no se aproxima a una normal  en el centro de su distribuci�n. 

# histograma de la distribuci�n empirica

qplot(calcio$oxalate,
      geom="histogram")


As� como se comento con el qqplot, la distribuci�n empirica presenta sesgo, 
colas pesadas y, valores atipicos. Lo anterior se puede apreciar mejor con el histograma.

Tambi�n, se realiza un boxplot.

boxplot(calcio$oxalate)

El boxplot de la distribuci�n empirica, es decir, los datos sobre el m�todo est�ndar,
argumenta lo anterior dicho sobre los valores a tipicos. Tambien se ve de manera
marcada el sesgo a la derecha (izquiera para quien ve), y como su mediana se tira a la derecha


En las notas se indica que posiblemente la variable aleatoria sea una log normal.
Utilizando la serie con logaritmos veremos si la distribuci�n muesrral en logaritmos
se distribuye como una normal. 

boxplot(calcio$ln.oxalate.)

Se aprecia una mediana un poco a la izquierda, como la distribuci�n se jala
o se carga para el lado opuesto al no tener logaritmos,y por �ltimo, un valor a tipico 
perdido en la distribuci�n.

qqplot.data(calcio$ln.oxalate) 

Lo ya mencionado, se puede ver mejor con el qqplot. Los valores a tipicos
eb una de sus colas, y un poco de sesgo hacia el lado opuesto preevio de
aplicar logaritmos. Es importante observar como el logaritmo de la muestra
se distribuye como una normal.

qplot(calcio$ln.oxalate.,
      geom="histogram")

El histograma muestra de mejor manera lo descito con el qq plot. Note que la cola
de uno de los lados es m�s pesada de lo que se ve�a con el qqplot.

boxplot(calcio$flame)
Se observan datoa a tipico en el derecho de la distribuci�n (cola), y una
mediana cargada hacia el lado izquierdo

En conclusi�n la muestra sin logaritmos se distribuye como una lognnormal, pero
la muestra con logaritmos es una normal.


DIAPOSITIVA 179 


IC PARA CALCIO PARA LOS DATOS DE CALCIO  

#valor medio del calcio 
muEst<- mean(calcio$oxalate)
z <- 1.96 # z- valor
a <- .05 # Nivel de insignificanca
n <- 118
s <- sd(calcio$oxalate) # Des. Estandar

ICupper <- muEst + z*(s/sqrt(n)) # IC superior
IClower <- muEst - z*(s/sqrt(n)) # IC inferior
IC <- c(IClower, ICupper) # Intervalos de Confianza

# library("knitr", lib.loc="~/R/win-library/3.3") Install

print(kable(
  cbind('Intervalos de confianza'=c('LOWER'=IC[1], 'UPPER'=IC[2]))))

considerando los logaritmos y consideramos pivote
que sigue una t de student

LogMuEst<- mean(calcio$ln.oxalate) # media del logaritmo de la muestra

t de student con n-1 grados de libertad

t117 <- 1.96 # t- valor
a <- .05 # ni vel de insignificancia estad�stica
n <- 118
sLog <- sd(calcio$ln.oxalate) # Des. Estandar de los logaritmmos de la muestra

LogICupper <- LogMuEst + t117*(sLog/sqrt(n)) # IC superior
LogIClower <- LogMuEst - t117*(sLog/sqrt(n)) # IC inferior
LogIC <- c(LogIClower, LogICupper) # intervalos de confianza
ErrorEstimacion <- t117*(sLog/sqrt(n)) # error de estimaci�n

print(kable(
  cbind('Intervalos de confianza (log muestra)'=c('LOWER'=LogIC[1], 
                                                  'UPPER'=LogIC[2],
                                                  'Error de estimaci�n'=ErrorEstimacion))))

tomando transformaci�n inversa a los logaritmos del intervalo de confianza

InvLogIC <- c(exp(LogIClower), exp(LogICupper)) # intervalos con exponencial

print(kable(
  cbind('Intervalos de confianza (exponencial log muestra)'=c('LOWER'=InvLogIC[1], 
                                                  'UPPER'=InvLogIC[2]))))

En este ultimo intervalo deberamos notar que la relacion entre
la media de la variable original y la de su transformacion logartmica esta
afectada por una constante que aqu no aparece. Recuerda la relacion
entre medias y varianzas que hemos establecido al nal del captulo 3, para
la Normal y Lognormal. Trata de incorporar esta informacion para
entender mejor la diferencia numerica entre estos intervalos.  

sabemos que los momentos de una normal y lognormal
son distintos, ya que si una variable se distribuye como una
lognormal, su generadora de momentos no existe. Pero sabemos como
es la generadora de una normal, de esta forma la 
variable que se distribuye como una lognormal al sacarle
la esperanza es como si plantearas la generadora pero
con una e a la xt, sin t. lo cual el esperados es la generadora
de momentos de una normal pero evaluando t en el momento que se quiera
de la log normal.


Ahora comparamos las muestras del archivo txt. 
Calculamos la diferencia entre medias de los dos m�todos distintos.
El �todo con oxalte ser� el m�todo est�ndar, el Flame el alternativo.

meanA<-mean(calcio$oxalate)- mean(calcio$flame) # Diferencia entre medias
varA <- var(calcio$oxalate)/var(calcio$flame) # Cociente de variaci�n 

print(kable(
  cbind('Diferencia entre dos Poblaciones'=c('Diff. Medias'=meanA, 
                                                              'Cociente Varianzas'=varA))))

varA<-var(calcio$oxalate)/length(calcio$oxalate) + var(calcio$flame)/length(calcio$flame) 

Se oserva que el estad�stico de la diferencia de medias entre poblaciones, es relativamente peque�o.
Lo cual, ya era de esperarse dado a la descripci�n de los datos (i.e, vienen de poblaciones
        lo m�s homogeneas posibles).
Entonces, como se pretendia que la muestras fueran lo controladas pero independientes, se toma
un cr�terio de variabilidad. El cociente entre la varianza de la muestra estandar y la alternativa
es ligeramente mayor a uno. Sin embargo, decir que la muestra de la poblaci�n estandar es m�s variable
seria muy precipitado.

A continuaci�n, se construye un intervalo de confianza para la diferencia de medias

DSA<- sqrt(varA)
UPPER <- mean(calcio$oxalate)-mean(calcio$flame)+1.64*DSA
LOWER <- mean(calcio$oxalate)-mean(calcio$flame)-1.64*DSA

print(kable(
  cbind('Diferencia entre dos Poblaciones IC'=c('IC Lower'=UPPER, 
                                             'IC upper'=LOWER))))

Para seguir jugando un poco con los datos, se simula la proporci�n de veces
que el intervalo de confianza contiene al valor de la hip�tesis sobre la Nula.
Recordemos que dicho valor es fijo,o dado, y son los intervalos de confianza
la que es una variable aleatoria, la cual en cada simmulaci�n estar�n cambiando.

outsideCI <- numeric(1000)  # almacenando proporci�n de veces que IC no contiene}
# el valor de la hip�tesis nula

set.seed(1) # fijando semlla 
# Ho: X = 0 vs Ha: X otra cosa
for (i in 1:1000){
  xmean <- rnorm(118, mean(calcio$oxalate), sd(calcio$oxalate))
  ymean <- rnorm(118, mean(calcio$flame), sd(calcio$flame))
  CI.lower <- (mean(xmean)-mean(ymean)) - qt(0.975, n-1)*DSA
  CI.upper <- (mean(xmean)-mean(ymean)) + qt(0.975, n-1)*DSA
  outsideCI[i] <- ifelse(0 < CI.lower | 0 < CI.upper, 1, 0)
} # en for

mean(outsideCI) # proporcion de la veces que mi intervalo
# no cubre el valor de la Ho: 0
print(kable(
  cbind('proporci�n de veces que el IC contiene al valor de la Ho'=c('100%(1-alpha)'=mean(outsideCI)*100))))

El 98% de las veces el IC captura el valor de la nula


A continuaci�n, realizamos el mismo ejercicio pero utilizando el p-valor


set.seed(1)
reps <- 100000  # n�mero de simulaciones
## Aproximaci�n de los p valores

pvalues <- numeric(reps)
for (i in 1:1000) {
  xmean <- rnorm(118, mean(calcio$oxalate), sd(calcio$oxalate))
  ymean <- rnorm(118, mean(calcio$flame), sd(calcio$flame))
  t.stat <- ((mean(xmean)-mean(ymean)+(0))/DSA)
  pvalues[i] <- 2*(1 - pt(abs(t.stat), n-2))
  # Forma alternativa: pvalues[i] <- t.test(x, mu = mu0)$p.value
} # end for

mean(pvalues < 0.05) # Con un alpha de 0.05 
#[1] 0.66907
print(kable(
  cbind('p-valor'=c('100%(1-alpha)'=mean(pvalues < 0.05)*100))))

El 99 porciento de la veces el intervalo de confianza contiene
al valor de la hipotesisnula.

BOOTSTRAP

Se pretende realizar un ejercicios computacional y estad�stico utilizando bootstrap.
Cuyo fin es estimar los errores estandar de algun estad�stico.
El caso de Intervalos de confienza con m�todo bootstrap queda pediente parallel
el siguiente tema.

Se utiliza los datos del archivo txt.  

median(calcio$oxalate)  # Estad�stico seleccionado Mediana
tboot <- numeric(1000) # Almacena la simulaci�n
for(i in 1:1000){ # Mil simulaciones
  xstar <- sample(calcio$oxalate,10, TRUE) # Algoritmo, tomar muestra con repetici�n de la funci�n de distribuci�n empirica
  #equivale a tomar un punto aleatorio en los datos originales.
    tboot[i] <- median(xstar)
} # end for

median(tboot) #  mediana simulada por boostrap       
se <- sqrt(var(tboot)) # Error estandar de la mediana     



print(kable(
  cbind('Error estandar'=c('Mediana'=median(tboot),'Error estandar'=se),
        digits=3)))
# intervalos de confianza para el bootstrap
# diferencias de medianas 

x1 <- calcio$oxalate 
x2 <- calcio$flame
n1 <- length(x1)
n2 <- length(x2)
th.hat <- median(x2)-median(x1)
B <- 1000
tboot <- numeric(1000)
for (i in 1:B){
  xx1 <- sample(calcio$oxalate, n1, TRUE)
  xx2 <- sample(calcio$flame, n2, TRUE) 
  tboot[i] <- median(xx2) - median(xx1)
}
median(tboot)
se <- sqrt(var(tboot))
 # confidence intervals
th.hat debe ser cercano a la normal para que funcione est� intervalo Wasserman (a�o).
Normal <- c(th.hat - 2*se, th.hat + 2*se)
pivotal <- c(2*th.hat - quantile(tboot, .975),
               2*th.hat - quantile(tboot, .025))
percentil <- c(quantile(tboot, 0.25), quantile(tboot, .975))
               

print(kable(
  cbind('95% Intervalo L'=c('Normal'=Normal[1],
      'Pivote'=pivotal[[1]], 'Percentil'= percentil[[1]]),
      '95% Intervalo U'=c('Normal'=Normal[2],
                        'Pivote'=pivotal[[2]], 'Percentil'= percentil[[2]]))))

# intervalos de confianza para el bootstrap
# diferencias de medianas 

x1 <- c(576,635,558,578,666,580,555,661,
        651,605,653,575,545,572,594)
x2 <- c(3.39,3.30,2.81,3.03,3.44,3.07,3,3.43,
        3.36,3.13,3.12,2.74,2.76,2.88,3.96)
sum((x1-mean(x1))*(x2-mean(x2)))/sqrt((sum((x1-mean(x1))^2)*sum((x2-mean(x2))^2)))
n1 <- length(x1)
n2 <- length(x2)
p <- cor(x1,x2)
B <- 1000
tboot <- numeric(1000)
for (i in 1:B){
  xx1 <- sample(x1, n1, TRUE)
  xx2 <- sample(x2, n2, TRUE) 
  tboot[i] <- cor(xx1,xx2)
}

se2 <- sqrt(var(tboot))
hist(xx1,xx2)

plot(x1,x2)

p + 2*se2
p - 2*se2

z <- c(
8406 ,
2342 ,
8187 ,
 8459 ,
4795 ,
3516 ,
4796 ,
 10238 
)
mean(z)
y <- c(
 -1200,
 2601,
 -2705,
 1982,
 -1290,
351,
-638,
 -2719)

n1 <- length(z)
n2 <- length(y)
p <- mean(y)/mean(z)
B <- 1000
tboot <- numeric(1000)
for (i in 1:B){
  xx1 <- sample(z, n1, TRUE)
  xx2 <- sample(y, n2, TRUE) 
  tboot[i] <- (mean(xx2)/mean(xx1))
}

se3 <- sqrt(var(tboot))
hist(tboot)
p + 2*se3
p - 2*se3



t <-sample(y, n1-1, TRUE)
tbarra <- mean(t)
vjack <- ((n2-1)*sum((t-tbarra)^2)/n2)
mean(y)
var(y)
# pagina 123 ml de una bernulli

pML <- 12/20
p <- seq(0,1,.01)
plot(p, (p^12)*(1-p)^(20-12), type="l")
plot(p, exp(12*log(p)+(20-12)*log(1-p)), type="l")

exp(12*log(p)+(20-12)*log(1-p))
