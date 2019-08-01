Se realizaran 1000  simulaciones, asumiendo los valores de las variables y
las muestras.

B<- 1000 # simulaciones
x1 <- 40 
n1 <- 50
x2 <- 30
n2 <- 50
Dado que bajo la posterior, tanto p1 como p2 son independientes, entonces
para obtener la distribución de la posterior realizamos obtenemos
B observaciones de la posterior, en este caso de cda p1 y p2
# muestra de las posterior
p1 <- rbeta(B, x1+1, n1-x1+1)
p2 <- rbeta(B, x2+1, n2-x2+1)
observamos como la media de p1 y p2 bajo la posteror se aproximan 
a la media estimada por verosiilitud
# media de maxima verosimilitud aproximada a postrior
postmean<-mean(p1)
meanML <-x1/n1     
postmean<-mean(p2)
meanML <-x2/n2     
Realizamos la transformacion de la variable y observamos la media posterior de 
la función, y obtenemos su densidad al reaizar el histograma
# realizando la transformacion de la variable
tau <- p1-p2
mean(tau)
hist(tau)
Se puede observar la densidad de la trasformación 
o más bien, la posterior de tau

# bajo una transformacion
#phi <- log((p1/(1-p1)))
#hist(phi)
#mean(phi)


#pregunta dos
a) Generar sample con media de cinco.
x <- rnorm(100,5,1)
mu <- 5
prior <- 1
b) la posteriror es una normal con media del estimador de máxima verosimilitud
Con base al sample generamos las observaciones de la posterior.

posterior <- dnorm(x,mean(x),1)
plot(density(posterior))

Como se puede observar , no se distribuye como una normal
. Sin embargo, necesitamos varias observaciones para que esto suceda.

# como se nota una posterior y su densidad no aproxima a la normal
# 1000 simulaciones de la posterior}
c)
posteriorB <-replicate(1000,rnorm(1,mean(x),1))
hist(posteriorB)
mean(posteriorB)
Podemos observar que la posterior al realizar varias simulaciones y tomar su media, 
se aproxima a la media del estadístico de la verosimilitud de unna normal.
# al realizar mil observaciones de una posterior y utilizar el histograma, ya parece normal
d)

Realizamos una transformación 
posteriro <- rnorm(B, mean(x),1)
hist(posteriro)
mean(posteriro)
construimos intervalos de confianza posteriori
IC <- c(quantile(posteriro,.05/2), quantile(posteriro,1-.05/2))
en base a conocer la distribución de la transformación, las B observaciones generadas
por la posterior, se evaluand en la transformacion.
theta <- exp(posteriro)
hist(theta)
mean(theta)
Se pued eobservar que la función se comporta como una exponencial.
Realizamos su intervalo posteriori
ICPosterior <- c(quantile(theta,.05/2), quantile(theta,1-.05/2))
z <- qnorm(1-.05/2)

ICNormal<- c(mean(theta)-z, mean(theta)+z)# revisar


##################fin
sd <- 0.0889104 # metodo delta o boorstrap
mean(tau) +qnorm(1-.1/2)*sd
mean(tau) -qnorm(1-.1/2)*sd