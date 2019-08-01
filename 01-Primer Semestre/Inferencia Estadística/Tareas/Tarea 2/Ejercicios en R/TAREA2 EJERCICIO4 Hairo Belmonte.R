#  title: "Tarea 2 Inferencia Estadística"
#author: "Hairo Ulises Miranda Belmonte"
#date: "04 de Septiembre de 2018"

# EJERCICIO 4a

# Función que simula lanzamientos de monedas
# con paramétros "p" y "n".
# 0 Éxito (Aguila)
# 1 Fracaso (Sol)

lanzamiento <- function(p,n){
resultado <- numeric(n)   # Vector para almacenar resultados
for (i in c(1:n)) {
  contador <- 1 # Almacena el número d intentos
  repeat {  # Se realiza el sample. Cuando sea 0 se detiene
    muestra <- sample(c(0, 1), 1, prob = c(p, 1-p)) # asignamos probabilidades
                                             # debo a que es una moneda sesgada
    if (muestra == 1) {
      contador <- contador + 1 # Sumamos uno, lo cual indica otro fracaso
    } else {
      break # En caso de que no sea 1, rompemos el repeat
    }
  }
  resultado [i] <- contador # vector tamaño cero
}
return(resultado) # regresa el vector con el número de
                  # fracasos hasta encontrar éxito
}

# EJERCICIO 4b
# Función que regresa el número de fracasos en lanzamientos de monedas
# hasta encontrar la primera águila. 

lanz1 <- lanzamiento(.5, 10000)
lanz2 <- lanzamiento(.1, 10000)
lanz3 <- lanzamiento(.01, 10000)

# Pasando a formato data frame
geom1 <- data.frame(x = lanz1, group="A")
geom2 <- data.frame(x = lanz2, group="B")
geom3 <- data.frame(x = lanz3, group="C")

graf <- rbind(geom1, geom2, geom3)

# Histogramas de las tres simulaciones
ggplot(graf, aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..count..), breaks=seq(0,100,5), alpha=.5, 
                 position="identity", lwd=0.2) +
  ggtitle("Histograma Normalizado") 
 
 # PMF

# Para el lanzamiento con probabilidad de .5

probabilidad.Lanz1 <- table(lanz1)
for (i in seq(1, length(probabilidad.Lanz1), 1)){ # probabilidad
  probabilidad.Lanz1[i] = probabilidad.Lanz1[i]/10000
  
}
prob.Lanz1 <- c()
for (i in seq(1, length(probabilidad.Lanz1), 1)){ # sacando del formato table
  prob.Lanz1[i]=probabilidad.Lanz1[[i]]
  
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz1,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.5)")
lines(dgeom(0:length(lanz1),.5),type="l", col="red", lwd=3 )

# Para el lanzamiento con probabilidad de .1

probabilidad.Lanz2 <- table(lanz2)
for (i in seq(1, length(probabilidad.Lanz2), 1)){ # probabilidad
  probabilidad.Lanz2[i] = probabilidad.Lanz2[i]/10000
  
}
prob.Lanz2 <- c()
for (i in seq(1, length(probabilidad.Lanz2), 1)){ # sacando del formato table
  prob.Lanz2[i]=probabilidad.Lanz2[[i]]
  
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz2,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.1)")
lines(dgeom(0:length(lanz2), .1),type="l", col="red", lwd=3 )


# Para el lanzamiento con probabilidad de .01

probabilidad.Lanz3 <- table(lanz3)
for (i in seq(1, length(probabilidad.Lanz3), 1)){ # probabilidad
  probabilidad.Lanz3[i] = probabilidad.Lanz3[i]/10000
  
}
prob.Lanz3 <- c()
for (i in seq(1, length(probabilidad.Lanz3), 1)){ # sacando del formato table
  prob.Lanz3[i]=probabilidad.Lanz3[[i]]
  
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz3,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.01)")
lines(dgeom(0:length(lanz3), .01), type="l", col="red", lwd=3 )



# EJERCICIO 4c

# Ejercicio anterior, pero con 10^6 simulaciones
lanz1 <- rep(0,1000000)
lanz2 <- rep(0,1000000)
lanz3 <- rep(0,1000000)

lanz1 <- lanzamiento(.5, 1000000)
lanz2 <- lanzamiento(.1, 1000000)
lanz3 <- lanzamiento(.01, 1000000)

# A formato data frame
geom1 <- data.frame(x = lanz1, group="A")
geom2 <- data.frame(x = lanz2, group="B")
geom3 <- data.frame(x = lanz3, group="C")

graf <- rbind(geom1, geom2, geom3)

#Histogramas de las tres simulaciones
ggplot(graf, aes(x, fill=group, colour=group)) +
  geom_histogram(aes(y=..count..), breaks=seq(0,100,5), alpha=.5, 
                 position="identity", lwd=0.2) +
  ggtitle("Histograma Normalizado") 

# PMF 

# Para el lanzamiento con probabilidad de .5

probabilidad.Lanz1 <- table(lanz1) # Vector en formato table

for (i in seq(1, length(probabilidad.Lanz1), 1)){ # probabilidades
  probabilidad.Lanz1[i] = probabilidad.Lanz1[i]/1000000
}

prob.Lanz1 <- c() # Sacndo de formato table

for (i in seq(1, length(probabilidad.Lanz1), 1)){
  prob.Lanz1[i]=probabilidad.Lanz1[[i]]
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz1,freq = FALSE,xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.5)",
     col="blue", xlim=c(1,10))
lines(dgeom(0:length(lanz1),.5),type="l", col="red", lwd=3 )

# Para el lanzamiento con probabilidad de .1

probabilidad.Lanz2 <- table(lanz2)
for (i in seq(1, length(probabilidad.Lanz2), 1)){ # probabilidad
  probabilidad.Lanz2[i] = probabilidad.Lanz2[i]/1000000
  }

prob.Lanz2 <- c()
for (i in seq(1, length(probabilidad.Lanz2), 1)){ # sacando del formato table
  prob.Lanz2[i]=probabilidad.Lanz2[[i]]
  
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz2,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.1)",  
     xlim=c(1,80))
lines(dgeom(0:length(lanz2),.1),type="l", col="red", lwd=3 )


# Para el lanzamiento con probabilidad de .01

probabilidad.Lanz3 <- table(lanz3)
for (i in seq(1, length(probabilidad.Lanz3), 1)){ # probabilidad
  probabilidad.Lanz3[i] = probabilidad.Lanz3[i]/1000000
  
}
prob.Lanz3 <- c()
for (i in seq(1, length(probabilidad.Lanz3), 1)){ # sacando del formato table
  prob.Lanz3[i]=probabilidad.Lanz3[[i]]
  
}

# Histograma del sample traslapando la pmf de la geometrica

par(bg = "gray")
hist(lanz3,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a un éxito", main = "Histograma y PMF (Pr=0.01)",
     xlim=c(0,800))
lines(dgeom(0:length(lanz3), .01),type="l", col="red", lwd=3 )


# Promedio y desviación estandar del lanzamiento con probabilidad .5 (sample y original)
mean(prob.Lanz1)
mean(dgeom(0:length(prob.Lanz1),.5))

sd(prob.Lanz1)
sd(dgeom(0:length(prob.Lanz1),.5))

# Promedio y desviación estandar del lanzamiento con probabilidad .1
mean(prob.Lanz2)
mean(dgeom(0:length(prob.Lanz2),.1))

sd(prob.Lanz2)
sd(dgeom(0:length(prob.Lanz2),.1))

# Promedio y desviación estandar del lanzamiento con probabilidad .01
mean(prob.Lanz3)
mean(dgeom(0:length(prob.Lanz3),.01))

sd(prob.Lanz3)
sd(dgeom(0:length(prob.Lanz3),.01))
