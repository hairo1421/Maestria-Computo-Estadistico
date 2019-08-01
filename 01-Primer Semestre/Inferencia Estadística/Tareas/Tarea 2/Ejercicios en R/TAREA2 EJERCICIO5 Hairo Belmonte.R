---
  #  title: "Tarea 2 Inferencia Estadística"
  #author: "Hairo Ulises Miranda Belmonte"
  #date: "04 de Septiembre de 2018"

# EJERCICIO 5

# Función que simula lanzamientos de monedas hasta encontrar r éxitos
# con paramétros "p", "n" y "r".
# 0 Éxito (Aguila)
# 1 Fracaso (Sol)
  

 

R.lanzamientos <- function(p, n, r){
  
  resultado <- numeric(n) # Vector para almacenar resultados
  
    for (i in c(1:n)) { # cambiae a seq
      
      contador <- 1 # Almacena el número de intentos
      exito <- 0 # Almacena el número de éxitos
      
    while(exito < r){ # Detener hasta encontrar los éxitos deseados
      
      repeat {  # Se realiza el sample. Cuando sea 0 se detiene
        muestra <- sample(c(0, 1), 1, prob = c(p, 1-p)) # asignamos probabilidades
          # debo a que es una moneda sesgada
          if (muestra == 1) {
            contador <- contador + 1 # Sumamos uno, lo cual indica otro fracaso
          } else {
        break # En caso de que no sea 1, rompemos el repeat
          } # end else
      } # end repeat
      
      exito <- exito+1
    } # end while
    
    resultado [i] <-  contador # vector tamaño cero
  } # end for
  
  return(resultado)
    
} # end function 
  
  


# CASO 1: N = 10^6, p = 0.2, r = 7.


# Toma Frecuencia de lanzamientos simulados
Rlanz1 <- rep(0, 1000000)
Rlanz1 <- R.lanzamientos(.2,1000000,7)
probabilidadLanz1 <- table(Rlanz1)

probLanz1 <- numeric(length(probabilidadLanz1))

# Extrae probabilidades de formato table
for (i in seq(1, length(probabilidadLanz1), 1)){
  probLanz1[i]=probabilidadLanz1[[i]]/1000000
}


mean(dnbinom(1:length(probLanz1),7,.2))
mean(probLanz1)
 # canbiar dimención de x y

par(bg = "gray")
hist(Rlanz1,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a r éxitos", main = "Histograma y PMF (Pr=0.2) y (r=7)", ylim=c(0,.045))
lines(dnbinom(0:60, 7, .2), type="l", col="red", lwd=3 )


# Caso 2: N = 10^6, p = 0.2, r = 2.

# Toma Frecuencia de lanzamientos simulados
Rlanz2 <- rep(0, 1000000)
Rlanz2 <- R.lanzamientos(.2,1000000,2)
probabilidadLanz2 <- table(Rlanz2)

probLanz2 <- numeric(length(probabilidadLanz2))

# Extrae probabilidades de formato table
for (i in seq(1, length(probabilidadLanz2), 1)){
  probLanz2[i]=probabilidadLanz2[[i]]/1000000
}


mean(dnbinom(1:length(probLanz2),2,.2))
mean(probLanz2)
# canbiar dimención de x y

par(bg = "gray")
hist(Rlanz2,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a r éxitos", main = "Histograma y PMF (Pr=0.2) y (r=2)", ylim=c(0,.1))
lines(dnbinom(0:60, 2, .2), type="l", col="red", lwd=3 )

# Caso 3: N = 10^6, p = 0.1, r = 2.

# Toma Frecuencia de lanzamientos simulados
Rlanz3 <- rep(0, 1000000)
Rlanz3 <- R.lanzamientos(.1,1000000,2)
probabilidadLanz3 <- table(Rlanz3)

probLanz3 <- numeric(length(probabilidadLanz3))

# Extrae probabilidades de formato table
for (i in seq(1, length(probabilidadLanz3), 1)){
  probLanz3[i]=probabilidadLanz3[[i]]/1000000
}


mean(dnbinom(1:length(probLanz3),2,.1))
mean(probLanz3)
# canbiar dimención de x y

par(bg = "gray")
hist(Rlanz3,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a r éxitos", main = "Histograma y PMF (Pr=0.1) y (r=2)", 
     ylim=c(0,.045), xlim=c(1,70))
lines(dnbinom(0:60, 2, .1), type="l", col="red", lwd=3 )

# Caso 4: N = 10^6, p = 0.1, r = 7.

# Toma Frecuencia de lanzamientos simulados
Rlanz4 <- rep(0, 1000000)
Rlanz4 <- R.lanzamientos(.1,1000000,7)
probabilidadLanz4 <- table(Rlanz4)

probLanz4 <- numeric(length(probabilidadLanz4))

# Extrae probabilidades de formato table
for (i in seq(1, length(probabilidadLanz4), 1)){
  probLanz4[i]=probabilidadLanz4[[i]]/1000000
}

mean(dnbinom(1:length(probLanz4),7,.1))
mean(probLanz4)
# canbiar dimención de x y

par(bg = "gray")
hist(Rlanz4,freq = FALSE, col="blue", 
     xlab="Lanzamientos previos a r éxitos", main = "Histograma y PMF (Pr=0.1) y (r=7)", ylim=c(0,.02))
lines(dnbinom(0:150, 7, .1), type="l", col="red", lwd=3 )
