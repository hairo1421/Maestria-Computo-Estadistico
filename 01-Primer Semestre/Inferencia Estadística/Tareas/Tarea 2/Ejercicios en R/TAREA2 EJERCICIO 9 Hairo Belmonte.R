---
  #  title: "Tarea 1 Inferencia Estadística"
  #author: "Hairo Ulises Miranda Belmonte"
  #date: "009 de agosto de 2018"
  
# Función que genera proceso de poisson
proceso.Poisson <- function(n, lambda, intervalo){
  
  # número de eventos ocurridos
  eventos <- qpois(1 - 1e-8, lambda = lambda * intervalo) #

  # genera las Ti del proceso
  t <- matrix(rexp(eventos * n, rate = lambda), ncol = n,
              dimnames = list(paste("S", 1:eventos, sep = ""), paste("samp", 1:n)))
  # Acomulado de exp
  S <- apply(t, 2, cumsum)
  # N(0) = 0
  S <- rbind("T0" = rep(0, n), S)
  # Plot simulaciónes 
  matplot(x = S, y = 0:eventos, type = "s", col = "darkgray",
          xlim = c(0, intervalo),
          main = "Homogeneous Poisson Process paths", xlab = "t", ylab = "N(t)")

    return(S)

}

# Simula 3 procesos con lambda 2, e intervalo [0, 10]
sim1 <-proceso.Poisson(3, 2, 10)


# Simula 10^4 procesos con lambda 0.5, e intervalo [0, 1]

sim2 <- proceso.Poisson(10000, 0.5, 1)

