############################################
############################################
############################################
#####   Tarea 4. Ciencia de datos
#####   Ejercicio 1. Kernel K-means
#####   Miranda Belmonte Hairo Ulise
#####   22 de Marzo del 2019
############################################
############################################
############################################

# librerías
library("tidyverse") 
library("kernlab") # Implementa Kernel K-means y Spectral cluster
library("ggplot2") # Visualización
library("magrittr") # Pippes
library("mlbench")  # Genera datos
library("cluster") # Implementa fuzzy k-means y K-medoides


# Datos de prueba

set.seed(1)
circle <- data.frame(radius = rep(c(0, 1), 100) + rnorm(100, sd = 0.05),
                     phi = runif(100, 0, 2 * pi),
                     group = rep(c("A", "B"), 100))

circle <- transform(circle,
                    x = radius * cos(phi),
                    y = radius * sin(phi),
                    z = rnorm(length(radius))) %>% select(group, x, y, z)

x <- circle$x %>% as.matrix
y <- circle$y %>% as.matrix 
X <- cbind(x,y)

# Datos no lineales para k=2
  # Figura uno
p0 <- X %>% as.data.frame %>% ggplot()+
  aes(x=X[,1], y=X[,2])+ geom_point() + labs(title="Datos de Prueba",
                                             x="x",
                                             y="y",
                                             subtitle = "(a)")

  # Figura dos
p1 <-mlbench.spirals(100,1)
p1plot <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point() + labs(title="Datos de Prueba",
                                       x="x",
                                       y="y",
                                       subtitle = "(b)")

grid.arrange(p0,p1plot,nrow=1)
#######################################
#######################################
##
##  Kernel K medias Función
## 
#######################################
#######################################
## input -> 
## X: matriz de observaciones
## k: clusters deseados
## e: número de iteraciones
## sigma: parámetro del kernel gaussiano
## output <- 
## 1) número de clusters
## 2) centroides
##
########################################
########################################


kkmedias<- function(X,k,e,sigma){
  # iniciammos parámetro de paro
  t <- 1
  # seleccionamos parámetro kernel gaussiano
  rbf <- rbfdot(sigma = sigma)
  # matriz kernel
  KernelMarrriz <- kernelMatrix(rbf, X)
  # inicializa asignación de clusters
  set.seed(6)
  cluster <- round(runif(length(X[,1]),1,2)) %>% as.matrix
  # función que calcula distancia
  distancia <- function(KernelMarrriz,X,inicia){
  i <- 1:k
  cosa <- 1-2*sapply(i, function(i) rowMeans(KernelMarrriz[,which(inicia==i)]))
  x <- matrix(0L,2,1)
  for(i in 1:2){
    x[i] <- sum(KernelMarrriz[which(inicia==i),which(inicia==i)])/(sum(inicia==i)^2) 
  } 
  i <- 1:2
  D <- sapply(i, function(i) cosa[,i]+x[i,1] )
  return(D)
  } # end distancia
  # iteraciones
  while(t < e){
    i <- 1:length(X[,1])
    j <- 1:k
    D <-   distancia(KernelMarrriz,X, cluster)
    i <-1:length(X[,1])
    cluster <-sapply(i, function(i) which.min(D[i,])) 
    i <-1:k
    A <- cbind(cluster, X) %>%  as.data.frame
    Medias <- matrix(0L,k,3)
    Medias <- sapply(i, function(i)   A %>% filter(cluster==i) %>% colMeans) 
    Medias[is.nan(Medias)] <- 0
    Xmean <- Medias  
    Xmean <- Xmean[(2:(dim(X)[2]+1)),] %>% as.matrix %>% t
    t <- t+1
  }
  
  return(list(centroides=Xmean,clusters=cluster))
} # end kkmedias

# Conjunto de datos "A"
e <- 100
k  <- 2
sigma <- 2
Resultado <- kkmedias(X,k,e,sigma)

pR1 <- X %>% as.data.frame %>% ggplot() + aes(x=X[,1],y=X[,2]) + geom_point(col=Resultado$clusters) +
  labs(title="Kernel K-means" ~ sigma == 2 ~ k == 2, x="X1", y="X2", caption = "Datos de Prueba")

Resultado$clusters %>% table 

# Conjunto de datos "B"
Resultado2 <- kkmedias(p1$x,k,e,sigma)
pR2 <- p1$x %>% as.data.frame %>% ggplot() + aes(x=V1,y=V2) + geom_point(col=Resultado2$clusters) +
  labs(title="Kernel K-means" ~ sigma == 2 ~ k == 2, x="X1", y="X2", caption = "Datos de Prueba")

Resultado2$clusters %>% table 

grid.arrange(pR1,pR2,nrow=1)


# Comparando respecto otras técnicas de cluster

###############
# IMAGEN "A"
###############

# cluster lineales hiperplanos
# Kmeans
set.seed(0)
kmeans1 <- kmeans(X,2)
pk1 <- X %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kmeans1$cluster) + labs(title="Kmeans k=2 ",
                                                             x="x",
                                                             y="y",
                                                             subtitle = "")
kmeans1$cluster %>% table

# kmediodes
set.seed(0)
kmedoides1 <- pam(X,2)
pk2 <- X %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kmedoides1$cluster) + labs(title="kmedoides k=2",
                                                                x="x",
                                                                y="y",
                                                                subtitle = "")
kmedoides1$clustering %>% table
# fuzzy-kmeans
set.seed(0)
kfuzzy1 <- fanny(X,2)
pk3 <- X %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kfuzzy1$cluster) + labs(title="fuzzy kmeans k=2",
                                                             x="x",
                                                             y="y")
kfuzzy1$clustering %>% table
# cluster lineales en espacio de carácteristicas                                                             subtitle = "(a)")
# spectral cluster
set.seed(0)
# sigma uno
sc1 <- specc(X, 2, kernel = "rbfdot", kpar = list(sigma=1))
pk4 <- X %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=sc1@.Data) + labs(title=bquote("Spectral cluster" ~ sigma == 1),
                                                       x="x", y="y")
sc1@.Data %>% table
# sigma 80
set.seed(2)
sc2 <- specc(X, 2, kernel = "rbfdot", kpar = list(sigma=80))
pk5 <- X %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=sc2@.Data) + labs(title=bquote("Spectral cluster" ~ sigma == 80),
                                                       x="x", y="y")


sc2@.Data %>% table

grid.arrange(pk1,pk2,pk3,pk4,pk5, ncol=3,nrow=2)

###############
# IMAGEN "B"
###############

# cluster lineales hiperplanos
# Kmeans
set.seed(0)
kmeans2 <- kmeans(p1$x,2)
pk1_2 <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kmeans2$cluster) + labs(title="Kmeans k=2 ",
                                                             x="x",
                                                             y="y",
                                                             subtitle = "")
kmeans2$cluster %>% table

# kmediodes
set.seed(0)
kmedoides2 <- pam(p1$x,2)
pk2_2 <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kmedoides2$cluster) + labs(title="kmedoides k=2",
                                                                x="x",
                                                                y="y",
                                                                subtitle = "")
kmedoides2$clustering %>% table
# fuzzy-kmeans
set.seed(0)
kfuzzy2 <- fanny(p1$x,2)
pk3_2 <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=kfuzzy2$cluster) + labs(title="fuzzy kmeans k=2",
                                                             x="x",
                                                             y="y")
kfuzzy2$clustering %>% table
# cluster lineales en espacio de carácteristicas                                                             subtitle = "(a)")
# spectral cluster
set.seed(0)
# sigma uno
sc2 <- specc(p1$x, 2, kernel = "rbfdot", kpar = list(sigma=1))
pk4_2 <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=sc2@.Data) + labs(title=bquote("Spectral cluster" ~ sigma == 1),
                                                       x="x", y="y")
sc2@.Data %>% table
# sigma 80
set.seed(2)
sc2_2 <- specc(p1$x, 2, kernel = "rbfdot", kpar = list(sigma=80))
pk5_2 <- p1$x %>% as.data.frame %>% ggplot()+
  aes(x=V1, y=V2)+ geom_point(colour=sc2_2@.Data) + labs(title=bquote("Spectral cluster" ~ sigma == 80),
                                                       x="x", y="y")


sc2_2@.Data %>% table

grid.arrange(pk1_2,pk2_2,pk3_2,pk4_2,pk5_2, ncol=3,nrow=2)


######################################################################
######################################################################
######################################################################
############################# FIN EJERCICIO 1 ########################
######################################################################
######################################################################
######################################################################