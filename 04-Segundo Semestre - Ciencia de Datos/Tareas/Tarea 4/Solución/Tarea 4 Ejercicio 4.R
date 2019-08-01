############################################
############################################
############################################
#####   Tarea 4. Ciencia de datos
#####   Ejercicio 4. AnÃ¡lisis sensibilidad
#####   Miranda Belmonte Hairo Ulise
#####   22 de Marzo del 2019
############################################
############################################
############################################

#################
#################
## LibrerÃ???as
#################
#################
library("magrittr")
library("ggplot2")
library("kernlab")
library("tm")
library("ggrepel") # geom_text_repel

################################################################
################################################################
###################### EJERCICIO 4 INCISO A ####################
################################################################
################################################################

###########
###########
##
## PARTE 1  
##
###########
###########

# Introduce ruta del archivo
getwd()
setwd("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/")
# extrae archivos de la etiquetas
bolsa <- read.csv("train_stock.csv",stringsAsFactors = F)
# genera lista
Y <- list()

# introduce la muestra para este ejercicio
  # en este caso se seleccionan las primeras 20 carÃ¡cteres
for (i in 1:20){
  Y[i] <- list(bolsa$description_x[i])
}
for (i in 21:40){
  Y[i] <- list(bolsa$description_y[i-20]) # toma losprimero 20
}

######################################
######################################
##
## FunciÃ³n limpia
##  input <- corpus de texto
##  retira: espacios en blanco
##          nÃºmeros
##          mayusculas a mÃ???nusculas
##          stop words en ingles
##          steams
##
######################################
######################################
limpiar <- function(corp){
  corp <- tm_map(corp,stripWhitespace) # espacios 
  corp <- tm_map(corp,removeNumbers) # nÃºmeros
  corp <- tm_map(corp,content_transformer(tolower)) # mayusculas
  corp <- tm_map(corp,removePunctuation) # puntuaciones
  corp <- tm_map(corp,removeWords,stopwords("english")) # stopwords
  corp <- tm_map(corp,stemDocument)  # stem
  return(corp)
} # end limpiar

# Pre- Proceso
Y2 <- VectorSource(Y)
Y2 <- VCorpus(Y2)
Y2 <- limpiar(Y2)

# Regresando a lista
Y3 <- list()
for (i in 1:length(Y2)){
  Y3[i] <- (Y2[[i]]$content)
}


# Tipos de Kernel

#spectrum: the kernel considers only matching substring of exactly length n (also know as string kernel). Each such matching substring is given a constant weight. The length parameter in this kernel has to be length > 1.
sk1 <- stringdot(type="spectrum", length=3, normalized=FALSE)

#boundrange: this kernel (also known as boundrange) considers only matching substrings of length less than or equal to a given number N. This type of string kernel requires a length parameter length > 1
sk2 <- stringdot(type="boundrange", length=3, normalized=FALSE)

#constant: The kernel considers all matching substrings and assigns constant weight (e.g. 1) to each of them. This constant kernel does not require any additional parameter.
sk3 <- stringdot(type="constant", length=3, normalized=FALSE)

#exponential: Exponential Decay kernel where the substring weight decays as the matching substring gets longer. The kernel requires a decay factor Î» > 1
sk4 <- stringdot(type="exponential", length=3, normalized=FALSE, lambda = 1.1)


# Indices, textos
index1 <- Y3[1:20] %>% as.matrix
index2 <- Y3[21:40] %>% as.matrix
index <- rbind(index1,index2)

index1_1 <- rep(1,20) %>% as.matrix
index2_2 <- rep(2,20) %>% as.matrix
index_2 <- rbind(index1_1,index2_2)
index_2 <- index_2 %>% as.vector

# Spectrum string kernel
kpc1 <- kpca(Y3, kernel =sk1)
pc <- kpc1@pcv
kpca.df1 <- data.frame("Label" = index, "color" = as.factor(index_2),
                      "PC1" = pc[,1], "PC2" = pc[,2])
p1 <- ggplot(kpca.df1, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="spectrum kernel, string="~3,
                                           y="PC2",
                                           x="PC1")
X11()
p1

# boundrange string kernel
kpc2 <- kpca(Y3, kernel =sk2)
pc2 <- kpc2@pcv
kpca.df2 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc2[,1], "PC2" = pc2[,2])
p2 <- ggplot(kpca.df2, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="boundrange kernel, string="~3,
                                           y="PC2",
                                           x="PC1")
X11()
p2

# constant string kernel
kpc3 <- kpca(Y3, kernel =sk3)
pc3 <- kpc3@pcv
kpca.df3 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc3[,1], "PC2" = pc3[,2])
p3 <- ggplot(kpca.df3, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="constant kernel, string="~3,
                                           y="PC2",
                                           x="PC1")


X11()
p3

# exponential string kernel
kpc4 <- kpca(Y3, kernel =sk4)
pc4 <- kpc4@pcv
kpca.df4 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc4[,1], "PC2" = pc4[,2])
p4 <- ggplot(kpca.df4, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="exponential kernel, string="~3,
                                           y="PC2",
                                           x="PC1")


X11()
p4
####################################################################
####################################################################
####################################################################
####################################################################

###########
###########
##
## PARTE 2  
##
###########
###########

# En esta parte en vez de tomar las descripciÃ³n, se toman los tickets
# con el fin de una mejorvisualizaciÃ³n solamente en el texto



rm(list=ls())
# introduzca la ruta de su archivo
getwd()
setwd("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/")
# extrae archivos de la etiquetas
bolsa <- read.csv("train_stock.csv",stringsAsFactors = F)
# genera lista vacia
Y <- list()
# tome una muestra por cuestiÃ³n de visualizaciÃ³n
# en este caso se toman las mismas que el caso de arriba
for (i in 1:20){
  Y[i] <- list(bolsa$ticker_x[i])
}
for (i in 21:40){
  Y[i] <- list(bolsa$ticker_y[i-20])
}

# Tipos de string kernel
#spectrum the kernel considers only matching substring of exactly length n (also know as string kernel). Each such matching substring is given a constant weight. The length parameter in this kernel has to be length > 1.
sk1 <- stringdot(type="spectrum", length=1, normalized=FALSE)

#boundrange this kernel (also known as boundrange) considers only matching substrings of length less than or equal to a given number N. This type of string kernel requires a length parameter length > 1
sk2 <- stringdot(type="boundrange", length=1, normalized=FALSE)

#constant The kernel considers all matching substrings and assigns constant weight (e.g. 1) to each of them. This constant kernel does not require any additional parameter.
sk3 <- stringdot(type="constant", length=1, normalized=FALSE)

#exponential Exponential Decay kernel where the substring weight decays as the matching substring gets longer. The kernel requires a decay factor Î» > 1
sk4 <- stringdot(type="exponential", length=1, normalized=FALSE, lambda = 1.1)


# Indices de textos y color
X <- unlist(Y)
index1 <- X[1:20] %>% as.matrix
index2 <- X[21:40] %>% as.matrix
index <- rbind(index1,index2)

index1_1 <- rep(1,20) %>% as.matrix
index2_2 <- rep(2,20) %>% as.matrix
index_2 <- rbind(index1_1,index2_2)
index_2 <- index_2 %>% as.vector

# spectrum string kernel
kpc1 <- kpca(Y, kernel =sk1)
pc <- kpc1@pcv
kpca.df1 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc[,1], "PC2" = pc[,2])
p1_2 <- ggplot(kpca.df1, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="spectrum kernel, string="~1,
                                           y="PC2",
                                           x="PC1")

# boundrange string kernel
kpc2 <- kpca(Y, kernel =sk2)
pc2 <- kpc2@pcv
kpca.df2 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc2[,1], "PC2" = pc2[,2])
p2_2 <- ggplot(kpca.df2, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="boundrange kernel, string="~1,
                                           y="PC2",
                                           x="PC1")



# constant string kernel
kpc3 <- kpca(Y, kernel =sk3)
pc3 <- kpc3@pcv
kpca.df3 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc3[,1], "PC2" = pc3[,2])
p3_2 <- ggplot(kpca.df3, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="constant kernel, string="~1,
                                           y="PC2",
                                           x="PC1")



# exponential string kernel
kpc4 <- kpca(Y, kernel =sk4)
pc4 <- kpc4@pcv
kpca.df4 <- data.frame("Label" = index, "color" = as.factor(index_2),
                       "PC1" = pc4[,1], "PC2" = pc4[,2])
p4_2 <- ggplot(kpca.df4, aes(label = Label, x = PC1, y = PC2, color = color)) + 
  geom_text_repel(segment.alpha	=1) + labs(title="String Kernel",
                                           subtitle="exponential kernel, string="~1,
                                           y="PC2",
                                           x="PC1")


# visualizar
X11()
p1_2
p2_2  
p3_2
p4_2

################################################################
################################################################
##################### FIN EJERCICO 4 INCISO A ##################
################################################################
################################################################
################################################################


################################################################
################################################################
###################### EJERCICIO 4 INCISO B ####################
################################################################
################################################################


# Introduce ruta del archivo
getwd()
setwd("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/")
# extrae archivos de la etiquetas
bolsa <- read.csv("train_stock.csv",stringsAsFactors = F)
# genera lista
x <- bolsa$description_x
y <- bolsa$description_y


######################################
######################################
##
## FunciÃ³n limpia
##  input <- corpus de texto
##  retira: espacios en blanco
##          nÃºmeros
##          mayusculas a mÃ???nusculas
##          stop words en ingles
##          steams
##
######################################
######################################
limpiar <- function(corp){
  corp <- tm_map(corp,stripWhitespace) # espacios 
  corp <- tm_map(corp,removeNumbers) # nÃºmeros
  corp <- tm_map(corp,content_transformer(tolower)) # mayusculas
  corp <- tm_map(corp,removePunctuation) # puntuaciones
  corp <- tm_map(corp,removeWords,stopwords("english")) # stopwords
  corp <- tm_map(corp,stemDocument)  # stem
  return(corp)
} # end limpiar

# Pre- Proceso
x <- VectorSource(x)
x <- VCorpus(x)
x <- limpiar(x)

y <- VectorSource(y)
y <- VCorpus(y)
y <- limpiar(y)

# tamaño de la muestra 10 observaciones
  # con motivos de mejorar la visualización
x2 <- list()

# puede cambiar el tamaño del subconjunto
# en este caso hay diez
for (i in 1:10){
  x2[i] <- list(x[[i]]$content)
}

y2 <- list() 
for (i in 1:10){
  y2[i] <- list(y[[i]]$content)
}


# Tipos de string kernel
#spectrum the kernel considers only matching substring of exactly length n (also know as string kernel). Each such matching substring is given a constant weight. The length parameter in this kernel has to be length > 1.
sk1 <- stringdot(type="spectrum", length=8, normalized=FALSE)

# Matriz de gram kernel string
XX <- kernelMatrix(sk1,y2)
XX %>% dim
Kpc <- kpca(x2, kernel=sk1)
pc <- Kpc@rotated
# proyecciones
resultado <- t(Kpc@pcv[,1:2])%*%XX
resultado <- t(resultado)

# Indices de textos y color
X <- unlist(y2)
index1 <- X[1:10] %>% as.matrix
X2 <- unlist(x2)
index2 <- X2[1:10] %>% as.matrix
index2 <- index2 %>% as.vector

# visualizción
kpca.df1 <- data.frame("Label" = index1, 
                       "PC1" = pc[,1], "PC2" = pc[,2])
p1 <- ggplot(kpca.df1, aes(label = Label, x = PC1, y = PC2)) + 
  geom_text_repel(segment.alpha	=1, col="blue") + labs(title="String Kernel",
                                           subtitle="spectrum kernel, string="~8,
                                           y="PC2",
                                           x="PC1") +
  geom_text(aes(resultado[1,1],resultado[2,1], label=index2[1]),col="red")+
  geom_text(aes(resultado[2,1],resultado[2,2], label=index2[2]),col="red")+
  geom_text(aes(resultado[3,1],resultado[3,2], label=index2[3]),col="red")+
  geom_text(aes(resultado[4,1],resultado[4,2], label=index2[4]),col="red")


x11()
# Exponential kernel
sk2 <- stringdot(type="exponential", length=8, normalized=FALSE)

# Matriz de gram kernel string
XX2 <- kernelMatrix(sk2,y2)
Kpc2 <- kpca(x2, kernel=sk2)
pc2 <- Kpc2@rotated
# proyecciones
resultado2 <- t(Kpc2@pcv[,1:2])%*%XX2
resultado2 <- t(resultado2)

# Indices de textos y color
X_2 <- unlist(y2)
index1_2 <- X_2[1:10] %>% as.matrix
X2_2 <- unlist(x2)
index2_2 <- X2_2[1:10] %>% as.matrix
index2_2 <- index2_2 %>% as.vector


# visualización
kpca.df2 <- data.frame("Label" = index1_2, 
                       "PC1" = pc2[,1], "PC2" = pc2[,2])
p2 <- ggplot(kpca.df2, aes(label = Label, x = PC1, y = PC2)) + 
  geom_text_repel(segment.alpha	=1, col="blue") + labs(title="String Kernel",
                                                       subtitle="exponential kernel, string="~8,
                                                       y="PC2",
                                                       x="PC1") +
  geom_text(aes(resultado2[1,1],resultado2[2,1], label=index2[1]),col="red")+
  geom_text(aes(resultado2[2,1],resultado2[2,2], label=index2[2]),col="red")+
  geom_text(aes(resultado2[3,1],resultado2[3,2], label=index2[3]),col="red")+
  geom_text(aes(resultado2[4,1],resultado2[4,2], label=index2[4]),col="red")

x11()
# constant kernel
sk3 <- stringdot(type="constant", length=8, normalized=FALSE)

# Matriz de gram kernel string
XX3 <- kernelMatrix(sk3,y2)
Kpc3 <- kpca(x2, kernel=sk3)
pc3 <- Kpc3@rotated

# proyecciones
resultado3 <- t(Kpc3@pcv[,1:2])%*%XX3
resultado3 <- t(resultado3)

# Indices de textos y color
X_3 <- unlist(y2)
index1_3 <- X_3[1:10] %>% as.matrix
X2_3 <- unlist(x2)
index2_3 <- X2_3[1:10] %>% as.matrix
index2_3 <- index2_3 %>% as.vector


# visualización
kpca.df3 <- data.frame("Label" = index1_3, 
                       "PC1" = pc3[,1], "PC2" = pc3[,2])
p3 <- ggplot(kpca.df3, aes(label = Label, x = PC1, y = PC2)) + 
  geom_text_repel(segment.alpha	=1, col="blue") + labs(title="String Kernel",
                                                       subtitle="constant kernel, string="~8,
                                                       y="PC2",
                                                       x="PC1") +
  geom_text(aes(resultado3[1,1],resultado3[2,1], label=index2[1]),col="red")+
  geom_text(aes(resultado3[2,1],resultado3[2,2], label=index2[2]),col="red")+
  geom_text(aes(resultado3[3,1],resultado3[3,2], label=index2[3]),col="red")+
  geom_text(aes(resultado3[4,1],resultado3[4,2], label=index2[4]),col="red")

x11()

# boundrange kernel
sk4 <- stringdot(type="boundrange", length=8, normalized=FALSE)

# Matriz de gram kernel string
XX4 <- kernelMatrix(sk4,y2)
Kpc4 <- kpca(x2, kernel=sk4)
pc4 <- Kpc4@rotated

# proyecciones
resultado4 <- t(Kpc4@pcv[,1:2])%*%XX4
resultado4 <- t(resultado4)

# Indices de textos y color
X_4 <- unlist(y2)
index1_4 <- X_4[1:10] %>% as.matrix
X2_4 <- unlist(x2)
index2_4 <- X2_4[1:10] %>% as.matrix
index2_4 <- index2_4 %>% as.vector


# visualización
kpca.df4 <- data.frame("Label" = index1_4, 
                       "PC1" = pc4[,1], "PC2" = pc4[,2])
p4 <- ggplot(kpca.df4, aes(label = Label, x = PC1, y = PC2)) + 
  geom_text_repel(segment.alpha	=1, col="blue") + labs(title="String Kernel",
                                                       subtitle="boundrange kernel, string="~8,
                                                       y="PC2",
                                                       x="PC1") +
  geom_text(aes(resultado4[1,1],resultado4[2,1], label=index2[1]),col="red")+
  geom_text(aes(resultado4[2,1],resultado4[2,2], label=index2[2]),col="red")+
  geom_text(aes(resultado4[3,1],resultado4[3,2], label=index2[3]),col="red")+
  geom_text(aes(resultado4[4,1],resultado4[4,2], label=index2[4]),col="red")

x11()

################################################################
################################################################
##################### FIN EJERCICO 4 INCISO B ##################
################################################################
################################################################
################################################################
