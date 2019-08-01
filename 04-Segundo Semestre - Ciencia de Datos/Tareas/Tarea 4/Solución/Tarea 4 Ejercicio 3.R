############################################
############################################
############################################
#####   Tarea 4. Ciencia de datos
#####   Ejercicio 3. Análisis sensibilidad
#####   Miranda Belmonte Hairo Ulise
#####   22 de Marzo del 2019
############################################
############################################
############################################

#################
#################
## Librerías
#################
#################

library("tm")
library("magrittr")
library("gridExtra")
library("RColorBrewer")
library("wordcloud")
library("ggplot2")
library("irlba")
# ingrese la dirección donde se encuentran sus archivos 
getwd()
setwd("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/")
# extrae archivos de la etiquetas
etiquetas <- read.csv("movie_reviews.csv")
etiquetas$sentimiento %>% levels

# Ruta de la dirección de los archivos
rut.data <- "C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/movie_reviews/movie_reviews/txtfiles/Criticas"
# corpus de textos
corp <- Corpus(DirSource(rut.data,recursive=TRUE),readerControl=list(language="en_US"))

# En caso de querer ver la representación de un texto
# retire el signo del número gato "#"

#inspect(corp[[500]])
#inspect(corp[[501]])
#corp[[1]]$content

## preproceso del texto

######################################
######################################
##
## Función limpia
##  input <- corpus de texto
##  retira: espacios en blanco
##          números
##          mayusculas a mínusculas
##          stop words en ingles
##          steams
##
######################################
######################################

limpiar <- function(corp){
corp <- tm_map(corp,stripWhitespace) # espacios 
corp <- tm_map(corp,removeNumbers) # números
corp <- tm_map(corp,content_transformer(tolower)) # mayusculas
corp <- tm_map(corp,removePunctuation) # puntuaciones
corp <- tm_map(corp,removeWords,stopwords("english")) # stopwords
corp <- tm_map(corp,stemDocument)  # stem
return(corp)
} # end limpiar

# limpia corpus de documentos
corp <- limpiar(corp)

# Retire "#" para observar un documento procesado
#inspect(corp[[500]])

tdm <- TermDocumentMatrix(corp) # archivos a matriz
# matriz tm tiene 99% de "sparsity"

# remueve .1 de "sparsity"
tdm <- removeSparseTerms(tdm,.90) # eliminamos algo de esparsidad
# matriz tm tiene 78% de "sparsity"


term.freq <- rowSums(as.matrix(tdm)) # términos más frecuentes
sort.freq <- sort.int(term.freq, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp <- names(sort.freq$x[1:50]) # nombre 50 términos más frecuentes


######################################
######################################
##
## Función count.terms
##  input <- matriz de terminos
##           nombre de terminos
##  output: Tabla de frecuencia
## 
##
######################################
######################################

count.terms <- function(tdms,terms){
  freq.table <- NULL
  for(i in 1:length(terms)){
    aa <- tm_term_score(tdms,terms[i])
    freq.table <- cbind(freq.table,aa)
  }
  colnames(freq.table) <- terms
  return(freq.table)
} # end count.term

# tabla térmminos 50 más frecuentes
X <- count.terms(tdm,nterms.corp) 
# ordenar conforme etiquetas para visualización
X <- X[etiquetas$archivo.texto,]

# PCA 50 términos más frecuentes
PCA1 <- princomp(X)
# etiquetas
criticas <- etiquetas$sentimiento
# visualización
p1 <- PCA1$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA1$scores[,1],y=PCA1$scores[,2], col=criticas)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      caption="N: negativas; P: positivas")



#################
# Escalar datos
#################
X_scale <- X %>% scale
PCA2 <- princomp(X_scale)
criticas <- etiquetas$sentimiento
p3 <- PCA2$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA2$scores[,1],y=PCA2$scores[,2], col=criticas)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Observaciones escaladas",
                      caption="N: negativas; P: positivas; matrix term scale")

##################
# matriz dispersa
##################
# La siguiente función trabaja con matrices dispersas
# no se reporta el resultado en el documento ya que los resultados
# no generan cosas distintas a lo anterior; sin embargo
# se le deja al usuario las funciones.

# SIN ESCALAR OBSERVACIONES
#PCAsparse <- prcomp_irlba(X, n = 2)
#p2 <-  PCAsparse$x %>% as.data.frame %>% ggplot() + 
#  aes(x=PCAsparse$x[,1],y=PCAsparse$x[,2], col=criticas)+
#  geom_point() + labs(title="Críticas Películas (Sparse matrix)",
#                      x="PC 1",
#                      y="PC 2",
#                      caption="N: negativas; P: positivas")
# ESCALANDO OBSERVACIONES
#PCAsparse <- prcomp_irlba(X_scale, n = 10)
#p4 <- PCAsparse$x %>% as.data.frame %>% ggplot() + 
#  aes(x=PCAsparse$x[,1],y=PCAsparse$x[,2], col=criticas)+
#  geom_point() + labs(title="Críticas Películas",
#                      x="PC 1",
#                      y="PC 2",
#                      caption="N: negativas; P: positivas,matrix term scale")


grid.arrange(p1,p3, nrow=1)


##############################################################################
##############################################################################
## Retirando observaciones más frecuentes que sesgan analisis
## aquellas palabras en común entre textos positivos y negativos
##############################################################################
##############################################################################


# Estos son los nombres de los términos más frecuentes
tdm_matrix <- as.matrix(tdm)
# Calculate the rowSums: term_frequency
term_frequency <- rowSums(tdm_matrix)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = T)
# Plot a barchart of the 10 most common words
barplot(term_frequency[1:50], col = "tan", las = 2)

# Limpiar base
rm_add_words <- c("film", "movi", "one", "charact", 
                  "make", "get", "just", "time", "seem", "even", "play", 
                  "stori", "also", "can", "see", "two", "two","look",  
                  "way"  ,"end",  "take", "come", "first" , "work", "plot",
                  "thing" ,   "littl" , "know", "year" ,"perform",  "peopl" ,"life")



rm_add_words # terminos que se retiran
# retirando terminos
corp <- tm_map(corp, removeWords, rm_add_words)
# objeto termino matriz
doc_term <- TermDocumentMatrix(corp)
# removiendo "sparcity"
tdm <- removeSparseTerms(doc_term,.90) # 80%
term.freq <- rowSums(as.matrix(tdm)) # términos más frecuentes
sort.freq <- sort.int(term.freq, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp <- names(sort.freq$x[1:50]) # nombre

# Estos son los nombres de los términos más frecuentes
tdm_matrix <- as.matrix(tdm)
# Calculate the rowSums: term_frequency
term_frequency <- rowSums(tdm_matrix)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = T)

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:50], col = "tan", las = 2)


# ordenando para visualizar
X <- count.terms(tdm,nterms.corp) 
X <- X[etiquetas$archivo.texto,]
# PCA Observaciones sin escalar
PCA <- princomp(X)
criticas <- etiquetas$sentimiento
p5 <- PCA$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA$scores[,1],y=PCA$scores[,2], col=criticas)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      caption="N: negativas; P: positivas")


# PCA observaciones escaladas
X_scale <- X %>% scale
PCA2 <- princomp(X_scale)
criticas <- etiquetas$sentimiento
p6 <-PCA2$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA2$scores[,1],y=PCA2$scores[,2], col=criticas)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Obs. escaladas",
                      caption="N: negativas; P: positivas")


grid.arrange(p5,p6,nrow=1)


#######################################################
#######################################################
#######################################################
######       Ejercicio 1 Inciso b #####################
#######################################################
#######################################################
#######################################################
rm(list = ls())
#################
#################
##
## Librerías
#################
#################
library("tm")
library("magrittr")
library("kernlab")
# ingrese la dirección donde se encuentran sus archivos 
getwd()
setwd("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/")
# extrae archivos de la etiquetas
etiquetas <- read.csv("movie_reviews.csv")
etiquetas$sentimiento %>% levels

# Cambiar ruta a la dirección de los archivos de críticas negativas
rut.data <- "C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/movie_reviews/movie_reviews/txtfiles/neg"
negative <- Corpus(DirSource(rut.data,recursive=TRUE),readerControl=list(language="en_US"))
# Cambiar ruta a la dirección de los archivos de críticas positivas
rut.data <- "C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/movie_reviews/movie_reviews/txtfiles/pos"
positivo <- Corpus(DirSource(rut.data,recursive=TRUE),readerControl=list(language="en_US"))


# Createlista  vacia para guardar los vectores
neg <- list() 
pos <- list() 
# Juntando críticas de 5 en cinco

  # Críticas negativas
k <- 5 # grupos de trxtos
for(j in 1:(500/k)){
for (i in 1:k){ 
  neg[[j]]<-negative[[(j-1)*k+i]]
} 
}
  # Críticas positivas
for(j in 1:(500/k)){
  for (i in 1:k){ 
    pos[[j]]<-positivo[[(j-1)*k+i]]
  } 
}

# Convertir listas a objetos VCorp
corp1 <- as.VCorpus(neg)
corp2 <- as.VCorpus(pos)
# Combinando corpus
corp <- c(corp1,corp2)

# preproceso

######################################
######################################
##
## Función limpia
##  input <- corpus de texto
##  retira: espacios en blanco
##          números
##          mayusculas a mínusculas
##          stop words en ingles
##          steams
##
######################################
######################################
limpiar <- function(corp){
  corp <- tm_map(corp,stripWhitespace) # espacios 
  corp <- tm_map(corp,removeNumbers) # números
  corp <- tm_map(corp,content_transformer(tolower)) # mayusculas
  corp <- tm_map(corp,removePunctuation) # puntuaciones
  corp <- tm_map(corp,removeWords,stopwords("english")) # stopwords
  corp <- tm_map(corp,stemDocument)  # stem
  return(corp)
} # end limpiar

# limpiar
corp <- limpiar(corp)

tdm <- TermDocumentMatrix(corp) # archivos a matriz
# 10202 términos 200 documentos
# 98% de esparcidad

tdm <- removeSparseTerms(tdm,.99) # eliminamos algo de esparsidad
# 3293 terminos 200 documentos
# 94% de esparcidad

term.freq <- rowSums(as.matrix(tdm)) # términos más frecuentes
sort.freq <- sort.int(term.freq, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp <- names(sort.freq$x[1:50]) # nombre

######################################
######################################
##
## Función count.terms
##  input <- matriz de terminos
##           nombre de terminos
##  output: Tabla de frecuencia
## 
##
######################################
######################################
count.terms <- function(tdms,terms){
  freq.table <- NULL
  for(i in 1:length(terms)){
    aa <- tm_term_score(tdms,terms[i])
    freq.table <- cbind(freq.table,aa)
  }
  colnames(freq.table) <- terms
  return(freq.table)
} # end count.terms

# tabla de terminos
X <- count.terms(tdm,nterms.corp) 
# índice para visualización
index1 <- rep(1,100) %>% as.matrix
index2 <- rep(2,100) %>% as.matrix
index <- rbind(index1,index2) 
# PCA Observaciones sin escalar
PCA <- princomp(X)
Critica <- as.factor(index)
p1 <- PCA$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA$scores[,1],y=PCA$scores[,2],col=Critica)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      caption="1: negativas; 2: positivas")


# PCA observaciones escaladas
X_scale <- X %>%  scale
PCA2 <- princomp(X_scale)
p2 <- PCA2$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA2$scores[,1],y=PCA2$scores[,2],col=Critica)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Obs. escaladas",
                      caption="1: negativas; 2: positivas; Datos escalados")


grid.arrange(p1,p2, nrow=1)


##############################################################################
##############################################################################
## Retirando observaciones más frecuentes que sesgan analisis
## aquellas palabras en común entre textos positivos y negativos
##############################################################################
##############################################################################


# mismas palabras que en el ejercicio anterior
rm_add_words <- c("film", "movi", "one", "charact", 
                  "make", "get", "just", "
                  time", "seem", "even", "play", 
                  "stori", "also", "can", "see", 
                  "two", "two","look",  "way"  ,"end",      "take",
                  "come"  ,   "first" , "work", "plot"  ,   "thing" ,   "littl"   , "know"    , "year" ,
                  "perform",  "peopl" ,   "life"   )

corp <- tm_map(corp, removeWords, rm_add_words)
doc_term <- TermDocumentMatrix(corp)
tdm <- removeSparseTerms(doc_term,.90) # 80%
term.freq <- rowSums(as.matrix(tdm)) # términos más frecuentes
sort.freq <- sort.int(term.freq, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp <- names(sort.freq$x[1:50]) # nombre

# Estos son los nombres de los términos más frecuentes
tdm_matrix <- as.matrix(tdm)
term_frequency <- rowSums(tdm_matrix)
term_frequency <- sort(term_frequency, decreasing = T)
# 50 palabras más frecuentes
barplot(term_frequency[1:50], col = "tan", las = 2)


# nuve de pablabras
set.seed(1234)
wordcloud(names(term_frequency[1:50]), term_frequency[1:50],
          max.words = 100, scale = c(3, 0.5), colors =  c("grey80","darkgoldenrod1", "tomato"))

# tabla de 50 terminos más frecuentes
X <- count.terms(tdm,nterms.corp) 
# PCA observaciones sin escalar
PCA <- princomp(X)

plot1 <- PCA$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA$scores[,1],y=PCA$scores[,2], col=Critica)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      caption="N: negativas; P: positivas")

# PCA observaciones con escalamiento
Y <- X %>% scale
PCA2 <- princomp(Y)

plot2 <- PCA2$scores %>% as.data.frame %>% ggplot() + 
  aes(x=PCA2$scores[,1],y=PCA2$scores[,2], col=Critica)+
  geom_point() + labs(title="Críticas Películas",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Obs.  escaladas",
                      caption="N: negativas; P: positivas")


grid.arrange(plot1,plot2, nrow=1)



#############################################################
#############################################################
################### FIN  EJERCICIO 3 ########################
#############################################################
#############################################################
