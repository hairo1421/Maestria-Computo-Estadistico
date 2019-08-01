############################################
############################################
############################################
#####   Tarea 6. Ciencia de datos
#####   Ejercicio 1. 
#####   Miranda Belmonte Hairo Ulise
#####   30 de Abril del 2019
############################################
############################################
############################################

#################
#################
## Librerías
#################
#################

library("magrittr")
library("tidyverse")
library("ggplot2")
library("kernlab")
library("factoextra")
library("gridExtra")
library("MASS")
library("caret")
library("nnet")
library("neuralnet")
library("GGally")

# introduce la ruta
getwd()
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 6")
# importa archivo my_all_tracks_2019
obs <- read.csv("my_all_tracks_2019", header = T)

# track.genre1, que indica el genero dado a cada canción
obs[1:23] %>% names
obs[24:31] %>% names
obs[32:549] %>% names  
obs %>% dim
#################
#################
## inciso a
#################
#################

# carácteristicas de pista
audioFeatures <- obs[24:31]
# carácteristicas de pista + duración de la pista
names(audioFeatures)[9] <- "track.duration"
Generos <- obs$track.genre1 
# subconjunto de datos
set.seed(1)
X <- sample_frac(tbl = cbind(Generos,audioFeatures), replace = FALSE, size = .20)
################
# PCA
###############

pca <- prcomp(X[,2:9], scale. = T) # no se toma encuenta primera columna
# Nota, se escalan los datos.
summary(pca)
# scores
score <- pca$x
# etiqueta generos
Generos <- X$Generos
# screeplot
p1 <- factoextra::fviz_screeplot(pca)
# Visualiza dos primeras componentes

p2 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Generos,shape=Generos) +
  scale_shape_manual(values=1:nlevels(Generos)) +
  geom_point() + labs(title="PCA",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos de la pista",
                      caption="Observaciones escaladas") 

##########################################
# Kmeans a las dos primeras componentes
##########################################
# Número de categorias
n <- obs$track.genre1 %>% levels() %>% length()
# K means
set.seed(1)
ObjKmeans <- kmeans(score[,1:2], n , iter.max = 100)
# Clusters
Grupos <- ObjKmeans$cluster %>% as.factor
# Visualización
p3 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Grupos,shape=Generos) +
  scale_shape_manual(values=1:nlevels(Generos)) +
  geom_point() + labs(title="Kmeans",
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos de las pistas",
                      caption="Observaciones escaladas") 

p3 <- p3 +  theme(legend.position = "bottom",
            legend.box = "vertical",legend.title = element_text(color = "blue"))



x11()
gridExtra::grid.arrange(p2, p3, nrow=1)
##########################################
# Spectral a las dos primeras componentes
##########################################

# se utiliza un kernel gaussiano con parametro de .1
set.seed(1)
ObjSpec <- specc(as.matrix(score[,1:2]), n, kernel = "rbfdot", kpar = list(sigma=.1))
# clusters
Grupos2 <- ObjSpec@.Data %>% as.factor
# etiqueta de los datos
Genero2 <- Generos
p4 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Grupos2,shape=Genero2) +
  scale_shape_manual(values=1:nlevels(Genero2)) +
  geom_point() + labs(title=bquote("Clustering Espectral" ~ sigma == .1),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p4 <- p4 +  theme(legend.position = "bottom",
            legend.box = "vertical",legend.title = element_text(color = "blue"))



# Incrementa valor del parámetro sigma a 10
set.seed(1)
ObjSpec2 <- specc(as.matrix(score[,1:2]), n, kernel = "rbfdot", kpar = list(sigma=10))
Grupos3 <- ObjSpec2@.Data %>% as.factor
Genero3 <- Generos
p5 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Grupos3,shape=Genero3) +
  scale_shape_manual(values=1:nlevels(Genero3)) +
  geom_point() + labs(title=bquote("Clustering Espectral" ~ sigma == 10),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p5 <- p5 +  theme(legend.position = "bottom",
                  legend.box = "vertical",legend.title = element_text(color = "blue"))


x11()
gridExtra::grid.arrange(p4, p5, nrow=1)

##################################################
# Kernel Kmeans primeras dos componentes
##################################################

# parámetro sigma de .1
set.seed(1)
A2 <- kkmeans(x = as.matrix(X[,2:9]), n,  kernel = "rbfdot", kpar = list(sigma = 0.1))
cluster <- A2@.Data %>% as.factor
p6 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=cluster,shape=Genero3) +
  scale_shape_manual(values=1:nlevels(Genero3)) +
  geom_point() + labs(title=bquote("Kernel Kmeans" ~ sigma == .1),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p6 <- p6 +  theme(legend.position = "bottom",
                  legend.box = "vertical",legend.title = element_text(color = "blue"))

# parámetro sigma de 10
set.seed(1)
A3 <- kkmeans(x = as.matrix(X[,2:9]), n,  kernel = "rbfdot", kpar = list(sigma = 10))
cluster2 <- A3@.Data %>% as.factor
p7 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=cluster2,shape=Genero3) +
  scale_shape_manual(values=1:nlevels(Genero3)) +
  geom_point() + labs(title=bquote("Kernel Kmeans" ~ sigma == 10),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p7 <- p7 +  theme(legend.position = "bottom",
                  legend.box = "vertical",legend.title = element_text(color = "blue"))


x11()
grid.arrange(p6,p7,nrow=1)



X2 <- X  %>%  filter(Generos!="Old-Time / Historic" & Generos!="International",
                     Generos!="Instrumental" & Generos!="Experimental",
                     Generos!="Easy Listening")
X2$Generos <- X2$Generos %>% as.character %>% as.factor 

##########################################
# Spectral a las dos primeras componentes
##########################################

pca <- prcomp(X2[,2:9], scale. = T)
score <- pca$x
n <- X2$Generos %>% nlevels
# se utiliza un kernel gaussiano con parametro de .1
set.seed(1)
ObjSpec3 <- specc(as.matrix(score[,1:2]), n, kernel = "rbfdot", kpar = list(sigma=.1))
# clusters
Grupos <- ObjSpec3@.Data %>% as.factor
# etiqueta de los datos
Genero <- X2$Generos
p8 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Grupos,shape=Genero) +
  scale_shape_manual(values=1:nlevels(Genero)) +
  geom_point() + labs(title=bquote("Clustering Espectral" ~ sigma == .1),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p8 <- p8 +  theme(legend.position = "bottom",
                  legend.box = "vertical",legend.title = element_text(color = "blue"))



# Incrementa valor del parámetro sigma a 10
set.seed(1)
ObjSpec4 <- specc(as.matrix(score[,1:2]), n, kernel = "rbfdot", kpar = list(sigma=10))
Grupos2 <- ObjSpec4@.Data %>% as.factor
Genero <- X2$Generos
p9 <- score %>% as.data.frame %>% ggplot() + 
  aes(x=score[,1],y=score[,2],col=Grupos2,shape=Genero) +
  scale_shape_manual(values=1:nlevels(Genero)) +
  geom_point() + labs(title=bquote("Clustering Espectral" ~ sigma == 10),
                      x="PC 1",
                      y="PC 2",
                      subtitle="Generos",
                      caption="Observaciones escaladas") 


p9 <- p9 +  theme(legend.position = "bottom",
                  legend.box = "vertical",legend.title = element_text(color = "blue"))


x11()
gridExtra::grid.arrange(p8, p9, nrow=1)


#####################
# inciso b 
#####################

# Nuevo subconjunto de datos
set.seed(1)
Obs <- sample_frac(tbl = obs, replace = FALSE, size = .10)
Obs %>% dim # 1313  549

# Selecciona un conjunto de entrenamiento y otro de prueba.
# de la base de datos
Datos_train <- sample_frac(tbl = Obs, replace = FALSE, size = 0.80)
Datos_test <- anti_join(Obs, Datos_train)

Datos_train %>% dim #  1050  549
Datos_test %>% dim # 263 549


# variables respuestas
Y1_train <- Datos_train$audio_features.danceability
Y1_test <- Datos_test$audio_features.danceability

Y2_train <- Datos_train$audio_features.energy
Y2_test <- Datos_test$audio_features.energy

# covariables
X_train <- Datos_train[,-c(25:26)]
X_test <- Datos_test[,-c(25:26)]
X_test %>% names

# Para cada variable de respuesta:

# Construye una variable con 3 categorias (baja, media, alta) a partir de sus
#valores numericos

# Categorias
# baja menor a 0.35 -> 0
# media mayor a 0.35 y menor a 0.65 -> 0
# alta  mayor a 0.65

# bailabilidad
Y1_train_category <- ifelse(Y1_train>0.65,2,ifelse(Y1_train>0.35,1,0)) %>% as.factor
Y1_test_category <- ifelse(Y1_test>0.65,2,ifelse(Y1_test>0.35,1,0)) %>% as.factor

# energía
Y2_train_category <- ifelse(Y2_train>0.65,2,ifelse(Y2_train>0.35,1,0)) %>% as.factor
Y2_test_category <- ifelse(Y2_test>0.65,2,ifelse(Y2_test>0.35,1,0)) %>% as.factor


# Ajusta clasicadores basados en LDA, QDA, Multilogit y Redes Neuronales,
# para estimar el nivel de \bailabilidad" y \energa" usando el bloque de
# caractersticas de la se~nal como covariables. Compara los resultados para cada clasicador y documenta tus hallazgos.


# Se debe extraer variable con valores constantes
# Primeros filtremos la base de datos de entrenamiento y de prueba
X_test %>% names
X_train %>% names
# variables característica señales
X_test_senal <- X_test[30:547]
X_train_senal <- X_train[30:547]
# removemos variables constantes
Nombres <- names(X_train_senal[, sapply(X_train_senal, function(v) var(v, na.rm=TRUE)>=1)])
VARIABLES <- as.data.frame(Nombres)
write.xlsx(VARIABLES, "VARIABLES MODELOS.xlsx")

X_test_senal2 <- X_test_senal[,Nombres]
X_train_senal2 <- X_train_senal[,Nombres]




###########################
# LDA Modelo1 bailabilidad
###########################
train_Modelo1 <- cbind(Y1_train_category,X_train_senal2)

lda.model <- lda(factor(Y1_train_category)~., data=train_Modelo1)

##Predicting training results.
predmodel.train.lda <- predict(lda.model, data=train_Modelo1)
Tabla1 <- table(Predicted=predmodel.train.lda$class, Etiqueta=Y1_train_category)

#Etiqueta
#Predicted   0   1   2
#0 187  49   6
#1  75 456  71
#2   9  49 148

# porcentaje de veces que son iguales
mean(predmodel.train.lda$class==Y1_train_category) # 0.7533333
# porcentaje de veces de error
mean(predmodel.train.lda$class!=Y1_train_category) # 0.2466667


# checamos precisión
predmodel.test.lda <- predict(lda.model, newdata=X_test_senal2)
# tabla de confusión
Tabla2 <- table(Predicted=predmodel.test.lda$class, Etiquetas=Y1_test_category)
#Etiquetas
#Predicted   0   1   2
#0  37  22   1
#1  25 101  24
#2   5  17  31

x11()
plot1 <- ldahist(predmodel.test.lda$x[,1], g= predmodel.test.lda$class)
# porcentaje de veces que son iguales
mean(predmodel.test.lda$class==Y1_test_category) #0.6425856
# porcentaje de veces de error
mean(predmodel.test.lda$class!=Y1_test_category) #0.3574144

x11()
# convertir a ggplot
Categorias <- Y1_test_category
Categorias <-  ifelse(Categorias==2, "Alta", ifelse(Categorias==1,"Media", "Baja"))

plot2 <- predmodel.test.lda %>% as.data.frame %>% 
  ggplot() + aes(x=predmodel.test.lda$x[,1], y=predmodel.test.lda$class, col=Categorias) + geom_point() +
  labs(title="LDA", subtitle = "Clasificación Bailabilidad", y="clases", x="Obs")


###########################
# LDA Modelo2 energía
###########################
train_Modelo2 <- cbind(Y2_train_category,X_train_senal2)

lda.model2 <- lda(factor(Y2_train_category)~., data=train_Modelo2)

##Predicting training results.
predmodel.train.lda2 <- predict(lda.model2, data=train_Modelo2)
Tabla3 <- table(Predicted=predmodel.train.lda2$class, Etiqueta=Y2_train_category)

#Etiqueta
#Predicted   0   1   2
#0 239  18   1
#1  48 253  54
#2   4  61 372


# porcentaje de veces que son iguales
mean(predmodel.train.lda2$class==Y2_train_category) #0.8228571
# porcentaje de veces de error
mean(predmodel.train.lda2$class!=Y2_train_category) #0.1771429


# checamos precisión
predmodel.test.lda2 <- predict(lda.model2, newdata=X_test_senal2)
Tabla4 <- table(Predicted=predmodel.test.lda2$class, Etiquetas=Y2_test_category)
#Etiquetas
#Predicted  0  1  2
#0 56  8  1
#1 19 49 20
#2  4 28 78

x11()
plot3 <- ldahist(predmodel.test.lda2$x[,1], g= predmodel.test.lda2$class)

# porcentaje de veces que son iguales
mean(predmodel.test.lda2$class==Y2_test_category) #0.6958175
# porcentaje de veces de error
mean(predmodel.test.lda2$class!=Y2_test_category) #0.3041825

x11()
# convertir a ggplot
Categorias2 <- Y2_test_category
Categorias2 <-  ifelse(Categorias2==2, "Alta", ifelse(Categorias2==1,"Media", "Baja"))

plot4 <- predmodel.test.lda2 %>% as.data.frame %>% 
  ggplot() + aes(x=predmodel.test.lda2$x[,1], y=predmodel.test.lda2$class, col=Categorias2) + geom_point() +
  labs(title = "LDA", subtitle = "Clasificación Energía", y="clases", x="Obs")



###########################
# QDA Modelo1 bailabilidad
###########################

train_Modelo1_qda <- cbind(Y1_train_category,X_train_senal2)

qda.model <- qda(factor(Y1_train_category)~., data=train_Modelo1_qda)

##Predicting training results.
predmodel.train.qda <- predict(qda.model, data=train_Modelo1_qda)
Tabla1_qda <- table(Predicted=predmodel.train.qda$class, Etiqueta=Y1_train_category)

#Etiqueta
#Predicted   0   1   2
#0 271   0   0
#1   0 554   0
#2   0   0 225

# porcentaje de veces que son iguales
mean(predmodel.train.qda$class==Y1_train_category) #1
# porcentaje de veces de error
mean(predmodel.train.qda$class!=Y1_train_category) #0


# checamos precisión
predmodel.test.qda <- predict(qda.model, newdata=X_test_senal2)
# tabla de confusión
Tabla2_qda <- table(Predicted=predmodel.test.qda$class, Etiquetas=Y1_test_category)

#Predicted   0   1   2
#0   1   1   0
#1  66 139  56
#2   0   0   0


# porcentaje de veces que son iguales
mean(predmodel.test.qda$class==Y1_test_category) #0.5323194
# porcentaje de veces de error
mean(predmodel.test.qda$class!=Y1_test_category) #0.4676806

x11()
# convertir a ggplot
Categorias <- Y1_test_category
Categorias <-  ifelse(Categorias==2, "Alta", ifelse(Categorias==1,"Media", "Baja"))

###########################
# QDA Modelo1 Energía
###########################

train_Modelo2_qda <- cbind(Y2_train_category,X_train_senal2)

qda.model2 <- qda(factor(Y2_train_category)~., data=train_Modelo2_qda)

##Predicting training results.
predmodel.train.qda2 <- predict(qda.model2, data=train_Modelo2_qda)
Tabla3_qda <- table(Predicted=predmodel.train.qda2$class, Etiqueta=Y2_train_category)

#Etiqueta
#Predicted   0   1   2
#0 290   0   0
#1   0 331   0
#2   1   1 427


# porcentaje de veces que son iguales
mean(predmodel.train.qda2$class==Y2_train_category) #0.9980952
# porcentaje de veces de error
mean(predmodel.train.qda2$class!=Y2_train_category) #0.001904762


# checamos precisión
predmodel.test.qda2 <- predict(qda.model2, newdata=X_test_senal2)
# tabla de confusión
Tabla4_qda <- table(Predicted=predmodel.test.qda2$class, Etiquetas=Y2_test_category)
#Etiquetas
#Predicted  0  1  2
#0 33  9  1
#1 14 10  2
#2 32 66 96


# porcentaje de veces que son iguales
mean(predmodel.test.qda2$class==Y2_test_category) #0.5285171
# porcentaje de veces de error
mean(predmodel.test.qda2$class!=Y2_test_category) #0.4714829


# convertir a ggplot
Categorias2 <- Y2_test_category
Categorias2 <-  ifelse(Categorias2==2, "Alta", ifelse(Categorias2==1,"Media", "Baja"))


#####################################
# Multilogit Modelo1 bailabilidad
#####################################
train_Modelo1_mlogit <- cbind(Y1_train_category,X_train_senal2)

# Fit the model
model_mlogit <- nnet::multinom(factor(Y1_train_category)~., data=train_Modelo1_mlogit )
# Summarize the model
#summary(model_mlogit)

##Predicting training results.
predmodel.train.mlogit <- predict(model_mlogit, data=train_Modelo1_mlogit)
Tabla1_mlogit <- table(Predicted=predmodel.train.mlogit, Etiqueta=Y1_train_category)
#Etiqueta
#Predicted   0   1   2
#0 172  55  15
#1  86 448  70
#2  13  51 140

# porcentaje de veces que son iguales
mean(predmodel.train.mlogit==Y1_train_category) #0.7238095
# porcentaje de veces de error
mean(predmodel.train.mlogit!=Y1_train_category) #0.2761905

# checamos precisión
predmodel.test.mlogit <- predict(model_mlogit, newdata=X_test_senal2)
# tabla de confusión
Tabla2_mlogit <- table(Predicted=predmodel.test.mlogit, Etiquetas=Y1_test_category)
#Etiquetas
#Predicted  0  1  2
#0 36 33  5
#1 23 88 25
#2  8 19 26

# porcentaje de veces que son iguales
mean(predmodel.test.mlogit==Y1_test_category) #0.5703422
# porcentaje de veces de error
mean(predmodel.test.mlogit!=Y1_test_category) #0.4296578



#####################################
# Multilogit Modelo2 Energía
#####################################
train_Modelo2_mlogit <- cbind(Y2_train_category,X_train_senal2)

# Fit the model
model2_mlogit <- nnet::multinom(factor(Y2_train_category)~., data=train_Modelo2_mlogit)
# Summarize the model
#summary(model2_mlogit)

##Predicting training results.
predmodel.train.mlogit2 <- predict(model2_mlogit, data=train_Modelo2_mlogit)
Tabla3_mlogit <- table(Predicted=predmodel.train.mlogit2, Etiqueta=Y2_train_category)
#Etiqueta
#Predicted   0   1   2
#0 254  31   6
#1  26 209  49
#2  11  92 372

# porcentaje de veces que son iguales
mean(predmodel.train.mlogit2==Y2_train_category)#0.7952381
# porcentaje de veces de error
mean(predmodel.train.mlogit2!=Y2_train_category)#0.2047619

# checamos precisión
predmodel.test.mlogit2 <- predict(model2_mlogit, newdata=X_test_senal2)
# tabla de confusión
Tabla4_mlogit <- table(Predicted=predmodel.test.mlogit2, Etiquetas=Y2_test_category)
#Etiquetas
#Predicted  0  1  2
#0 49 13  2
#1 23 37 16
#2  7 35 81

# porcentaje de veces que son iguales
mean(predmodel.test.mlogit2==Y2_test_category) #0.634981
# porcentaje de veces de error
mean(predmodel.test.mlogit2!=Y2_test_category) #0.365019


#####################################
# Redes Modelo1 bailabilidad
#####################################


                                                                                                                                                        0, TRUE, FALSE)) 
# concatenando datos de entrenamientos con sus respuestas
base_datos <- cbind(Y1_train_category, X_train_senal2)
base_datos %>% names %>% head
# pocentaje de obs que se utilizan 100%
base_datos2 <- base_datos

base_datos2 %>% dim

# codificar etiquetas como multiclase; i.e., como dummy's por categoría
# Entrenamiento
train <- cbind(base_datos2, class.ind(as.factor(base_datos2$Y1_train_category)))

train %>%  dim
base_datos2 %>% dim

# Nombre de las nuevas variables
names(train) <- c(names(base_datos2)[1:207],"l1","l2","l3")
# Se escalan las observaciones
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[2:210] <- data.frame(lapply(train[2:210], scl))
head(train)
# Se retira la variable respuesta que no es dummy
train2 <- train[2:210]
# Formula del modelo
n <- names(train2)
f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1","l2","l3")], collapse = " + ")))
f
# Ajuste del modelo datos de entrenamiento

# con una capa

#set.seed(11)
set.seed(55)
red0 <- neuralnet(f,
                  data = train2,
                  hidden = 4,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  likelihood = TRUE, algorithm = "rprop+")
  
set.seed(55)
red1 <- neuralnet(f,
                  data = train2,
                  hidden = 5,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  likelihood = TRUE, algorithm = "rprop+")


set.seed(2)
red2 <- neuralnet(f,
                  data = train2,
                  hidden = 6,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  likelihood = TRUE,   algorithm = "rprop+")

set.seed(2)
red3 <- neuralnet(f,
                 data = train2,
                hidden = 3,
               act.fct = "logistic",
              linear.output = FALSE,
             lifesign = "minimal",
            likelihood = TRUE,   algorithm = "rprop+")




set.seed(3)
red4 <- neuralnet(f,
                 data = train2,
                hidden = c(20,10,3),
               act.fct = "logistic",
              linear.output = FALSE,
             lifesign = "minimal",
            likelihood = TRUE)




# Bar plot 
Class_NN_ICs <- tibble(Network = rep(c("RED1", "RED2", "RED3", "RED4","RED5"), each = 3), 
                       Metric = rep(c("AIC", "BIC", "ce Error * 100"), length.out = 15), 
                       Value = c(red3$result.matrix[4, 1], red3$result.matrix[5, 1], 100 * red3$result.matrix[1, 1],
                                 red0$result.matrix[4,1], red0$result.matrix[5, 1], 100 * red0$result.matrix[1, 1],
                                 red1$result.matrix[4,1], red1$result.matrix[5, 1], 100 * red1$result.matrix[1, 1], 
                                 red2$result.matrix[4, 1], red2$result.matrix[5, 1], 100 * red2$result.matrix[1,1], 
                                red4$result.matrix[4, 1], red4$result.matrix[5, 1], 100 * red4$result.matrix[1, 1]))

graph1 <- Class_NN_ICs %>% ggplot(aes(Network, Value, fill = Metric)) + geom_col(position = "dodge") + 
  ggtitle("AIC, BIC, y Cross-Entropy Error de clasificación ANNs", 
          "Nota: ce Error es 100 veces su valor verdadero")


Class_NN_ICs2 <- tibble(Network = rep(c("RED1", "RED2", "RED3", "RED4"), each = 3), 
                       Metric = rep(c("AIC", "BIC", "ce Error * 100"), length.out = 12), 
                       Value = c(red3$result.matrix[4, 1], red3$result.matrix[5, 1], 100 * red3$result.matrix[1, 1],
                                 red0$result.matrix[4,1], red0$result.matrix[5, 1], 100 * red0$result.matrix[1, 1],
                                 red1$result.matrix[4,1], red1$result.matrix[5, 1], 100 * red1$result.matrix[1, 1], 
                                 red2$result.matrix[4, 1], red2$result.matrix[5, 1], 100 * red2$result.matrix[1,1]))

graph2 <- Class_NN_ICs2 %>% ggplot(aes(Network, Value, fill = Metric)) + geom_col(position = "dodge") + 
  ggtitle("AIC, BIC, y Cross-Entropy Error de clasificación ANNs", 
          "Nota: ce Error es 100 veces su valor verdadero")

X11()

grid.arrange(graph1, graph2, nrow=1)

# El error que se observa en el gráfico es el valor del cross-entropy error, el cual es una medición entre las diferencias
# de los valores predichos y observados en cada observación
red0$result.matrix[1,1]
red1$result.matrix[1,1]
red2$result.matrix[1,1]
red3$result.matrix[1,1]
red4$result.matrix[1,1]

# Se opta por un modelo no tan complejo; i.e., una capa
X11()
#red2
plot(red4)
#plot(red2)
train2 %>% names
pr.nn <- compute(red2, train2[1:206]) # cambie la red para ver la precisión
pr.nn_ <- pr.nn$net.result
head(pr.nn_)
# Accuracy (training set)
train2 %>% dim
original_values <- max.col(train2[,207:209])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)
mean(pr.nn_2 != original_values)
Tabla1_net <- table(pr.nn_2,original_values)

#original_values
#pr.nn_2   1   2   3
#1 239   8   1
#2  27 523  41
#3   5  23 183

# acurracy 
# red0 0.8714286
# red1 0.8933333
# red2 .9
# red3 0.8580952
# red4 0.9361905

# error clasificación
# red0 0.1285714
# red1 0.1066667
# red2 .1
# red3 0.1419048
# red4 0.06380952

# Se selecciona red2

# datos prueba
base_datos3 <-  cbind(Y1_test_category, X_test_senal2)
#base_datos3 <-  sample_frac(tbl = base_datos3, replace = FALSE, size = .25)
base_datos3 %>% dim
train_test <- cbind(base_datos3, class.ind(as.factor(base_datos3$Y1_test_category)))
train_test %>%  head
train_test %>% dim
# nombre de los datos
names(train_test) <- c(names(train_test)[1:207],"l1","l2","l3")
# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train_test[2:207] <- data.frame(lapply(train_test[2:207], scl))
head(train_test)
train_test2 <- train_test[2:210]
train_test2 %>% names
pr.nn_test <- compute(red1, train_test2[1:206])
pr.nn_test_ <- pr.nn_test$net.result
head(pr.nn_test_)
# precisión de los datos
original_values_test <- max.col(train_test2[,207:209])
pr.nn_2_test <- max.col(pr.nn_test_)
mean(pr.nn_2_test == original_values_test)
mean(pr.nn_2_test != original_values_test)
Tabla2_net <- table(pr.nn_2_test,original_values_test)

#original_values_test
#pr.nn_2_test  1  2  3
#1 37 49  5
#2 30 89 41
#3  0  2 10

# red 2 
# acuracy
# 0.5171103
# 0.4828897

#####################################
# Redes Modelo1 Energía
#####################################

# concatenando datos de entrenamientos con sus respuestas

base_datos_energy <- cbind(Y2_train_category, X_train_senal2)
base_datos_energy %>% names %>% head
base_datos_energy2 <- base_datos_energy
#base_datos_energy2 <-  sample_frac(tbl = base_datos_energy, replace = FALSE, size = .25)
base_datos_energy2 %>% dim #1050 obs

# codificar etiquetas como multiclase; i.e., como dummy's por categoría
# Entrenamiento
train_energy <- cbind(base_datos_energy2, class.ind(as.factor(base_datos_energy2$Y2_train_category)))

train_energy %>%  dim
base_datos_energy2 %>% dim

# Nombre de las nuevas variables
names(train_energy) <- c(names(base_datos_energy2)[1:207],"l1","l2","l3")
# Se escalan las observaciones
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train_energy[2:207] <- data.frame(lapply(train_energy[2:207], scl))
head(train_energy)

# Se retira la variable respuesta que no es dummy
train_energy2 <- train_energy[2:210]
# Formula del modelo
n <- names(train_energy2)
f <- as.formula(paste("l1 + l2 + l3 ~", paste(n[!n %in% c("l1","l2","l3")], collapse = " + ")))
f
# Ajuste del modelo datos de entrenamiento

# con una capa


set.seed(1)
red0_energy <- neuralnet(f,
                         data = train_energy2,
                         hidden = 4,
                         act.fct = "logistic",
                         linear.output = FALSE,
                         lifesign = "minimal",
                         likelihood = TRUE)


set.seed(100)
red1_energy <- neuralnet(f,
                         data = train_energy2,
                         hidden = 5,
                         act.fct = "logistic",
                         linear.output = FALSE,
                         lifesign = "minimal",
                         likelihood = TRUE)
set.seed(101)
red2_energy <- neuralnet(f,
                         data = train_energy2,
                         hidden = 6,
                         act.fct = "logistic",
                         linear.output = FALSE,
                         lifesign = "minimal",
                         likelihood = TRUE)
set.seed(200)
red3_energy <- neuralnet(f,
                         data = train_energy2,
                         hidden = 3,
                         act.fct ="logistic",
                         linear.output = FALSE,
                         lifesign = "minimal",
                         likelihood = TRUE)

set.seed(300)
red4_energy <- neuralnet(f,
                         data = train_energy2,
                         hidden = c(20,10,3),
                         act.fct = "logistic",
                         linear.output = FALSE,
                         lifesign = "minimal",
                         likelihood = TRUE)




# Bar plot 
Class_NN_ICs <- tibble(Network = rep(c("RED1", "RED2", "RED3", "RED4",  "RED5"), each = 3), 
                       Metric = rep(c("AIC", "BIC", "ce Error * 100"), length.out = 15), 
                       Value = c(red3_energy$result.matrix[4, 1], red3_energy$result.matrix[5, 1], 100 * red3_energy$result.matrix[1, 1],
                                 red0_energy$result.matrix[4,1], red0_energy$result.matrix[5, 1], 100 * red0_energy$result.matrix[1, 1],
                                 red1_energy$result.matrix[4,1], red1_energy$result.matrix[5, 1], 100 * red1_energy$result.matrix[1, 1], 
                                 red2_energy$result.matrix[4, 1], red2_energy$result.matrix[5, 1], 100 * red2_energy$result.matrix[1,1],
                                 red4_energy$result.matrix[4, 1], red4_energy$result.matrix[5,1], 100 * red4_energy$result.matrix[1, 1]))

graph4 <- Class_NN_ICs %>% ggplot(aes(Network, Value, fill = Metric)) + geom_col(position = "dodge") + 
  ggtitle("AIC, BIC, y Cross-Entropy Error de clasificación ANNs", 
          "Note: ce Error es 100 veces su valor verdadero")

Class_NN_ICs2 <- tibble(Network = rep(c("RED1", "RED2", "RED3", "RED4"), each = 3), 
                       Metric = rep(c("AIC", "BIC", "ce Error * 100"), length.out = 12), 
                       Value = c(red3_energy$result.matrix[4, 1], red3_energy$result.matrix[5, 1], 100 * red3_energy$result.matrix[1, 1],
                                 red0_energy$result.matrix[4,1], red0_energy$result.matrix[5, 1], 100 * red0_energy$result.matrix[1, 1],
                                 red1_energy$result.matrix[4,1], red1_energy$result.matrix[5, 1], 100 * red1_energy$result.matrix[1, 1], 
                                 red2_energy$result.matrix[4, 1], red2_energy$result.matrix[5, 1], 100 * red2_energy$result.matrix[1,1]))

graph5 <- Class_NN_ICs2 %>% ggplot(aes(Network, Value, fill = Metric)) + geom_col(position = "dodge") + 
  ggtitle("AIC, BIC, y Cross-Entropy Error de clasificación ANNs", 
          "Note: ce Error es 100 veces su valor verdadero")
X11()
grid.arrange(graph4, graph5 ,nrow=1)

# cross entropy
red0_energy$result.matrix[1,1]
red1_energy$result.matrix[1,1] # 56.075
red2_energy$result.matrix[1,1]
red3_energy$result.matrix[1,1]
red4_energy$result.matrix[1,1] # 53.51424
# red0 que es de 4 nodos

X11()
plot(red1_energy)
# clasificación 

pr.nn_energy <- compute(red1_energy, train_energy2[,1:206]) # cabie de red para er precisión
pr.nn_energy_ <- pr.nn_energy$net.result
head(pr.nn_energy_)
# Accuracy (training set)
train_energy2 %>% dim
original_values_energy <- max.col(train_energy2[,207:209])
pr.nn_2_energy  <- max.col(pr.nn_energy_)
mean(pr.nn_2_energy  == original_values_energy)
mean(pr.nn_2_energy  != original_values_energy)
Tabla3_net <- table(pr.nn_2_energy,original_values_energy)

#original_values_energy
#pr.nn_2_energy   1   2   3
#1 273   4   1
#2  15 305  15
#3   3  23 411

# acuraccy
# red0 0.9219048
# red1 0.9419048
# red2 0.9361905
# red3 0.9171429
# red4 0.9390476


# error
# red0 0.07809524
# red1 0.05809524
# red2 0.06380952
# red3 0.08285714
# red4 0.06095238

# prueba
base_datos_energy3 <-  cbind(Y2_test_category, X_test_senal2)
set.seed(1)
#base_datos_energy3 <-  sample_frac(tbl = base_datos_energy3, replace = FALSE, size = .25)
base_datos_energy3 %>% dim # 263 obs
train_test_energy <- cbind(base_datos_energy3, class.ind(as.factor(base_datos_energy3$Y2_test_category)))
train_test_energy %>%  head
train_test_energy %>% names
# Set labels name
names(train_test_energy) <- c(names(train_test_energy)[1:207],"l1","l2","l3")
# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train_test_energy[2:207] <- data.frame(lapply(train_test_energy[2:207], scl))
head(train_test_energy)
train_test_energy %>% names
train_test_energy2 <- train_test_energy[2:210]
train_test_energy2%>% names
pr.nn_test_energy <- compute(red0_energy, train_test_energy2[1:206])
pr.nn_test_energy_ <- pr.nn_test_energy$net.result
head(pr.nn_test_energy_)
# Accuracy (training set)
original_values_test_energy <- max.col(train_test_energy2[,207:209])
pr.nn_2_test_energy <- max.col(pr.nn_test_energy_)
mean(pr.nn_2_test_energy == original_values_test_energy)
mean(pr.nn_2_test_energy != original_values_test_energy)
Tabla5_net <- table(pr.nn_2_test_energy,original_values_test_energy)

#original_values_test_energy
#pr.nn_2_test_energy  1  2  3
#1 73 23  9
#2  4 30 18
#3  2 32 72

# accuracy
# red2 0.6653992
# error
# red2 0.3346008


#################
#################
## inciso c
#################
#################
library("openxlsx")


# introduce la ruta
getwd()
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 6")
# importa archivo my_all_tracks_2019
nuuevas_obs <- read.csv("my_all_tracks_No_genre_2019", header = T)
nuuevas_obs %>% dim
nuuevas_obs %>% names
# Filtrar variables que se utilizaron en los modelos
BASE <- nuuevas_obs[,Nombres]
Etiqutas  <- nuuevas_obs[,1:13]
BASE_new <- cbind(Etiqutas,BASE)
BASE_new %>% dim
# Se toma subconjunto para probar
BASE2 <- sample_frac(tbl = BASE_new, replace = FALSE, size = 0.05)
BASE2 %>% dim # 112 Obs
#########################
# Modelo 1 Bailabilidad
#########################

# LDA
LDA <- predict(lda.model, newdata=BASE2[14:219])
# Clasificación
Labels_LDA <- ifelse(LDA$class==2,"Alto",ifelse(LDA$class==1,"Medio","Bajo"))
Base_LDA <- data.frame(Grupo = LDA$class, Grupo_Etiqueta = Labels_LDA, id_Pista=BASE2$track_id,
           Archivo=BASE2$file)
           
write.xlsx(Base_LDA, "Base_LDA_bailabilidad.xlsx")

# QDA
QDA <- predict(qda.model, newdata=BASE2[14:219])

# Clasificación
Labels_QDA <- ifelse(QDA$class==2,"Alto",ifelse(QDA$class==1,"Medio","Bajo"))
BaseQDA <- data.frame(Grupo = QDA$class, Grupo_Etiqueta = Labels_QDA, id_Pista=BASE2$track_id,
                         Archivo=BASE2$file)

write.xlsx(BaseQDA, "BaseQDA_bailabilidad.xlsx")

#Multilogit
MLOGIT <- predict(model_mlogit, newdata=BASE2[14:219])
# Clasificación
Labels_MLOGIT <- ifelse(MLOGIT==2,"Alto",ifelse(MLOGIT==1,"Medio","Bajo"))
BaseMLOGIT  <- data.frame(Grupo = MLOGIT, Grupo_Etiqueta = Labels_MLOGIT, id_Pista=BASE2$track_id,
                      Archivo=BASE2$file)

write.xlsx(BaseMLOGIT, "BaseMLOGIT_bailabilidad.xlsx")

# RED NEURONAL 3
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
BASE2_escalada <- data.frame(lapply(BASE2[14:219], scl))
head(BASE2_escalada)
RED3 <- compute(red2, BASE2_escalada)
RED3_ <- RED3$net.result
head(RED3_)
RED3_Etiquetas <- max.col(RED3_)
Labels_red3 <- ifelse(RED3_Etiquetas==3,"Alto",ifelse(RED3_Etiquetas==2,"Medio","Bajo"))
Base_RED3<- data.frame(Grupo = RED3_Etiquetas, Grupo_Etiqueta = Labels_red3, id_Pista=BASE2$track_id,
                       Archivo=BASE2$file)
write.xlsx(Base_RED3, "Base_RED3_bailabilidad.xlsx")
#       3           Alto   135742 fma_large/135/135742.mp3
#      3           Alto   125284 fma_large/125/125284.mp3
#      1           Bajo    12070 fma_large/012/012070.mp3

RED5 <- compute(red4, BASE2_escalada)
RED5_ <- RED5$net.result
head(RED5_)
RED5_Etiquetas <- max.col(RED5_)
Labels_red5 <- ifelse(RED5_Etiquetas==3,"Alto",ifelse(RED5_Etiquetas==2,"Medio","Bajo"))
Base_RED5<- data.frame(Grupo = RED5_Etiquetas, Grupo_Etiqueta = Labels_red5, id_Pista=BASE2$track_id,
                       Archivo=BASE2$file)

write.xlsx(Base_RED5, "Base_RED5_bailabilidad.xlsx")


#########################
# Modelo 2 Energía
#########################

# LDA

LDA2 <- predict(lda.model2, newdata=BASE2[14:219])
# Clasificación
Labels_LDA2 <- ifelse(LDA2$class==2,"Alto",ifelse(LDA2$class==1,"Medio","Bajo"))
Base_LDA2 <- data.frame(Grupo = LDA2$class, Grupo_Etiqueta = Labels_LDA2, id_Pista=BASE2$track_id,
                       Archivo=BASE2$file)

write.xlsx(Base_LDA2, "Base_LDA_energia.xlsx")


# QDA
QDA2 <- predict(qda.model2, newdata=BASE2[14:219])

# Clasificación
Labels_QDA2 <- ifelse(QDA2$class==2,"Alto",ifelse(QDA2$class==1,"Medio","Bajo"))
BaseQDA2 <- data.frame(Grupo = QDA2$class, Grupo_Etiqueta = Labels_QDA2, id_Pista=BASE2$track_id,
                      Archivo=BASE2$file)

write.xlsx(BaseQDA2, "BaseQDA_energia.xlsx")

# MULTILOGIT
MLOGIT2 <- predict(model2_mlogit, newdata=BASE2[14:219])
# Clasificación
Labels_MLOGIT2 <- ifelse(MLOGIT2==2,"Alto",ifelse(MLOGIT2==1,"Medio","Bajo"))
BaseMLOGIT2  <- data.frame(Grupo = MLOGIT2, Grupo_Etiqueta = Labels_MLOGIT2, id_Pista=BASE2$track_id,
                          Archivo=BASE2$file)

write.xlsx(BaseMLOGIT2, "BaseMLOGIT_energia.xlsx")


# RED NEURONAL 2

RED3_2 <- compute(red1_energy, BASE2_escalada)
RED3__2 <- RED3_2$net.result
head(RED3__2)
RED3_Etiquetas_2 <- max.col(RED3__2)
Labels_red3_2 <- ifelse(RED3_Etiquetas_2==3,"Alto",ifelse(RED3_Etiquetas_2==2,"Medio","Bajo"))
Base_RED3_2<- data.frame(Grupo = RED3_Etiquetas_2, Grupo_Etiqueta = Labels_red3_2, id_Pista=BASE2$track_id,
                       Archivo=BASE2$file)
write.xlsx(Base_RED3_2, "Base_RED3_energy.xlsx")

# RED NEURONAL 5

RED5_2 <- compute(red4_energy, BASE2_escalada)
RED5__2 <- RED5_2$net.result
head(RED5__2)
RED5_Etiquetas_2 <- max.col(RED5__2)
Labels_red5_2 <- ifelse(RED5_Etiquetas_2==3,"Alto",ifelse(RED5_Etiquetas_2==2,"Medio","Bajo"))
Base_RED5_2 <- data.frame(Grupo = RED5_Etiquetas_2, Grupo_Etiqueta = Labels_red5_2, id_Pista=BASE2$track_id,
                         Archivo=BASE2$file)
#   2           Alto    97009 fma_large/097/097009.mp3
write.xlsx(Base_RED5_2, "Base_RED5_energy.xlsx")

#########################
# FIN
#########################