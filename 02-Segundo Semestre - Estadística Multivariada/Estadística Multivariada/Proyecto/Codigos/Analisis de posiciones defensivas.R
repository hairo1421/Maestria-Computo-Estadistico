remove(list=ls())
library(tidyverse)
setwd("C:/Users/angel/Desktop/MCE_CIMAT/Segundo_Semestre/Estadistica_Multivariada/ProyectoFinal/basesFinales")
# Lectura de tablas base
dat_bateo<- read.csv("dat_bateo_RL.csv")
dat_campo<- read.csv("dat_campo.csv")
dat_picheo<- read.csv("dat_picheo_RL.csv")

# Quitar filas dummy
dat_campo<- dat_campo[-which(is.na(dat_campo$Id)),]

## Obtencion de las posiciones mas frecuentes de cada
## jugador
pos<-names(dat_campo)[32:40]
list<- dat_campo[,32:40] %>% apply(1,which.max)
posiciones_campo<- pos[list]
dat_campo<- cbind(dat_campo,posiciones_campo)
names(dat_campo)[45]<- "Posicion"
plot(table(dat_campo$Posicion),main="Posiciones defensivas",ylab="Frecuencia",xlab="Posicion")

#####
# Cosas para graficas
library(car)
library(plotly)
axe1 <- list(title = "PC1",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
axe2 <- list(title = "PC2",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
axe3 <- list(title = "PC3",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
scene3<- list(xaxis=axe1,yaxis=axe2,zaxis=axe3)
library("htmlwidgets")
########

### Analisis 2018 (sin contemplar Rtot y Rtot.yr)
var<- names(dat_campo)[c(4,7:16,19:22,45)]
var
base_campo2018<- dat_campo[which(dat_campo$year=="2018"),var]
table(base_campo2018$Posicion)
base_campo2018<- base_campo2018[complete.cases(base_campo2018),]
plot(table(base_campo2018$Posicion),main="Posiciones defensivas",ylab="Frecuencia",xlab="Posicion")
varPca<- var[-16]
Campo_valores <- prcomp(base_campo2018[,varPca], scale=T)
scores<- as.data.frame(Campo_valores$x)
attach(scores)
pl<-plot_ly(scores,x=PC1,y=PC2,z=PC3,
            mode='text',text=base_campo2018$Posicion,color=base_campo2018$Posicion)%>%
  layout(title="Jugadores en PCA 3D",
         scene = scene3
  )
pl


which(scores$PC3>15)
base_campo2018$Rdrs.yr[341]
dat_campo$Rdrs.yr[404]
jug<-dat_campo[which(dat_campo$Id=="freemmi01"),]
summary(base_campo2018$Rdrs.yr)
hist(dat_campo$Rdrs.yr,breaks = 100,xlab="Rdrs.yr",main="Distribucion de Rdrs.yr")
saveWidget(as_widget(pl), "PosOutlier.html")
# Posible error en el registro de freemmi01, sera removido
# Remover
base_campo2018<-base_campo2018[-341,]
Campo_valores <- prcomp(base_campo2018[,varPca], scale=T)
scores<- as.data.frame(Campo_valores$x)
attach(scores)
pl<-plot_ly(scores,x=PC1,y=PC2,z=PC3,
            mode='text',text=base_campo2018$Posicion,color=base_campo2018$Posicion)%>%
  layout(title="Jugadores en PCA 3D",
         scene = scene3
  )
pl
saveWidget(as_widget(pl), "Posiciones.html")

### Ahora sin pitchers, para incluir Rdrs Rdrs.yr
var<- names(dat_campo)[c(3,4,7:16,17,18,19:22,45)]
var
base_campo2018<- dat_campo[which(dat_campo$year=="2018"),var]
table(base_campo2018$Posicion)
## Quitar pitchers
base_campo2018<- base_campo2018[-which(base_campo2018$Posicion=="P"),]
table(base_campo2018$Posicion)
base_campo2018<- base_campo2018[complete.cases(base_campo2018),]
table(base_campo2018$Posicion)
varPca<- var[-c(1,19)]
base_campo2018[which(base_campo2018$Id=="freemmi01"),]
# Remover outlier
base_campo2018<- base_campo2018[-which(base_campo2018$Id=="freemmi01"),]

Campo_valores <- prcomp(base_campo2018[,varPca], scale=T)
scores<- as.data.frame(Campo_valores$x)
attach(scores)
pl<-plot_ly(scores,x=PC1,y=PC2,z=PC3,
            mode='text',text=base_campo2018$Posicion,color=base_campo2018$Posicion)%>%
  layout(title="Jugadores en PCA 3D",
         scene = scene3
  )
pl
saveWidget(as_widget(pl), "PosicionesSinPitcher.html")
# No se tuvo mucha ganancia
## Se usaran las variables 
var<- names(dat_campo)[c(3,4,7:16,19:22,45)]
var
varPca<- var[-c(1,17)]
varPca
variables<- varPca
variables
### Aprendizaje Supervisado
### Crear matriz
set.seed(0)
library(caret)
datos<- dat_campo[,variables]
datos<- cbind(datos,clase=dat_campo$Posicion)
datos<- datos[complete.cases(datos),]
Clasificador1<- datos
Clasificador1 %>% names
# Division mediante sample_frac
Clasificador1_train <- sample_frac(Clasificador1, .8)
Clasificador1_test <- anti_join(Clasificador1,Clasificador1_train)
Clasificador1_train %>% dim # 2784  16
Clasificador1_test %>% dim #691  16
table(Clasificador1$clase)
100*table(Clasificador1_test$clase)/sum(table(Clasificador1_test$clase))
100*table(Clasificador1_train$clase)/sum(table(Clasificador1_train$clase))
# Division mediante createDataPartition
train <- createDataPartition(y = datos$clase, p = 0.8, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]
100*table(datos_test$clase)/sum(table(datos_test$clase))
100*table(datos_train$clase)/sum(table(datos_train$clase))
datos_train %>% dim # 2784  16
datos_test %>% dim #691  16

# Mantener la de createDataPartition
Clasificador1_test<-datos_test
Clasificador1_train<- datos_train

# Random Forest
# PARALELIZACIÓN DE PROCESO
#===============================================================================
# RANDOM FOREST
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 5
repeticiones<-3
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador1_test)-1),
                               min.node.size = c(2, 5,  10, 30),
                               splitrule = "gini")
set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "cv", number = particiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)

modelo_rf1 <- train(clase ~ ., data = Clasificador1_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)
modelo_rf1
modelo_rf1$finalModel
SeleccionVariables1 <- modelo_rf1$finalModel$variable.importance %>% as.data.frame
SeleccionVariables1 <- SeleccionVariables1 %>% rownames_to_column()
colnames(SeleccionVariables1) <- c("variable","importance")
plot1<- ggplot(SeleccionVariables1, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables de Campo")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
plot1
# Revision del expected error out of bag
modelo_rf1$finalModel
pred<- predict(modelo_rf1,newdata = Clasificador1_test)
pred
## Revision de accuracy conjunto prueba
confusionMatrix(data = pred, reference = Clasificador1_test$clase)

#### Hacer lo mismo con variables importantes dadas por el random forest
variables<- c("CG","RF.G","RF.9","DP","PO","A")
variables


### Aprendizaje Supervisado
### Crear matriz 
library(caret)
set.seed(0)
datos<- dat_campo[,variables]
datos<- cbind(datos,clase=dat_campo$Posicion)
datos<- datos[complete.cases(datos),]
Clasificador1<- datos
Clasificador1 %>% names
# Division mediante sample_frac
Clasificador1_train <- sample_frac(Clasificador1, .8)
Clasificador1_test <- anti_join(Clasificador1,Clasificador1_train)
Clasificador1_train %>% dim # 2784  16
Clasificador1_test %>% dim #691  16
table(Clasificador1$clase)
100*table(Clasificador1_test$clase)/sum(table(Clasificador1_test$clase))
100*table(Clasificador1_train$clase)/sum(table(Clasificador1_train$clase))
# Division mediante createDataPartition
train <- createDataPartition(y = datos$clase, p = 0.8, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]
100*table(datos_test$clase)/sum(table(datos_test$clase))
100*table(datos_train$clase)/sum(table(datos_train$clase))
datos_train %>% dim #3258   6
datos_test %>% dim #811  6

# Mantener la de createDataPartition
Clasificador1_test<-datos_test
Clasificador1_train<- datos_train

# Random Forest
# PARALELIZACIÓN DE PROCESO
#===============================================================================
# RANDOM FOREST
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 5
repeticiones<-3
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador1_test)-1),
                               min.node.size = c(2, 5,  10, 30),
                               splitrule = "gini")
set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "cv", number = particiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)

modelo_rf2 <- train(clase ~ ., data = Clasificador1_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)
modelo_rf2
modelo_rf2$finalModel
SeleccionVariables1 <- modelo_rf2$finalModel$variable.importance %>% as.data.frame
SeleccionVariables1 <- SeleccionVariables1 %>% rownames_to_column()
colnames(SeleccionVariables1) <- c("variable","importance")
plot1<- ggplot(SeleccionVariables1, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables de Campo")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
plot1
# Revision del expected error out of bag
modelo_rf1$finalModel
pred<- predict(modelo_rf2,newdata = Clasificador1_test)
pred
## Revision de accuracy conjunto prueba
confusionMatrix(data = pred, reference = Clasificador1_test$clase)


#Ajuste mediante nnet
modelo_nnet <- train(clase ~ ., data = Clasificador1_train,
                     method = "nnet")
pred<- predict(modelo_nnet,newdata = Clasificador1_test)
confusionMatrix(data = pred, reference = Clasificador1_test$clase)
# Ajuste mediante svm radial
modelo_svmr <- train(clase ~ ., data = Clasificador1_train,
                     method = "svmRadial")
pred<- predict(modelo_svmr,newdata = Clasificador1_test)
confusionMatrix(data = pred, reference = Clasificador1_test$clase)

# Visualizacion de variables
## CG
ggplot(data = datos, aes(x = clase, y = CG, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre CG")+xlab("Posicion")
theme_bw()

## RF.G
ggplot(data = datos, aes(x = clase, y = RF.G, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RF.G")+xlab("Posicion")
theme_bw()

## RF.9
ggplot(data = datos, aes(x = clase, y = RF.9, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RF.9")+xlab("Posicion")
theme_bw()

## DP
ggplot(data = datos, aes(x = clase, y = DP, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre DP")+xlab("Posicion")
theme_bw()

## PO
ggplot(data = datos, aes(x = clase, y = PO, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre PO")+xlab("Posicion")
theme_bw()

## A
ggplot(data = datos, aes(x = clase, y = A, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre A")+xlab("Posicion")
theme_bw()






##################################################################################################
##################################################################################################
##################################################################################################
###### Hacer lo mismo, pero ahora para variables de bateo

## Obtener la posicion defensiva de los jugadores de bateo
remove(list=ls())
library(tidyverse)
setwd("C:/Users/angel/Desktop/MCE_CIMAT/Segundo_Semestre/Estadistica_Multivariada/ProyectoFinal/basesFinales")
# Lectura de tablas base
dat_bateo<- read.csv("dat_bateo_RL.csv")
dat_campo<- read.csv("dat_campo.csv")
dat_picheo<- read.csv("dat_picheo_RL.csv")

# Quitar filas dummy
dat_campo<- dat_campo[-which(is.na(dat_campo$Id)),]
dat_bateo<- dat_bateo[-which(is.na(dat_bateo$Id)),]

## Obtencion de las posiciones mas frecuentes de cada
## jugador
pos<-names(dat_campo)[32:40]
list<- dat_campo[,32:40] %>% apply(1,which.max)
posiciones_campo<- pos[list]
dat_campo<- cbind(dat_campo,posiciones_campo)
names(dat_campo)[45]<- "Posicion"


## Agregar posicion a dat_bateo
idcampo<- match(dat_bateo$Id,dat_campo$Id)
dummy<- dat_bateo[which(is.na(idcampo)),]
posiciones_campo<- dat_campo$Posicion[idcampo]
dat_bateo<- cbind(dat_bateo,Posicion=posiciones_campo)

#####
# Cosas para graficas
library(car)
library(plotly)
axe1 <- list(title = "PC1",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
axe2 <- list(title = "PC2",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
axe3 <- list(title = "PC3",zeroline = TRUE,showline = TRUE,showticklabels = TRUE,showgrid = TRUE)
scene3<- list(xaxis=axe1,yaxis=axe2,zaxis=axe3)
library("htmlwidgets")
########

## Explorar 2018
var<- names(dat_bateo)[c(4,7:30,74)]
var
base_bateo2018<- dat_bateo[which(dat_bateo$year=="2018"),var]
table(base_bateo2018$Posicion)
base_bateo2018<- base_bateo2018[complete.cases(base_bateo2018),]
table(base_bateo2018$Posicion)
varPca<- var[-26]
bateo_valores <- prcomp(base_bateo2018[,varPca], scale=T)
scores<- as.data.frame(bateo_valores$x)
attach(scores)
pl<-plot_ly(scores,x=PC1,y=PC2,z=PC3,
            mode='text',text=base_bateo2018$Posicion,color=base_bateo2018$Posicion)%>%
  layout(title="Jugadores en PCA 3D",
         scene = scene3
  )
pl
saveWidget(as_widget(pl), "posBateoStandar.html")

### Supervisado
variables<- varPca
variables
### Aprendizaje Supervisado
### Crear matriz 
set.seed(0)
library(caret)
datos<- dat_bateo[,variables]
datos<- cbind(datos,clase=dat_bateo$Posicion)
datos<- datos[complete.cases(datos),]
Clasificador1<- datos
Clasificador1 %>% names
# Division mediante createDataPartition
train <- createDataPartition(y = Clasificador1$clase, p = 0.8, list = FALSE, times = 1)
Clasificador1_train <- Clasificador1[train, ]
Clasificador1_test  <- Clasificador1[-train, ]
Clasificador1_train %>% dim # 2313  26
Clasificador1_test %>% dim #574  26
100*table(Clasificador1_test$clase)/sum(table(Clasificador1_test$clase))
100*table(Clasificador1_train$clase)/sum(table(Clasificador1_train$clase))
# Random Forest
# PARALELIZACIÓN DE PROCESO
#===============================================================================
# RANDOM FOREST
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 5
repeticiones<-3
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador1_test)-1),
                               min.node.size = c(2, 5,  10, 30),
                               splitrule = "gini")
set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "cv", number = particiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)

modelo_rf1 <- train(clase ~ ., data = Clasificador1_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)
modelo_rf1
modelo_rf1$finalModel
SeleccionVariables1 <- modelo_rf1$finalModel$variable.importance %>% as.data.frame
SeleccionVariables1 <- SeleccionVariables1 %>% rownames_to_column()
colnames(SeleccionVariables1) <- c("variable","importance")
plot1<- ggplot(SeleccionVariables1, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo Basicas")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
plot1
# Revision del expected error out of bag
modelo_rf1$finalModel
pred<- predict(modelo_rf1,newdata = Clasificador1_test)
pred
## Revision de accuracy conjunto prueba
confusionMatrix(data = pred, reference = Clasificador1_test$clase)


#### Prediccion solo con variables importantes
### Supervisado
variables<- c("TB","BB","R","SB","SH")
### Aprendizaje Supervisado
### Crear matriz 
set.seed(0)
library(caret)
datos<- dat_bateo[,variables]
datos<- cbind(datos,clase=dat_bateo$Posicion)
datos<- datos[complete.cases(datos),]
Clasificador1<- datos
Clasificador1 %>% names
# Division mediante createDataPartition
train <- createDataPartition(y = Clasificador1$clase, p = 0.8, list = FALSE, times = 1)
Clasificador1_train <- Clasificador1[train, ]
Clasificador1_test  <- Clasificador1[-train, ]
Clasificador1_train %>% dim # 2995  6
Clasificador1_test %>% dim #744  6
100*table(Clasificador1_test$clase)/sum(table(Clasificador1_test$clase))
100*table(Clasificador1_train$clase)/sum(table(Clasificador1_train$clase))
# Random Forest
# PARALELIZACIÓN DE PROCESO
#===============================================================================
# RANDOM FOREST
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 5
repeticiones<-3
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador1_test)-1),
                               min.node.size = c(2, 5,  10, 30),
                               splitrule = "gini")
set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "cv", number = particiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)

modelo_rf1 <- train(clase ~ ., data = Clasificador1_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)
modelo_rf1
modelo_rf1$finalModel
SeleccionVariables1 <- modelo_rf1$finalModel$variable.importance %>% as.data.frame
SeleccionVariables1 <- SeleccionVariables1 %>% rownames_to_column()
colnames(SeleccionVariables1) <- c("variable","importance")
plot1<- ggplot(SeleccionVariables1, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo Basicas")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
plot1
# Revision del expected error out of bag
modelo_rf1$finalModel
pred<- predict(modelo_rf1,newdata = Clasificador1_test)
pred
## Revision de accuracy conjunto prueba
confusionMatrix(data = pred, reference = Clasificador1_test$clase)
# Ajuste mediante nnet
modelo_nnet <- train(clase ~ ., data = Clasificador1_train,
                    method = "nnet")
pred<- predict(modelo_nnet,newdata = Clasificador1_test)
confusionMatrix(data = pred, reference = Clasificador1_test$clase)
# Ajuste mediante svm radial
modelo_svmr <- train(clase ~ ., data = Clasificador1_train,
                     method = "svmRadial")
pred<- predict(modelo_svmr,newdata = Clasificador1_test)
confusionMatrix(data = pred, reference = Clasificador1_test$clase)


####### Visualizacion de densidades
library(ggpubr)
## R
ggplot(data = datos, aes(x = clase, y = R, color = clase)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre R")+xlab("Posicion")
  theme_bw()
## TB
  ggplot(data = datos, aes(x = clase, y = TB, color = clase)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre TB")+xlab("Posicion")
  theme_bw()
## BB
ggplot(data = datos, aes(x = clase, y = BB, color = clase)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre BB")+xlab("Posicion")
  theme_bw()
  
  ## SB
  ggplot(data = datos, aes(x = clase, y = SB, color = clase)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre SB")+xlab("Posicion")
  theme_bw()
  
  ## SH
  ggplot(data = datos, aes(x = clase, y = SH, color = clase)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre SH")+xlab("Posicion")
  theme_bw()

  
