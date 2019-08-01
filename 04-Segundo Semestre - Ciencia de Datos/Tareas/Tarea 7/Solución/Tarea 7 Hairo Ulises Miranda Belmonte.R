
# Tarea 7 Ciencia de Datos
# 22/05/2019


####################################
# EJERCICIO 1
####################################
# Librerías
library("tidyverse")
library("magrittr")
library("rpart")
library("adabag")
############################
# Primera Parte
############################
# Generación de datos de acuerdo al paper de Zhu J(2006)
set.seed(5)
u <- runif(100)
e <- rnorm(100)
j <- 1:21
v1 <-  rep(0,21)
v2 <-  rep(0,21)
v3 <-  rep(0,21)
i <- 1
for(i in 1:21){
v1[i] = max(6 - abs(j[i] - 11), 0)
v2[i] = v1[i]-4
v3[i] = v1[i] + 4
}
c1 <- matrix(0,100,21)
for(i in 1:21){
c1[,i] <-u*v1[i]+(1-u)*v2[i]+e[i]
}
c2 <- matrix(0,100,21)
for(i in 1:21){
  c2[,i] <-u*v1[i]+(1-u)*v3[i]+e[i]
}
c3 <- matrix(0,100,21)
for(i in 1:21){
  c3[,i] <-u*v2[i]+(1-u)*v3[i]+e[i]
}

y1 <- rep(1,100)
y2 <- rep(2,100)
y3 <- rep(3,100)

uno <- cbind(c1,y1)
dos <- cbind(c2,y2)
tres <- cbind(c3,y3)
base <- rbind(uno,dos,tres)
a <- rep(0,21)
for(i in 1:21){
  a[i] <- paste("x",i)
}

colnames(base) <- c(a,"y")
base <- base %>% as.data.frame()
# Base de entrenamiento y prueba
base_train <- sample_frac(base,size = .8,replace = F)
base_test <- anti_join(base, base_train)

# FUNCION ADABOOST SAMME

# INPUT <- BASE DE ENTRENAMIENTO, BASE DE PRUEBA O DE ENTRENAMIENTO
# EN ESTE CASO SOLO SE ENFOCA EN EVALUAR EL ERROR DE ENTRENAMIENTO
# SE DEBE MANDAR DOS BESES LA BASE DE ENTRENAMIENTO
# m INDICA EL NÚMERO DE ITERACIONES.

# OUTPUT <- ERROR DE ENTRENAMIENTO


ADABOOSTSAMME <- function(base_train, m){
  # Variables auxiliares
  n <- base_train$y %>%  length()
  w <- rep(1/n, n)  # inicialización de pesos
  #n2 <- base_test[,1] %>% length()
  yhat <- matrix(0, n, m)
  err <- rep(0,m)
  a <- rep(0,m)
  h <- vector('list',m)
  #base_test$y <- base_test$y %>% as.factor
  base_train$y <- base_train$y %>% as.factor
  K <- nlevels(base_train$y)
  momento <- 0
 
 
  #Algoritmo
  for(i in 1:m){
    h[[i]] <- rpart(y~.,data=base_train,method='class',
                    minsplit=5,weights=w, cp=0.01) # árboles 
    momento <- predict(h[[i]], newdata = base_train, type = "class")
    yhat[,i] <- momento %>% as.character %>% as.numeric
    err[i] <- sum(w*(yhat[,i]!=as.numeric(as.character(base_train$y))))/sum(w)
    a[i] <- log((1-err[i])/err[i]) + log(K-1)
    # actualización pesos
    w <- w*exp(a[i]*(yhat[,i]!=as.numeric(as.character(base_train$y))))
    w <- w/sum(w)
  }# end for

  classfinal <- array(0, c(n,nlevels(base_train$y)))
  for (i in 1:nlevels(base_train$y)){
    classfinal[,i] <- matrix(as.numeric(yhat==levels(base_train$y)[i]),nrow=n)%*%as.vector(a)
  }
 
  H <-apply(classfinal,1,which.max)

  cm <- table(as.numeric(as.character(base_train$y)), H)
  Acurracy_train <- sum(diag(cm))/sum(cm)
  error_train <- 1-Acurracy_train
  return(error_train)
} # end ADABOOSTSAMME

# Complejidad del clasificador, error de entrenamiento
# conforme crece el número de iteraciones (o árboles).

x <- seq(0,20,1)
x <-x[-1]
errorEntrenamiento <- sapply(x, function(x) ADABOOSTSAMME(base_train, x))

# Visualizaición
x11()
errorEntrenamiento %>% as.data.frame %>% 
  ggplot() + aes(x=x, y=errorEntrenamiento) + geom_point() +
  geom_line(aes(x=x, y=errorEntrenamiento)) +
  labs(title = "AdaBoost", subtitle = "SAMME", x="Iteraciones",
       y="Error clasificación")

# Contrastar contra Adaboost M1 Clasificador multiclase,
# Método Breiman (M1) trata como problema binario
ADABOOSTM1 <- function(base_train, m){
  result <- boosting(y~., data=base_train, boos=F, 
                     mfinal=m, coeflearn="Breiman")
  y_pred <- predict.boosting(result, newdata = base_train[,-22])
  cm <- table(base_train$y, y_pred$class)
  return(1-sum(diag(cm)) / sum(cm) )
}

# Error de entrenamiento
base_train$y <- base_train$y %>% as.factor
x <- seq(0,20,1)
x <-x[-1]
errorEntrenamiento2 <- sapply(x, function(x) ADABOOSTM1(base_train, x))

# Visualización
x11()

errorEntrenamiento2 %>% as.data.frame %>% 
  ggplot() + aes(x=x, y=errorEntrenamiento2) + geom_point() +
  geom_line(aes(x=x, y=errorEntrenamiento2)) +
  labs(title = "AdaBoost", subtitle = "M1", x="Iteraciones",
       y="Error clasificación")

# Visualización
x11()
errorEntrenamiento %>% as.data.frame %>% 
  ggplot() +  geom_point(aes(x=x, y=errorEntrenamiento, col="SAMME")) +
  geom_line(aes(x=x, y=errorEntrenamiento, col="SAMME")) +
  labs(title = "AdaBoost", subtitle = "", x="Iteraciones",
       y="Error clasificación") +
  geom_point(aes(x=x, y=errorEntrenamiento2,col="M1")) +
  geom_line(aes(x=x, y=errorEntrenamiento2,col="M1"))


############################
# Segunda Parte
############################
getwd()
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7")
datosNetTest <- read.table("netTest.data", header = T)
datosSat<- read.table("sat.trn", header = T)
ADABOOSTSAMME <- function(base_train, m){
  # Variables auxiliares
  n <- base_train$y %>%  length()
  w <- rep(1/n, n)  # inicialización de pesos
  #n2 <- base_test[,1] %>% length()
  yhat <- matrix(0, n, m)
  err <- rep(0,m)
  a <- rep(0,m)
  h <- vector('list',m)
  #base_test$y <- base_test$y %>% as.factor
  base_train$y <- base_train$y %>% as.factor
  K <- nlevels(base_train$y)
  momento <- 0
  
  # Se modifica el algoritmo solo para que reciva casos con K=2
  # las etiquetas
  
  #Algoritmo
  for(i in 1:m){
    h[[i]] <- rpart(y~.,data=base_train,method='class',
                    minsplit=5,weights=w, cp=0.05) # árboles 
    momento <- predict(h[[i]], newdata = base_train, type = "class")
    yhat[,i] <- momento %>% as.character %>% as.numeric
    err[i] <- sum(w*(yhat[,i]!=as.numeric(as.character(base_train$y))))/sum(w)
    a[i] <- log((1-err[i])/err[i]) + log(K-1)
    # actualización pesos
    w <- w*exp(a[i]*(yhat[,i]!=as.numeric(as.character(base_train$y))))
    w <- w/sum(w)
  }# end for
  
  classfinal <- array(0, c(n,nlevels(base_train$y)))
  for (i in 1:nlevels(base_train$y)){
    classfinal[,i] <- matrix(as.numeric(yhat==levels(base_train$y)[i]),nrow=n)%*%as.vector(a)
  }
  
  H <-apply(classfinal,1,which.max)
  ###############################
  # Esta es la modificación
  H[H==1] <- 0
  H[H==2] <- 1
  ###############################
  cm <- table(as.numeric(as.character(base_train$y)), H)
  Acurracy_train <- sum(diag(cm))/sum(cm)
  error_train <- 1-Acurracy_train
  return(error_train)
} # end ADABOOSTSAMME


datosNetTest$y <- datosNetTest$y %>% as.factor

# Función que evalua distintos métodos multiclase de AdaBoost
Evaluacion <- function(m, datosNetTest){

  errorSamme_R <- NULL
  errorM1 <- NULL
  errorSAMME <- NULL
  

    errorSamme_R<- boosting(y~., data=datosNetTest, boos=F, 
                                  mfinal=m, coeflearn="Zhu",cp=0.05)
    
    cm1 <- table(datosNetTest$y, errorSamme_R$class)
    error1 <- 1-sum(diag(cm1))/ sum(cm1)
    # dos
    errorM1 <- boosting(y~., data=datosNetTest, boos=F, 
                             mfinal=m, coeflearn="Breiman",cp=0.05)
    
    cm2 <- table(datosNetTest$y, errorM1$class)
    error2 <- 1-sum(diag(cm2))/ sum(cm2)
    # tres 
    errorSAMME <- ADABOOSTSAMME(datosNetTest,m)
    
    return(list(e1=error1,e2=error2,e3=errorSAMME))
}

# Visualización
x <- seq(1,10,1)
ResultadosNetTest <-sapply(x, function(x) Evaluacion(x, datosNetTest))

ErrorSAMMA_R <-ResultadosNetTest[1,] %>% unlist
ErrorM1 <-ResultadosNetTest[2,] %>% unlist
ErrorSAMMA <-ResultadosNetTest[3,] %>% unlist
  x11()
ErrorSAMMA_R %>% as.data.frame %>% 
  ggplot() +  geom_point(aes(x=x, y=ErrorSAMMA_R, col="SAMME_R")) +
  geom_line(aes(x=x, y=ErrorSAMMA_R, col="SAMME_R")) +
  labs(title = "AdaBoost", subtitle = "", x="Iteraciones",
       y="Error clasificación") +
  geom_point(aes(x=x, y=ErrorM1,col="M1")) +
  geom_line(aes(x=x, y=ErrorM1,col="M1")) +
  geom_point(aes(x=x, y=ErrorSAMMA,col="SAMME_Propio")) +
  geom_line(aes(x=x, y=ErrorSAMMA,col="SAMME_Propio"))




##################################
# Ejercicio 2
##################################

# Librerías
library("tidyverse")
library("caret")
library("ggplot2")
library("magrittr")

# Introducir ruta donde se almacenan archivos
getwd()
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7")

# Datos
trainX <- read.table("mnistXtrain.dat", header = T)
testX <- read.table("mnistXtest.dat", header = T)
trainY <- read.table("mnistYtrain.dat", header = T)
testY <- read.table("mnistYtest.dat", header = T)
# Uniendo datos
X <- rbind(trainX, testX)
Y <- rbind(trainY, testY)
base <- cbind(Y,X)
# 10% de las observaciones
base <- sample_frac(base, size=.10,  replace=F )
# de ese 10% el 80% para entrenamiento
entrenamiento <- sample_frac(base, size=.8,  replace=F )
# 20% de prueba
prueba <-  anti_join(base, entrenamiento)
# Nombre variable respuesta
names(entrenamiento)[1] <- "y"
names(prueba)[1] <- "y"
# Número de particiones para K-folds Cross Validation
folds <- createFolds(entrenamiento$y, k = 5)

#############################
#     SVM
#############################

# Librerías
library("e1071")

# Parámetros
costo <- c(1,20,100)
GAMMA <-seq(0.01,.1,.01)
errores<- matrix(0L,3,10)

#Ajuste del clasificador
SVM <- function(S, gam, cp){
  cvKernelSVM <- lapply(S, function(x){
    training_fold <- entrenamiento[-x, ]
    #test_fold <- entrenamiento[x, ]
    clasificador <- svm(y ~ .,
                        data = training_fold, 
                        type = 'C-classification', 
                        kernel = 'radial',
                        gamma=gam,
                        cost=cp)
    y_pred_test <- predict(clasificador, newdata = prueba[,-1])
    cm_test <- table(prueba$y, y_pred_test)
    return(sum(diag(cm_test)) / sum(cm_test))
  })
  precisionKernelSVM <- mean(as.numeric(cvKernelSVM))
  errorKernelSVM <- 1-precisionKernelSVM  
  return(errorKernelSVM)
  
}

# CV plot  
for(i in 1:length(costo)){
  for(j in 1:length(GAMMA)){
    errores[i,j] <-SVM(folds, GAMMA[j], costo[i])
  }
}

# Visualización
p1 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point() + geom_line() +
  labs(title="SVM", subtitle = "costo=1", x="gamma", y="CV/5 Misclassification Rate"  )
p2 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point() + geom_line() +
  labs(title="SVM", subtitle = "costo=20", x="gamma", y="CV/5 Misclassification Rate"  )
p3 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point() + geom_line() +
  labs(title="SVM", subtitle = "costo=100", x="gamma", y="CV/5 Misclassification Rate"  )


p4 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point(color="red") + geom_line(color="red") +
  labs(title="SVM", subtitle = "", x="gamma", y="CV/5 Misclassification Rate" , caption = "Rojo: costo de 1, Azul: costo de 10, Negro:costo de 100" )+
  geom_point(aes(x=GAMMA, y=errores[2,]),color="blue" ) + geom_line(aes(x=GAMMA, y=errores[2,]),color="blue" ) +
  geom_point(x=GAMMA, y=errores[3,]) + geom_line(x=GAMMA, y=errores[3,]) 

x11()
gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)



# MODELO SELECCIONADO
# PÁRAMETRO KERNEL GAUSSIANO DE 0.03
# PÁRAMETRO DE COSTO POR CLASIFICAR MAL DE 1
datosPruebaSVM <- prueba[,-1] # aquí van los datos de prueba
cvKernelSVM <- lapply(folds, function(x){
  training_fold <- entrenamiento[-x, ]
  clasificador <- svm(y ~ .,
                      data = training_fold, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      gamma=0.03,
                      cost=1)
  y_pred_test <- predict(clasificador, newdata = datosPruebaSVM)
  cm_test <- table(prueba$y, y_pred_test)
  return(sum(diag(cm_test)) / sum(cm_test))
})

# Precisión y Error de clasificación
precisioSVM <- mean(as.numeric(cvKernelSVM)) # 0.916875
errorSVM <- 1-mean(as.numeric(cvKernelSVM)) #  0.083125

#####################
# Árbol de Decisión
#####################

# Parámetros
costo2 <- c(-1, -0.5,0,0.5)
deep <- seq(2,30,1)
errores2<- matrix(0L,length(costo2),length(deep))

# Ajuste del clasificador
Arbol <- function(S, costo2, deep){
  cvDecisionTree <- lapply(folds, function(x){
    training_fold <- entrenamiento[-x, ]
    #test_fold <- prueba[x, ]
    clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                          cp=costo2, xval=0, maxdepth = deep )
    
    y_pred <- predict(clasificadorArbol, newdata = prueba[,-1], type = "class")
    cm <- table(prueba$y, y_pred)
    precision <- sum(diag(cm)) / sum(cm)
    #precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
    return(precision)
  })  
  precisionKernelArbol<- mean(as.numeric(cvDecisionTree))
  errorKernelArbol <- 1-precisionKernelArbol  
  return(errorKernelArbol)
}



# CV plot
for(i in 1:length(costo2)){
  for(j in 1: length(deep)){
    errores2[i,j] <- Arbol(folds, costo2[i], deep[j])
  }
  }

# visualización
ap1 <-  ggplot() +
  aes(x=deep, y=errores2[1,]) + geom_point() + geom_line() +
  labs(title="Árbol de clasificación", subtitle = "cp=-1", x="árbol-size", y="CV/5 Misclassification Rate"  )
ap2 <- ggplot() +
  aes(x=deep, y=errores2[2,]) + geom_point() + geom_line() +
  labs(title="Árbol de clasificación", subtitle = "cp=-.5", x="árbol-size", y="CV/5 Misclassification Rate"  )

ap3 <- ggplot() +
  aes(x=deep, y=errores2[3,]) + geom_point() + geom_line() +
  labs(title="Árbol de clasificación", subtitle = "cp=0", x="árbol-size", y="CV/5 Misclassification Rate"  )



ap4 <- ggplot() +
  aes(x=deep, y=errores2[1,]) + geom_point(color="red") + geom_line(color="red") +
  labs(title="Árbol de clasificación", subtitle = "", x="árbol-size", y="CV/5 Misclassification Rate" , caption = "Rojo: costo de -1, Azul: costo de -0.5, Negro:costo de 0" )+
  geom_point(aes(x=deep, y=errores2[2,]),color="blue" ) + geom_line(aes(x=deep, y=errores2[2,]),color="blue" ) +
  geom_point(x=deep, y=errores2[3,]) + geom_line(x=deep, y=errores2[3,]) 

x11()
gridExtra::grid.arrange(ap1,ap2,ap3,ap4,nrow=2,ncol=2)


# MODELO SELECCIONADO ÁRBOL DE CLASIFICACIÓN
# número de partición máxima 6
# costo de impuridad (impurity) -1

datosPruebaArbol <- prueba[,-1]
cvDecisionTree <- lapply(folds, function(x){
  training_fold <- entrenamiento[-x, ]
  set.seed(2000)
  clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                             cp=-1, xval=0, maxdepth = 6 )
  
  y_pred <- predict(clasificadorArbol, newdata = datosPruebaArbol, type = "class")
  cm <- table(prueba$y, y_pred)
  precision <- sum(diag(cm)) / sum(cm)
  return(precision)
})  

# Precisión y Error clasificación
precisionArbol <- mean(as.numeric(cvDecisionTree)) # 0.635625
errorArbol <- 1-precisionArbol  # 0.364375


###########################
#       RANDOFOREST
###########################
set.seed(1649)

#Librerías 
library("randomForest")
library("caret")
library("rpart")
# Parámetros
deepRF <- c(10,200,500,800, 1000)
ncolumnas <- dim(entrenamiento)[2]
mtryRF <- c(.4*ncolumnas,.6*ncolumnas,ncolumnas)
errores3<- matrix(0L,length(mtryRF),length(deepRF))

# Se require que la variable respuesta sea factor
entrenamiento$y <- entrenamiento$y %>% as.factor 

# Ajuste del clasificador
RF <- function(S, mtryRF, deepRF){
  cvRandomForest <- lapply(folds, function(x){
    training_fold <- entrenamiento[-x, ]
    clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                   ntree = deepRF, mtry= mtryRF)
    y_pred <- predict(clasificadorRF, newdata = prueba)
    cm <- table(prueba$y, y_pred)
    precision <- sum(diag(cm)) / sum(cm)
    return(precision)
  })
  return(1- mean(as.numeric(cvRandomForest)))
}

# CV plot
for(i in 1:length(mtryRF)){
  for(j in 1: length(deepRF)){
    errores3[i,j] <- RF(folds, mtryRF[i], deepRF[j])
  }
}

# Visualización
RFap1 <-  ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=314", x="# de árbol", y="CV/5 Misclassification Rate"  )
RFap2 <- ggplot() +
  aes(x=deepRF, y=errores3[2,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=471", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap3 <-  ggplot() +
  aes(x=deepRF, y=errores3[3,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=785", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap4 <- ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point(color="red") + geom_line(color="red") +
  labs(title="Random Forest", subtitle = "", x="# de árbol", y="CV/5 Misclassification Rate" , caption = "Rojo: # variables 314  , Azul:  # variables 471 , Negro: # variables 785" )+
  geom_point(aes(x=deepRF, y=errores3[2,]),color="blue" ) + geom_line(aes(x=deepRF, y=errores3[2,]),color="blue" ) +
  geom_point(aes(x=deepRF, y=errores3[3,])) + geom_line(aes(x=deepRF, y=errores3[3,])) 

x11()
gridExtra::grid.arrange(RFap1,RFap2,RFap3,RFap4,nrow=2,ncol=2)

# MODELO SELECCIONADO RANDOM FOREST
# número de árboles 200
# número de variables 314

cvRandomForest <- lapply(folds, function(x){
  training_fold <- entrenamiento[-x, ]
  clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                 ntree = 200, mtry= 314)
  y_pred <- predict(clasificadorRF, newdata = prueba)
  cm <- table(prueba$y, y_pred)
  precision <- sum(diag(cm)) / sum(cm)
  return(precision)
})

# Precisión y Error en clasificación
AcurracyRF <-  mean(as.numeric(cvRandomForest)) # 0.881875
errorRF <- 1- mean(as.numeric(cvRandomForest)) # 0.118125

###########################
#       ADABOOST
###########################

# Librerías
library("ada")
library("adabag")

# Parámetros
deepADA <- c(2, 5, 10, 20, 50)
errores4<- matrix(0L,1, length(deepADA))

# Ajuste del clasificador

ADABOOst <- function(S, deepADA){
  cvAdaBoost <- lapply(folds, function(x){
    training_fold <- entrenamiento[-x, ]
    clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                             mfinal=deepADA, coeflearn="Zhu")
    y_pred <- predict.boosting(clasificadorADA, newdata = prueba[,-1])
    cm <- table(prueba$y, y_pred$class)
    return(sum(diag(cm)) / sum(cm))
  })
  return(1-mean(as.numeric(cvAdaBoost)))
}

# CV plot
  for(i in 1:length(deepADA)){
    errores4[1,i] <-  ADABOOst(folds, deepADA[i])
  }


# Visualización
ADap1 <-  ggplot() +
  aes(x=deepADA, y=errores4[1,]) + geom_point() + geom_line() +
  labs(title="AdaBoost", subtitle = "SAMME", x="Iteraciones", y="CV/5 Misclassification Rate"  )




# MODELO SELECCIONADO ADABOOST SAMME 
# Número de iteraciones 20

cvAdaBoost <- lapply(folds, function(x){
  training_fold <- entrenamiento[-x, ]
  clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                              mfinal=20, coeflearn="Zhu")
  y_pred <- predict.boosting(clasificadorADA, newdata = prueba[,-1])
  cm <- table(prueba$y, y_pred$class)
  return(sum(diag(cm)) / sum(cm))
})

# Precisión y Error en clasificación
AcurracyAdaBoost <-  mean(as.numeric(cvAdaBoost)) # 0.806875
errorAdaBoost <-1-mean(as.numeric(cvAdaBoost)) # 0.193125



###############################
# Red Neuronal
###############################
# Redes neuronales

library("nnet")
library("neuralnet")

train <- cbind(entrenamiento, class.ind(as.factor(entrenamiento$y)))
temp <- dim(train)[2]
# Se retira la variable respuesta que no es dummy
train <- train[,2:temp]

names(train) <- c(names(train)[1:784],"l0","l1","l2",
                  "l3","l4","l5",
                  "l6","l7","l8","l9")

# Formula del modelo
n <- names(train)
f <- as.formula(paste("l0 + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 ~", 
                      paste(n[!n %in% c("l0","l1","l2","l3","l4","l5","l6","l7","l8","l9")],
                            collapse = " + ")))

# Ajuste del modelo datos de entrenamiento
set.seed(61)

# 4 nodos ocultos
red1 <- neuralnet(f,
                  data = train,
                  hidden = 4,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  algorithm = "rprop+",
                  likelihood = T)


# 5 nodos ocultos
red2 <- neuralnet(f,
                  data = train,
                  hidden = 5,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  algorithm = "rprop+",
                  likelihood = T)


# 6 nodos ocultos
red3 <- neuralnet(f,
                  data = train,
                  hidden = 6,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  algorithm = "rprop+",
                  likelihood = T)

red4 <- neuralnet(f,
                  data = train,
                  hidden = 7,
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "minimal",
                  algorithm = "rprop+",
                  likelihood = T)



# Bar plot 
Class_NN_ICs <- tibble(Network = rep(c("RED1", "RED2", "RED3", "RED4"), each = 3), 
                       Metric = rep(c("AIC", "BIC", "ce Error * 100"), length.out = 12), 
                       Value = c(red1$result.matrix[4, 1], red1$result.matrix[5, 1], 100 * red1$result.matrix[1, 1],
                                 red2$result.matrix[4,1], red2$result.matrix[5, 1], 100 * red2$result.matrix[1, 1],
                                 red3$result.matrix[4,1], red3$result.matrix[5, 1], 100 * red3$result.matrix[1, 1], 
                                 red4$result.matrix[4, 1], red4$result.matrix[5, 1], 100 * red4$result.matrix[1,1]))

x11()
graph1 <- Class_NN_ICs %>% ggplot(aes(Network, Value, fill = Metric)) + geom_col(position = "dodge") + 
  ggtitle("AIC, BIC, y Cross-Entropy Error de clasificación ANNs", 
          "Note: ce Error es 100 veces su valor verdadero")



# cross entropy
red1$result.matrix[1,1] #149.4704 
red2$result.matrix[1,1] #32.0914 
red3$result.matrix[1,1] #33.50988
red4$result.matrix[1,1] #17.90724 



# Modelo seleccionado
test <- cbind(prueba, class.ind(as.factor(prueba$y)))
temp <- dim(test)[2]
test <- test[,2:temp]
# 7 nodos ocultos en una capa oculta
cvRN <- lapply(folds, function(x){
  training_fold <- train[-x, ]
  clasificadorRN <- neuralnet(f,
                    data = training_fold,
                    hidden = 7,
                    act.fct = "logistic",
                    linear.output = FALSE,
                    lifesign = "minimal",
                    algorithm = "rprop+")
  pr.nn<- compute(clasificadorRN, prueba[,-1]) 
  y_pred<- pr.nn$net.result
  # Y verdaderas
  original_values_test <- max.col(test[,785:794])
  # Y predichas
  pr.nn_2_test<- max.col(y_pred)
  # Tabla 
  cm <- table(original_values_test, pr.nn_2_test)
  return(sum(diag(cm)) / sum(cm))
})

# Precisión y Error en clasificación
AcurracyRN <-  mean(as.numeric(cvRN)) # 0.695
errorRN <-1-mean(as.numeric(cvRN)) # 0.305

#############################
# Modelo seleccionado
#############################

# IMPLEMENTACIÓN EN PCA 

pc <- princomp(entrenamiento[,-1])
## usaremos solo 2 PC
xpc.train <- cbind(pc$scores[,1],pc$scores[,2])
## proyecta los datos de prueba en los PC
test.pc <- predict(pc,prueba[,-1])
xpc.test <- cbind(test.pc[,1],test.pc[,2])

## grafica 2 scores
cols <- colors()[c(24,26,51,53,17,142,404,507,621,463)]
pchs <- as.character(0:9)

graph1 <- pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2]) +
  geom_point(cex=4, pch=pchs[entrenamiento[,1]+1],col=cols[entrenamiento[,1]+1]) + labs(title = "Digitos (0.9)",x="1st PC",y="2nd PC" )


ngrid <- 100
x1 <- seq(min(pc$scores[,1]),max(pc$scores[,1]),length=ngrid)
x2 <- seq(min(pc$scores[,2]),max(pc$scores[,2]),length=ngrid)
x <- expand.grid(x1,x2)

# Clasificador
clasificador <- svm(as.factor(entrenamiento[,1]) ~ .,
                    data = xpc.train, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    gamma=0.03,
                    cost=1)
# predicción
y.hat <- predict(clasificador,xpc.test,decision.values=FALSE,probability=FALSE)
## error
err.svm <- sum(prueba[,1]!=y.hat)/length(entrenamiento[,1])
err.svm
## predice el grid
colnames(x) <- NULL
pred.grid <- predict(clasificador,x,decision.values=FALSE)

# visualización
g1 <- ggplot(data=as.data.frame(x), aes(x=x[,1],y=x[,2])) +
   geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Digitos (0,9)", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 0.128125")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2]), cex=4,col=cols[entrenamiento[,1]+1],pch=pchs[entrenamiento[,1]+1]) +
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=4,col="red") 

g1 <- ggplot() + geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Digitos (0,9)", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 0.128125")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2]), cex=4,col=cols[entrenamiento[,1]+1],pch=pchs[entrenamiento[,1]+1]) +
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=4,col="red") 

x11()

# frontera de clasificación y datos de prueba
plot(x,col=cols[as.numeric(pred.grid)+1],pch=19,cex=2,main="Digitos (0,9)",
     xlab="1st PC",ylab="2nd PC",sub=paste("Error test:",round(err.svm,3)))
points(xpc.train,cex=1.5,col=cols[entrenamiento[,1]+1],pch=pchs[entrenamiento[,1]+1])
points(xpc.test,pch="+",cex=1,col="red")
contour(x1,x2,matrix(as.numeric(pred.grid),ngrid,ngrid),levels=c(0,1,2,3,4,5,6,7,8,9),add=T)




##################################
# Ejercicio 3
##################################

# Librerias
library("tidyverse")
library("imager") # manipular imagenes
library("plyr") # visualización 
library("raster") # manipular imagenes
library("gridExtra") # visualizar imagenes
library("grid") # visualizar imagnes
library("rgl") # manipular imagenes
library("ggplot2") # visualizar imagenes
library("plot3D") # visualizar 3d imagenes
library("plot3Drgl") # visualizar 3d imagenes
library("kernlab") # Kernel PCA y Kernel K-means
rm(list = ls())
# Importar carpeta de imagene; introduccir dirección
files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea")

##############################################
# Matrices que almacenan medianas para el caso RGB y HSV

MedianasHSV <- matrix(0L, 1300,3)

# Se importa imagen por imagen y se separa en tres canales 
# a cada canal de una imagen le tomamos la mediana
for(i in 1:1300) {
  MedianasHSV[i,] <- RGBtoHSV(files[[i]]) %>% channels %>%  laply(median) #medianpixel 
}
MedianasHSV[,1] <- MedianasHSV[,1]/360





setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316")
Etiquetas <- read.csv("meta_data_fruits_tarea.csv")
base <- cbind(Etiquetas$tipo.fruta, MedianasHSV) %>% as.data.frame()
names(base) <- c("y","H","S","V")
train <- sample_frac(as.data.frame(base), size = .8)
test <- anti_join(as.data.frame(base),train)
train$y <- train$y %>% as.factor
test$y <- test$y %>% as.factor

################################################################

#############################
#     SVM
#############################

# Librerías
library("e1071")
library("caret")
# Número de particiones para K-folds Cross Validation
folds <- createFolds(train$y, k = 5)
# Parámetros

GAMMA <-seq(1,20,1)
errores<- matrix(0L,1,length(GAMMA))

#Ajuste del clasificador
SVM <- function(S, gama){
  cvKernelSVM <- lapply(S, function(x){
    training_fold <- train[-folds$Fold1, ]
    clasificador <- svm(y ~ .,
                        data = training_fold, 
                        type = 'C-classification', 
                        kernel = 'radial',
                        gamma=gama)
    y_pred_test <- predict(clasificador, newdata = test[,-1])
    cm_test <- table(as.numeric(as.character(test$y)), y_pred_test)
    return(sum(diag(cm_test)) / sum(cm_test))
  })
  precisionKernelSVM <- mean(as.numeric(cvKernelSVM))
  errorKernelSVM <- 1-precisionKernelSVM  
  return(errorKernelSVM)
  
}

# CV plot  

  for(j in 1:length(GAMMA)){
    errores[1,j] <-SVM(folds, GAMMA[j])
  }


# Visualización
x11()
p1 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point() + geom_line() +
  labs(title="SVM", subtitle = "", x="gamma", y="CV/5 Misclassification Rate"  )



# MODELO SELECCIONADO
# PÁRAMETRO KERNEL GAUSSIANO DE 7

datosPruebaSVM <- test[,-1] # aquí van los datos de prueba
cvKernelSVM <- lapply(folds, function(x){
  training_fold <- train[-x, ]
  clasificador <- svm(y ~ .,
                      data = training_fold, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      gamma=7)
  y_pred_test <- predict(clasificador, newdata = datosPruebaSVM)
  cm_test <- table(as.numeric(as.character(test$y)), y_pred_test)
  return(sum(diag(cm_test)) / sum(cm_test))
})

# Precisión y Error de clasificación
precisioSVM <- mean(as.numeric(cvKernelSVM)) # 0.9853282
errorSVM <- 1-mean(as.numeric(cvKernelSVM)) # 0.01467181

#####################
# Árbol de Decisión
#####################
library("rpart")
# Parámetros

deep <- seq(2,30,1)
errores2<- matrix(0L,1,length(deep))

# Ajuste del clasificador
Arbol <- function(S, deep){
  cvDecisionTree <- lapply(folds, function(x){
    training_fold <- train[-x, ]
    #test_fold <- prueba[x, ]
    clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                               xval=0, maxdepth = deep )
    
    y_pred <- predict(clasificadorArbol, newdata = test[,-1], type = "class")
    cm <- table(as.numeric(as.character(test$y)), y_pred)
    precision <- sum(diag(cm)) / sum(cm)
    return(precision)
  })  
  precisionKernelArbol<- mean(as.numeric(cvDecisionTree))
  errorKernelArbol <- 1-precisionKernelArbol  
  return(errorKernelArbol)
}



# CV plot

  for(j in 1: length(deep)){
    errores2[1,j] <- Arbol(folds,  deep[j])
  }


# visualización
x11()
ap1 <-  ggplot() +
  aes(x=deep, y=errores2[1,]) + geom_point() + geom_line() +
  labs(title="Árbol de clasificación", subtitle = "", x="árbol-size", y="CV/5 Misclassification Rate"  )




# MODELO SELECCIONADO ÁRBOL DE CLASIFICACIÓN
# número de partición máxima 6

set.seed(2000)
datosPruebaArbol <- test[,-1]
cvDecisionTree <- lapply(folds, function(x){
  training_fold <- train[-x, ]
 
  clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                             cp=-1, xval=0, maxdepth = 6 )
  
  y_pred <- predict(clasificadorArbol, newdata = datosPruebaArbol, type = "class")
  cm <- table(as.numeric(as.character(test$y)), y_pred)
  precision <- sum(diag(cm)) / sum(cm)
  return(precision)
})  

# Precisión y Error clasificación
precisionArbol <- mean(as.numeric(cvDecisionTree)) # 0.9023438
errorArbol <- 1-precisionArbol  #  0.09765625


###########################
#       RANDOFOREST
###########################
set.seed(1649)

#Librerías 
library("randomForest")
library("caret")
library("rpart")
# Parámetros
deepRF <- c(10,200,500,800, 1000)
mtryRF <- c(1,2,3)
errores3<- matrix(0L,length(mtryRF),length(deepRF))


# Ajuste del clasificador
RF <- function(S, mtryRF, deepRF){
  cvRandomForest <- lapply(folds, function(x){
    training_fold <- train[-x, ]
    clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                   ntree = deepRF, mtry= mtryRF)
    y_pred <- predict(clasificadorRF, newdata = test[,-1])
    cm <- table(as.numeric(as.character(test$y)), y_pred)
    precision <- sum(diag(cm)) / sum(cm)
    return(precision)
  })
  return(1- mean(as.numeric(cvRandomForest)))
}

# CV plot
for(i in 1:length(mtryRF)){
  for(j in 1: length(deepRF)){
    errores3[i,j] <- RF(folds, mtryRF[i], deepRF[j])
  }
}

# Visualización
RFap1 <-  ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=1", x="# de árbol", y="CV/5 Misclassification Rate"  )
RFap2 <- ggplot() +
  aes(x=deepRF, y=errores3[2,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=2", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap3 <-  ggplot() +
  aes(x=deepRF, y=errores3[3,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=3", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap4 <- ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point(color="red") + geom_line(color="red") +
  labs(title="Random Forest", subtitle = "", x="# de árbol", y="CV/5 Misclassification Rate" , caption = "Rojo: # variables 1  , Azul:  # variables 2 , Negro: # variables 3" )+
  geom_point(aes(x=deepRF, y=errores3[2,]),color="blue" ) + geom_line(aes(x=deepRF, y=errores3[2,]),color="blue" ) +
  geom_point(aes(x=deepRF, y=errores3[3,])) + geom_line(aes(x=deepRF, y=errores3[3,])) 

x11()
gridExtra::grid.arrange(RFap1,RFap2,RFap3,RFap4,nrow=2,ncol=2)

# MODELO SELECCIONADO RANDOM FOREST
# número de árboles 200
# número de variables 1
set.seed(1)
cvRandomForest <- lapply(folds, function(x){
  training_fold <- train[-x, ]
  clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                 ntree = 200, mtry= 1)
  y_pred <- predict(clasificadorRF, newdata = test[,-1])
  cm <- table(as.numeric(as.character(test$y)), y_pred)
  precision <- sum(diag(cm)) / sum(cm)
  return(precision)
})

# Precisión y Error en clasificación
AcurracyRF <-  mean(as.numeric(cvRandomForest)) # 0.984375
errorRF <- 1- mean(as.numeric(cvRandomForest)) # 0.015625

###########################
#       ADABOOST
###########################

# Librerías
library("ada")
library("adabag")

# Parámetros
deepADA <- c(2, 5, 10, 20, 50)
errores4<- matrix(0L,1, length(deepADA))

# Ajuste del clasificador

ADABOOst <- function(S, deepADA){
  cvAdaBoost <- lapply(folds, function(x){
    training_fold <- train[-x, ]
    clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                                mfinal=deepADA, coeflearn = 'Zhu')
    y_pred <- predict.boosting(clasificadorADA, newdata = test[,-1])
    cm <- table(as.numeric(as.character(test$y)), y_pred$class)
    return(sum(diag(cm)) / sum(cm))
  })
  return(1-mean(as.numeric(cvAdaBoost)))
}

# CV plot
for(i in 1:length(deepADA)){
  errores4[1,i] <-  ADABOOst(folds, deepADA[i])
}


# Visualización
x11()
ADap1 <-  ggplot() +
  aes(x=deepADA, y=errores4[1,]) + geom_point() + geom_line() +
  labs(title="AdaBoost", subtitle = "SAMME", x="Iteraciones", y="CV/5 Misclassification Rate"  )




# MODELO SELECCIONADO ADABOOST SAMME 
# Número de iteraciones 20
set.seed(1)
cvAdaBoost <- lapply(folds, function(x){
  training_fold <- train[-folds$Fold1, ]
  clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                              mfinal=20, coeflearn="Zhu")
  y_pred <- predict.boosting(clasificadorADA, newdata = test[,-1], type="class")
  cm <- table(test$y, as.numeric(y_pred$class))
  return(sum(diag(cm)) / sum(cm))
})

# Precisión y Error en clasificación
AcurracyAdaBoost <-  mean(as.numeric(cvAdaBoost)) #0.9804688
errorAdaBoost <-1-mean(as.numeric(cvAdaBoost)) # 0.01953125




#############################
# Modelo seleccionado
#############################

# IMPLEMENTACIÓN EN PCA 

pc <- princomp(train[,-1])
## usaremos solo 2 PC
xpc.train <- cbind(pc$scores[,1],pc$scores[,2])
## proyecta los datos de prueba en los PC
test.pc <- predict(pc,test[,-1])
xpc.test <- cbind(test.pc[,1],test.pc[,2])

## grafica 2 scores
cols <- colors()[c(21,22,23,24,26,51,53,17,142,404,507,621,463)]
pchs <- as.character(1:13)

# Cambiando niveles
temporal <- train
levels(train$y) <- Etiquetas$tipo.fruta %>% levels
Frutas <- train$y

# Visualización
x11()
pc$scores %>% as.data.frame() %>% ggplot() +
  geom_point(cex=4, aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas, shape=Frutas)) + 
   scale_shape_manual(values=1:nlevels(Frutas))+
labs(title = "Frutas",x="1st PC",y="2nd PC" )


# Dibujar grid
ngrid <- 100
x1 <- seq(min(pc$scores[,1]),max(pc$scores[,1]),length=ngrid)
x2 <- seq(min(pc$scores[,2]),max(pc$scores[,2]),length=ngrid)
x <- expand.grid(x1,x2)
train <- temporal

# Clasificador
set.seed(1)
clasificador <- svm(as.factor(train[,1]) ~ .,
                    data = xpc.train, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    gamma=0.03,
                    cost=1)

# predicción
y.hat <- predict(clasificador,xpc.test,decision.values=FALSE,probability=FALSE)
## error
err.svm <- mean(as.numeric(as.character(test[,1]))!=y.hat)
err.svm
## predice el grid
colnames(x) <- NULL
pred.grid <- predict(clasificador,x,decision.values=FALSE)




# Visualizacipon
ggplot() + geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Frutas", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 0.4108527")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2],col=Frutas, shape=Frutas), cex=4) +
  scale_shape_manual(values=1:nlevels(Frutas))+
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=4,col="black") 



x11()


# frontera de clasificación y datos de prueba
plot(x,col=cols[as.numeric(pred.grid)+1],pch=19,cex=.5,main="Frutas",
     xlab="1st PC",ylab="2nd PC",sub=paste("Error test:",round(err.svm,3)))
points(xpc.train,cex=1.5,col=cols[as.numeric(as.character(train[,1]))],pch=c(1:11)[as.numeric(Frutas)])
points(xpc.test,pch="+",cex=1.5,col="black")
contour(x1,x2,matrix(as.numeric(pred.grid),ngrid,ngrid),levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13),add=T)
  




########################################################
#   PARTE II EJERCICIO 3 Cuartiles
########################################################

# Librerías
library("tidyverse")
library("magrittr")
library("imager") 
# introduzca dirección de las imagenes
files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/")
dir<-"C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"
n<-length(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"))
images_name<-as.vector(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"))
rgbimg<-matrix(0,n,3)
hsvimg<-matrix(0,n,9)
etiquetas<-rep('fruta',n)
# separar en canales HSV con su mediana y sus principales cuartiles
for(i in 1:n)
{
  imgtemp<-load.image(paste(dir,images_name[i], sep = ""))
  rgbimg[i,]<-imsplit(imgtemp,"c") %>% lapply(median) %>% unlist() %>% as.vector()
  hsvtemp<-HSVtoRGB(imgtemp)
  hsvimg[i,]<-apply(hsvtemp, 4, quantile, probs=c(0.25,0.5,0.75)) %>% as.vector()
  etiquetas[i]<-strsplit(images_name[i],'_')[[1]][1]
}
# Etiquetas de las imagenes
etiquetas2 <- etiquetas %>% as.factor
frutas <- levels(as.factor(etiquetas)) 
NuevasEtiquetas <- factor(c("1","2","3","4","5","6","7","8","9","10","11"))
levels(etiquetas2) <- NuevasEtiquetas %>% levels
# Base de entrenamiento y de validación
base <- cbind(etiquetas2, hsvimg) %>% as.data.frame()
names(base)[1] <- "y"
train <- sample_frac(base, size = .8)
test <- anti_join(base,train)
train$y <- train$y %>% as.factor
test$y <- test$y %>% as.factor

# Se trabaja con los scores
# IMPLEMENTACIÓN EN PCA 

pc <- princomp(train[,-1])
## entrenamiento
xpc.train <- cbind(pc$scores[,1],pc$scores[,2])
## prueba proyectar
test.pc <- predict(pc,test[,-1])
## se utiliza solo las dos primeras componentes
xpc.test <- cbind(test.pc[,1],test.pc[,2])

## grafica 2 scores
cols <- colors()[c(12,24,26,51,53,17,142,404,507,621,463)]
pchs <- as.character(1:11)

# Visualizar
temporal <- train

levels(train$y) <- etiquetas %>% as.factor %>% levels

Frutas <- train$y

# Visualización
x11()
pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas, shape=Frutas) +
  scale_shape_manual(values=1:nlevels(Frutas)) +
  geom_point(cex=4) + labs(title = "Frutas cuartiles",x="1st PC",y="2nd PC" )

pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas) +
  geom_point(cex=4, pch=pchs[train$y]) + labs(title = "Frutas cuartiles",x="1st PC",y="2nd PC" )

# Regresando niveles a variable respuesta
train <- temporal

train_pc <- cbind(train$y, xpc.train) %>% as.data.frame()
names(train_pc)[1] <- "y" 
train_pc$y <-train_pc$y %>% as.factor


#############################
#     SVM
#############################

# Librerías
library("e1071")
library("caret")

folds <- createFolds(train$y, k = 5)
# Parámetros

GAMMA <-seq(1,60,1)
errores<- matrix(0L,1,length(GAMMA))


#Ajuste del clasificador
SVM <- function(S, gama){
  cvKernelSVM <- lapply(S, function(x){
    training_fold <- train_pc[-x, ]
    clasificador <- svm(y ~ .,
                        data = training_fold, 
                        type = 'C-classification', 
                        kernel = 'radial',
                        gamma=gama)
    
    y_pred_test <- predict(clasificador, newdata = xpc.test)
    cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred_test)))
    return(sum(diag(cm_test)) / sum(cm_test))
  })
  precisionKernelSVM <- mean(as.numeric(cvKernelSVM))
  errorKernelSVM <- 1-precisionKernelSVM  
  return(errorKernelSVM)
  
}

# CV plot  

for(j in 1:length(GAMMA)){
  errores[1,j] <-SVM(folds, GAMMA[j])
}


# Visualización
x11()
p1 <- ggplot() +
  aes(x=GAMMA, y=errores[1,]) + geom_point() + geom_line() +
  labs(title="SVM", subtitle = "", x="gamma", y="CV/5 Misclassification Rate"  )


# MODELO SELECCIONADO
# PÁRAMETRO KERNEL GAUSSIANO DE 20

datosPruebaSVM <- xpc.test# aquí van los datos de prueba
cvKernelSVM <- lapply(folds, function(x){
  training_fold <- train_pc[-x, ]
  clasificador <- svm(y ~ .,
                      data = training_fold, 
                      type = 'C-classification', 
                      kernel = 'radial',
                      gamma=20)
  
  y_pred_test <- predict(clasificador, newdata = datosPruebaSVM)
  cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred_test)))
  return(sum(diag(cm_test)) / sum(cm_test))
})


# Precisión y Error de clasificación
precisioSVM <- mean(as.numeric(cvKernelSVM)) # 0.7546154
errorSVM <- 1-mean(as.numeric(cvKernelSVM)) # 0.2453846

#####################
# Árbol de Decisión
#####################
library("rpart")

xpc.test <- xpc.test %>% as.data.frame
names(xpc.test) <- c("V2","V3")
# Ajuste del clasificador
# Parámetros

deep <- seq(2,20,1)
errores2<- matrix(0L,1,length(deep))

Arbol <- function(S, deep){
  cvDecisionTree <- lapply(S, function(x){
    training_fold <- train_pc[-folds$Fold1, ]
    clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                               xval=0, maxdepth = deep)
    
    y_pred <- predict(clasificadorArbol, newdata = xpc.test, type = "class")
    cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred)))
    precision <- sum(diag(cm_test)) / sum(cm_test)
    return(precision)
  })  
  precisionKernelArbol<- mean(as.numeric(cvDecisionTree))
  errorKernelArbol <- 1-precisionKernelArbol  
  return(errorKernelArbol)
}



# CV plot

for(j in 1: length(deep)){
  errores2[1,j] <- Arbol(folds,  deep[j])
}


# visualización
x11()
ap1 <-  ggplot() +
  aes(x=deep, y=errores2[1,]) + geom_point() + geom_line() +
  labs(title="Árbol de clasificación", subtitle = "", x="árbol-size", y="CV/5 Misclassification Rate"  )




# MODELO SELECCIONADO ÁRBOL DE CLASIFICACIÓN
# número de partición máxima 8


datosPruebaArbol <- xpc.test
cvDecisionTree <- lapply(folds, function(x){
  training_fold <- train_pc[-x, ]
  set.seed(2000)
  clasificadorArbol <- rpart(y ~ ., data = training_fold, method = "class",
                             xval=0, maxdepth = 8 )
  
  y_pred <- predict(clasificadorArbol, newdata = xpc.test, type = "class")
  cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred)))
  precision <- sum(diag(cm_test)) / sum(cm_test)
  return(precision)
})  

# Precisión y Error clasificación
precisionArbol <- mean(as.numeric(cvDecisionTree)) # 0.6876923
errorArbol <- 1-precisionArbol  #  0.3123077


###########################
#       RANDOFOREST
###########################
set.seed(1649)

#Librerías 
library("randomForest")
library("caret")
library("rpart")
# Parámetros
deepRF <- c(10,200,500,800, 1000)
mtryRF <- c(1,2,3)
errores3<- matrix(0L,length(mtryRF),length(deepRF))


# Ajuste del clasificador
RF <- function(S, mtryRF, deepRF){
  cvRandomForest <- lapply(folds, function(x){
    training_fold <- train_pc[-x, ]
    clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                   ntree = deepRF, mtry= mtryRF)
    y_pred <- predict(clasificadorRF, newdata = xpc.test)
    cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred)))
    precision <- sum(diag(cm_test)) / sum(cm_test)
    return(precision)
  })
  return(1- mean(as.numeric(cvRandomForest)))
}

# CV plot
for(i in 1:length(mtryRF)){
  for(j in 1: length(deepRF)){
    errores3[i,j] <- RF(folds, mtryRF[i], deepRF[j])
  }
}

# Visualización
RFap1 <-  ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=1", x="# de árbol", y="CV/5 Misclassification Rate"  )
RFap2 <- ggplot() +
  aes(x=deepRF, y=errores3[2,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=2", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap3 <-  ggplot() +
  aes(x=deepRF, y=errores3[3,]) + geom_point() + geom_line() +
  labs(title="Random Forest", subtitle = "Variables=3", x="# de árbol", y="CV/5 Misclassification Rate"  )

RFap4 <- ggplot() +
  aes(x=deepRF, y=errores3[1,]) + geom_point(color="red") + geom_line(color="red") +
  labs(title="Random Forest", subtitle = "", x="# de árbol", y="CV/5 Misclassification Rate" , caption = "Rojo: # variables 1  , Azul:  # variables 2 , Negro: # variables 3" )+
  geom_point(aes(x=deepRF, y=errores3[2,]),color="blue" ) + geom_line(aes(x=deepRF, y=errores3[2,]),color="blue" ) +
  geom_point(aes(x=deepRF, y=errores3[3,])) + geom_line(aes(x=deepRF, y=errores3[3,])) 

x11()
gridExtra::grid.arrange(RFap1,RFap2,RFap3,RFap4,nrow=2,ncol=2)

# MODELO SELECCIONADO RANDOM FOREST
# número de árboles 200
# número de variables 1

cvRandomForest <- lapply(folds, function(x){
  training_fold <- train_pc[-x, ]
  clasificadorRF <- randomForest(y ~ ., data = training_fold, 
                                 ntree = 200, mtry= 1)
  y_pred <- predict(clasificadorRF, newdata = xpc.test)
  
  cm_test <- table(as.numeric(as.character(test$y)), as.numeric(as.character(y_pred)))
  precision <- sum(diag(cm_test)) / sum(cm_test)
  return(precision)
})

# Precisión y Error en clasificación
AcurracyRF <-  mean(as.numeric(cvRandomForest)) # 0.7484615
errorRF <- 1- mean(as.numeric(cvRandomForest)) # 0.2515385

###########################
#       ADABOOST
###########################

# Librerías
library("ada")
library("adabag")

# Parámetros
deepADA <- c(2, 5, 10, 20, 50)
errores4<- matrix(0L,1, length(deepADA))

# Ajuste del clasificador

ADABOOst <- function(S, deepADA){
  cvAdaBoost <- lapply(folds, function(x){
    training_fold <- train_pc[-folds$Fold1, ]
    clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                                mfinal=deepADA, coeflearn = 'Zhu')
    y_pred <- predict.boosting(clasificadorADA, newdata = xpc.test)
    
    cm_test <- table(as.numeric(as.character(test$y)), as.numeric(y_pred$class))
    
    return(sum(diag(cm_test)) / sum(cm_test))
  })
  return(1-mean(as.numeric(cvAdaBoost)))
}

# CV plot
for(i in 1:length(deepADA)){
  errores4[1,i] <-  ADABOOst(folds, deepADA[i])
}


# Visualización
x11()
ADap1 <-  ggplot() +
  aes(x=deepADA, y=errores4[1,]) + geom_point() + geom_line() +
  labs(title="AdaBoost", subtitle = "SAMME", x="Iteraciones", y="CV/5 Misclassification Rate"  )


# MODELO SELECCIONADO ADABOOST SAMME 
# Número de iteraciones 20

cvAdaBoost <- lapply(folds, function(x){
  training_fold <- train_pc[-x, ]
  clasificadorADA <- boosting(y~., data=training_fold, boos=F, 
                              mfinal=20, coeflearn="Zhu")
  y_pred <- predict.boosting(clasificadorADA, newdata = xpc.test)
  cm_test <- table(as.numeric(as.character(test$y)), as.numeric(y_pred$class))
  
  return(sum(diag(cm_test)) / sum(cm_test))
})

# Precisión y Error en clasificación
AcurracyAdaBoost <-  mean(as.numeric(cvAdaBoost)) # 0.71
errorAdaBoost <-1-mean(as.numeric(cvAdaBoost)) # 0.29


#############################
# Modelo seleccionado
#############################

# El clasificador visualizado  en las primeras dos componentes
# Grid
ngrid <- 100
x1 <- seq(min(pc$scores[,1]),max(pc$scores[,1]),length=ngrid)
x2 <- seq(min(pc$scores[,2]),max(pc$scores[,2]),length=ngrid)
x <- expand.grid(x1,x2)

# Clasificador
clasificador <- svm(y ~ .,
                    data = train_pc, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    gamma=20)
# predicción
y.hat <- predict(clasificador,xpc.test,decision.values=FALSE,probability=FALSE)
## error
err.svm <- mean(as.numeric(as.character(test[,1]))!=y.hat)
err.svm
## predice el grid
colnames(x) <- NULL
pred.grid <- predict(clasificador,x,decision.values=FALSE)

# Vsualización
ggplot() + geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Frutas", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 0.2423077")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2],col=Frutas, shape=Frutas), cex=4) +
  scale_shape_manual(values=1:nlevels(Frutas))+
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=4,col="black") 


x11()

# Frontera de clasificación
plot(x,col=cols[as.numeric(pred.grid)+2],pch=19,cex=.1,main="Frutas",
     xlab="1st PC",ylab="2nd PC",sub=paste("Error test:",round(err.svm,3)))
points(xpc.train,cex=1,col=cols[as.numeric(as.character(train_pc[,1]))],pch=c(1:11)[as.numeric(Frutas)])
points(xpc.test,pch="+",cex=1.5,col="black")
contour(x1,x2,matrix(as.numeric(pred.grid),ngrid,ngrid),levels=c(1,2,3,4,5,6,7,8,9,10,11),add=T)



#################################
# PUNTO EXTRA
################################

# Utilizando cuartiles de la representación HSV

# Introduzca las imagenes de la tarea 4; i.e, cambie  path
files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/")

dir<-"C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"
n<-length(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"))
images_name<-as.vector(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea/"))
rgbimg<-matrix(0,n,3)
hsvimg<-matrix(0,n,9)
etiquetas3<-rep('fruta',n)
# cuartiles HSV
for(i in 1:n)
{
  imgtemp<-load.image(paste(dir,images_name[i], sep = ""))
  rgbimg[i,]<-imsplit(imgtemp,"c") %>% lapply(median) %>% unlist() %>% as.vector()
  hsvtemp<-HSVtoRGB(imgtemp)
  hsvimg[i,]<-apply(hsvtemp, 4, quantile, probs=c(0.25,0.5,0.75)) %>% as.vector()
  etiquetas3[i]<-strsplit(images_name[i],'_')[[1]][1]
}
# labels
etiquetas2 <- etiquetas3 %>% as.factor
frutas <- levels(as.factor(etiquetas3)) 
NuevasEtiquetas <- factor(c("1","2","3","4","5","6","7","8","9","10","11"))
levels(etiquetas2) <- NuevasEtiquetas %>% levels

# datos de entrenamiento y de prueba
base <- cbind(etiquetas2, hsvimg) %>% as.data.frame()
names(base)[1] <- "y"
train <- sample_frac(base, size = .8)
test <- anti_join(base,train)
train$y <- train$y %>% as.factor
test$y <- test$y %>% as.factor

# Se trabaja con los scores
# IMPLEMENTACIÓN EN PCA 

pc <- princomp(train[,-1])


# MIS IMAGENES

# Introduzca imagenes reales
files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas/")
dir<-"C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas/"
n<-length(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas/"))
images_name<-as.vector(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas/"))
rgbimg<-matrix(0,n,3)
hsvimg<-matrix(0,n,9)
etiquetas<-rep('fruta',n)
# cuartiles HSV
for(i in 1:n)
{
  imgtemp<-load.image(paste(dir,images_name[i], sep = ""))
  rgbimg[i,]<-imsplit(imgtemp,"c") %>% lapply(median) %>% unlist() %>% as.vector()
  hsvtemp<-HSVtoRGB(imgtemp)
  hsvimg[i,]<-apply(hsvtemp, 4, quantile, probs=c(0.25,0.5,0.75)) %>% as.vector()
  etiquetas[i]<-strsplit(images_name[i],'_')[[1]][1]
}
hsvimg <- hsvimg %>% as.data.frame()
names(hsvimg) <- c("V2",  "V3" , "V4" , "V5" , "V6" , "V7" ,
                   "V8",  "V9" , "V10")
## entrenamiento
xpc.train <- cbind(pc$scores[,1],pc$scores[,2])

## prueba proyectar
test.pc <- predict(pc,hsvimg)
## se utiliza solo las dos primeras componentes
xpc.test <- cbind(test.pc[,1],test.pc[,2])

## grafica 2 scores
cols <- colors()[c(12,24,26,51,53,17,142,404,507,621,463)]
pchs <- as.character(1:11)

# Visualizar
temporal <- train

levels(train$y) <- etiquetas3 %>% as.factor %>% levels

Frutas <- train$y

#Visualizar
x11()
pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas, shape=Frutas) +
  scale_shape_manual(values=1:nlevels(Frutas)) +
  geom_point(cex=4) + labs(title = "Frutas cuartiles",x="1st PC",y="2nd PC" )


pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas) +
  geom_point(cex=4, pch=pchs[train$y]) + labs(title = "Frutas cuartiles",x="1st PC",y="2nd PC" )


train <- temporal

train_pc <- cbind(train$y, xpc.train) %>% as.data.frame()
names(train_pc)[1] <- "y" 
train_pc$y <-train_pc$y %>% as.factor


ngrid <- 100
x1 <- seq(min(pc$scores[,1]),max(pc$scores[,1]),length=ngrid)
x2 <- seq(min(pc$scores[,2]),max(pc$scores[,2]),length=ngrid)
x <- expand.grid(x1,x2)

# clasificador
library("e1071")
library("magrittr")
clasificador <- svm(y ~ .,
                    data = train_pc, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    gamma=20)

# etiquetando imagenes reales
etiquetas3 %>% as.factor %>% levels
Nombres <- c("Apple",  "Avocado","Avocado","Apple","Apple")
id <- c(1,3,3,1,1)
etiquetas <- cbind(etiquetas, Nombres, id) %>% as.data.frame()
# predicción
y.hat <- predict(clasificador,xpc.test,decision.values=FALSE,probability=FALSE)
## error
err.svm <- mean(as.numeric(as.character(etiquetas[,3]))!=y.hat)
err.svm
## predice el grid
colnames(x) <- NULL
pred.grid <- predict(clasificador,x,decision.values=FALSE)

# Visualización 

p <- ggplot() + geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Frutas", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 1")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2],col=Frutas, shape=Frutas), cex=4) +
  scale_shape_manual(values=1:nlevels(Frutas))+
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=10,col="black") 

# Se visualizan las clasiicaciones de las imagenes reales en las primeras
# dos componentes. Para eso de plotea la imagen en ggplot

# Introduzca imagenes convertidas en png
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas 2/")
# Librerías neceaias
library("ggplot2")
library("png")
library("grid")
getwd()
# Obtener imagen en un cuadro de gráfico
img1 <- readPNG("man.png")
img3 <- readPNG("aguacate1.png")
img4 <- readPNG("aguacate2.png")
img5 <- readPNG("manzana1.png")
img6 <- readPNG("manzana2.png")
g1<- rasterGrob(img1, interpolate=TRUE)
g3<- rasterGrob(img3, interpolate=TRUE)
g4<- rasterGrob(img4, interpolate=TRUE)
g5<- rasterGrob(img5, interpolate=TRUE)
g6<- rasterGrob(img6, interpolate=TRUE)

# Visualización
x11()
p +
  geom_point(data=as.data.frame(xpc.test), aes(xpc.test[,1],xpc.test[,2])) +
  annotation_custom(g1,xmin=xpc.test[1,1], xmax=xpc.test[1,1]+.1,ymin=xpc.test[1,2], ymax=xpc.test[1,2]+.1) +
  annotation_custom(g3,xmin=xpc.test[2,1], xmax=xpc.test[2,1]+.1,ymin=xpc.test[2,2], ymax=xpc.test[2,2]+.1)+
  annotation_custom(g4,xmin=xpc.test[3,1], xmax=xpc.test[3,1]+.1,ymin=xpc.test[3,2], ymax=xpc.test[3,2]+.1)+
  annotation_custom(g5,xmin=xpc.test[4,1], xmax=xpc.test[4,1]+.1,ymin=xpc.test[4,2], ymax=xpc.test[4,2]+.1)+
  annotation_custom(g6,xmin=xpc.test[5,1], xmax=xpc.test[5,1]+.1,ymin=xpc.test[5,2], ymax=xpc.test[5,2]+.1)


#######################
# HSV SIN CUARTILES
######################
# Librerias
library("tidyverse")
library("imager") # manipular imagenes
library("plyr") # visualización 
library("raster") # manipular imagenes
library("gridExtra") # visualizar imagenes
library("grid") # visualizar imagnes
library("rgl") # manipular imagenes
library("ggplot2") # visualizar imagenes

rm(list = ls())
# Importar carpeta de imagene; introduccir dirección
files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea")

# Matrices que almacenan medianas para el caso RGB y HSV

MedianasHSV <- matrix(0L, 1300,3)

# Se importa imagen por imagen y se separa en tres canales 
# a cada canal de una imagen le tomamos la mediana
for(i in 1:1300) {
  MedianasHSV[i,] <- RGBtoHSV(files[[i]]) %>% channels %>%  laply(median) #medianpixel 
}
MedianasHSV[,1] <- MedianasHSV[,1]/360

# Se importa los metadatos de las frutas donde vienen sus etiquetas
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/TAREA 4/Tarea 4-20190316")
# estos son los de la tarea 4
Etiquetas <- read.csv("meta_data_fruits_tarea.csv")
base <- cbind(Etiquetas$tipo.fruta, MedianasHSV) %>% as.data.frame()
# se cambia de nombre
names(base) <- c("y","H","S","V")
# obtenemos datos de entrenamiento y de prueba
train <- sample_frac(as.data.frame(base), size = .8)
test <- anti_join(as.data.frame(base),train)
train$y <- train$y %>% as.factor
test$y <- test$y %>% as.factor

# Se trabaja con los scores
# IMPLEMENTACIÓN EN PCA 

pc <- princomp(train[,-1])


# MIS IMAGENES; importamos las imagenes reales (formato jpg)

files <- load.dir("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas 2")


# Matrices que almacenan medianas para el caso RGB y HSV

MedianasHSV2 <- matrix(0L, 5,3)

# Se importa imagen por imagen y se separa en tres canales 
# a cada canal de una imagen le tomamos la mediana
for(i in 1:5) {
  MedianasHSV2[i,] <- RGBtoHSV(files[[i]]) %>% channels %>%  laply(median) #medianpixel 
}
MedianasHSV2[,1] <- MedianasHSV2[,1]/360
colnames(MedianasHSV2) <- c("H","S","V")
## entrenamiento scores
xpc.train <- cbind(pc$scores[,1],pc$scores[,2])

## prueba proyectar imagenes reales 
test.pc <- predict(pc,MedianasHSV2)
## se utiliza solo las dos primeras componentes
xpc.test <- cbind(test.pc[,1],test.pc[,2])

## grafica 2 scores
cols <- colors()[c(21,22,23,24,26,51,53,17,142,404,507,621,463)]
pchs <- as.character(1:13)

temporal <- train
levels(train$y) <- Etiquetas$tipo.fruta %>% levels
Frutas <- train$y

# Visualización
x11()
pc$scores %>% as.data.frame() %>% ggplot() +
  aes(x=pc$scores[,1], y=pc$scores[,2],col=Frutas) +
  geom_point(cex=4, pch=pchs[train$y]) + labs(title = "Frutas",x="1st PC",y="2nd PC" )

#grid
ngrid <- 100
x1 <- seq(min(pc$scores[,1]),max(pc$scores[,1]),length=ngrid)
x2 <- seq(min(pc$scores[,2]),max(pc$scores[,2]),length=ngrid)
x <- expand.grid(x1,x2)
train <- temporal

# clasificador
set.seed(1)
clasificador <- svm(as.factor(train[,1]) ~ .,
                    data = xpc.train, 
                    type = 'C-classification', 
                    kernel = 'radial',
                    gamma=.03)

# predicción imagenes reales
y.hat <- predict(clasificador,xpc.test,decision.values=FALSE,probability=FALSE)


# etiquetamos imagenes reales
images_name<-as.vector(list.files("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas/"))
lab<-c("manzana", "aguacate","aguacate", "manzana","manzana")
id <- c(2,5,5,2,2)
lab <- cbind(images_name,lab,id) %>% as.data.frame
# el error de clasificación
err.svm <- mean(as.numeric(as.character(lab[,3]))!=y.hat)
err.svm
## predice el grid
colnames(x) <- NULL
pred.grid <- predict(clasificador,x,decision.values=FALSE)



# Visualización de la predicción
x11()
p <- ggplot() + geom_point(col=cols[as.numeric(pred.grid)+1],pch=19,cex=1.5) +
  labs(title = "Frutas", x="1st PC",y="2nd PC", caption="Error de Entrenamiento: 1")  + geom_point(data= as.data.frame(xpc.train), mapping=aes(x=xpc.train[,1], y=xpc.train[,2],col=Frutas, shape=Frutas), cex=4) +
  scale_shape_manual(values=1:nlevels(Frutas))+
  geom_point(data = as.data.frame(xpc.test), mapping = aes(x = xpc.test[,1], y = xpc.test[,2]), pch="+",cex=10,col="black") 



# se visualiza la foto de la imagen en las dos primeras componentes
# extrae las imagenes en formato png
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 7/Frutas 2/")
# librerías necesarias
library("ggplot2")
library("png")
library(3)
getwd()
# Visualizar imagenes reales en las primeras dos componentes, y donde fueron
# clasificadas

img1 <- readPNG("man.png")
img3 <- readPNG("aguacate1.png")
img4 <- readPNG("aguacate2.png")
img5 <- readPNG("manzana1.png")
img6 <- readPNG("manzana2.png")
g1<- rasterGrob(img1, interpolate=TRUE)
g3<- rasterGrob(img3, interpolate=TRUE)
g4<- rasterGrob(img4, interpolate=TRUE)
g5<- rasterGrob(img5, interpolate=TRUE)
g6<- rasterGrob(img6, interpolate=TRUE)

# Visualización
x11()
p +
  geom_point(data=as.data.frame(xpc.test), aes(xpc.test[,1],xpc.test[,2])) +
  annotation_custom(g1,xmin=xpc.test[1,1], xmax=xpc.test[1,1]+.1,ymin=xpc.test[1,2], ymax=xpc.test[1,2]+.1) +
  annotation_custom(g3,xmin=xpc.test[2,1], xmax=xpc.test[2,1]+.1,ymin=xpc.test[2,2], ymax=xpc.test[2,2]+.1)+
  annotation_custom(g4,xmin=xpc.test[3,1], xmax=xpc.test[3,1]+.1,ymin=xpc.test[3,2], ymax=xpc.test[3,2]+.1)+
  annotation_custom(g5,xmin=xpc.test[4,1], xmax=xpc.test[4,1]+.1,ymin=xpc.test[4,2], ymax=xpc.test[4,2]+.1)+
  annotation_custom(g6,xmin=xpc.test[5,1], xmax=xpc.test[5,1]+.1,ymin=xpc.test[5,2], ymax=xpc.test[5,2]+.1)

###########################################
###########################################
###########################################
############# FIN  ########################
###########################################
###########################################
###########################################
