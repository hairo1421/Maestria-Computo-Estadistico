###############################
#
# Obj. II 	Predecir salario Catcher
#
###############################

############################
# Inciso a.1
###########################

######################################################################
# Con datos de bateo y la posición defensiva  (a) 9 veces)
######################################################################

# CLASIFICDOR GENERAL SIN IMPORTAR POSICIÓN

# Librerías

library("tidyverse")
library("ggplot2")
library("magrittr")
library("factoextra")
library("ggrepel")
library("plotly")
library("htmlwidgets")
library("readxl")

getwd()
setwd("C:/Users/h_air/Desktop/Datos")
Base_sin_valores_perdidos <- read_excel("Base con valores perdidos.xlsx")
BASE_NOnA <- Base_sin_valores_perdidos[,-1]
BASE_NOnA <- BASE_NOnA %>% as.data.frame()

BASE_NOnA <- BASE_NOnA %>% filter(Posicion=="RF")
BASE_NOnA %>% dim # 222 117
BASE_NOnA$clase[BASE_NOnA$clase==3] <- 2
BASE_NOnA$clase[BASE_NOnA$clase==4] <- 3
#############################
# Clasificadores Multiclase
#############################

################## BASE BATEO #########################

colnames(BASE_NOnA)[1:73] # base bateo
colnames(BASE_NOnA)[117]# base salario

# CLASIFICADOR VARIABLES BASE
Varibles_base <- names(BASE_NOnA)[10:30]
Varibles_base <- c(Varibles_base,names(BASE_NOnA)[117])
#[1] "R"      "H"      "X2B"    "X3B"   
#[5] "HR"     "RBI"    "SB"     "CS"    
#[9] "BB"     "SO"     "BA"     "OBP"   
#[13] "SLG"    "OPS"    "OPS."   "TB"    
#[17] "GDP"    "HBP"    "SH"     "SF"    
#[21] "IBB"    "clase" 

#####################################
# Selección de variables con arbol
######################################
Clasificador1 <-BASE_NOnA[,Varibles_base]
Clasificador1$clase[Clasificador1$clase==-1] <-NA
Clasificador1 <- Clasificador1[complete.cases(Clasificador1),]
Clasificador1 %>% dim # 141  22

Clasificador1$clase <- Clasificador1$clase %>% as.factor()
Clasificador1 %>% names
Clasificador1_train <- sample_frac(Clasificador1, .8)
Clasificador1_test <- anti_join(Clasificador1,Clasificador1_train)
Clasificador1_train %>% dim # 113  22
Clasificador1_test %>% dim # 28 22

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
repeticiones <-1
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



SeleccionVariables1 <- modelo_rf1$finalModel$variable.importance %>% as.data.frame
SeleccionVariables1 <- SeleccionVariables1 %>% rownames_to_column()
colnames(SeleccionVariables1) <- c("variable","importance")

clasificador1_p1<- ggplot(SeleccionVariables1, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo  Básicas")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


temp1 <- SeleccionVariables1 %>% filter(importance>4)
# Variables seleccionadas
temp1$variable
# [1]"RBI" "SB"  "SO"  "BA"  "OBP" "GDP" "HBP"   


# varios modelos
Clasificador1_test <- Clasificador1_test[, c(temp1$variable,"clase")]
Clasificador1_train <- Clasificador1_train[, c(temp1$variable,"clase")]


Clasificador1_train %>% names
library("modelgrid")
grid_modelos <- model_grid()
grid_modelos
grid_modelos <- grid_modelos %>%
  share_settings(
    y = Clasificador1_train$clase,
    x = Clasificador1_train[,-8],
    metric = "Accuracy",
    trControl = trainControl(method = "cv",
                             number = 10,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE
    )
  )


grid_modelos <- grid_modelos %>%
  add_model(
    model_name = "Reg_logistica",
    method     = "multinom",
    tuneGrid   = expand.grid(
      decay = c(0.0001, 0.1, 0.5)
    )
  )%>%
  add_model(
    model_name = "SVM",
    method = "svmRadial",
    tuneGrid   = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 0.5, 1),
      C = c(1 , 20, 50, 100, 200)
    )
  ) %>%
  add_model(
    model_name = "RandomForest",
    method     = "ranger",
    num.trees  = 200,
    tuneGrid   = expand.grid(
      mtry = 1:(ncol(Clasificador1_test)-1),
      min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
      splitrule = "gini"
    )
  ) %>%
  add_model(
    model_name = "RedNeuronal",
    method = "nnet",
    tuneGrid   = expand.grid(size = c(10, 20, 50, 80, 100, 120),
                             decay = c(0.0001, 0.1, 0.5)
    ),
    rang = c(-0.9, 0.9),
    MaxNWts = 2000,
    # Para que no se muestre cada iteración por pantalla
    trace = FALSE
  )


grid_modelos$models


# Se emplean 4 cores en paralelo.
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

grid_modelos <- train(grid_modelos, train_all = FALSE, resample_seed = 123)
grid_modelos$model_fits

stopCluster(cl)

clasificador1_p5 <- ggplot(grid_modelos$model_fits$RandomForest, highlight = TRUE) +
  labs(title = "Random Forest") +
  theme_bw()

clasificador1_p4 <- ggplot(grid_modelos$model_fits$RedNeuronal, highlight = TRUE) +
  labs(title = "Red Neuronal") +
  theme_bw()

clasificador1_p3 <- ggplot(grid_modelos$model_fits$Reg_logistica, highlight = TRUE) +
  labs(title = "Modelo Logístico Multinomial") +
  theme_bw()

clasificador1_p2 <- ggplot(grid_modelos$model_fits$SVM, highlight = TRUE) +
  labs(title = "SVM") +
  theme_bw()

############################
# Comparando modelos
############################

modelos <- list(logistic = grid_modelos$model_fits$Reg_logistica,
                SVMradial =  grid_modelos$model_fits$SVM,
                NNET =  grid_modelos$model_fits$RedNeuronal,
                RF =  grid_modelos$model_fits$RandomForest)

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))


clasificacion1_p7 <- metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = .5, linetype = "dashed")  +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() 

clasificacion1_p8 <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed")  +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")


Clasificador1_test %>% names
predicciones <- extractPrediction(
  models = modelos,
  testX = Clasificador1_test[, -8],
  testY = Clasificador1_test$clase
)
predicciones %>% head()

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))


metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))


clasificacion1_p9 <-ggplot(data = metricas_predicciones,
                           aes(x = reorder(object, accuracy), y = accuracy,
                               color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")

# no se clasifica con estas variables

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## Variables Estadísticas del jugador + base 
################################################################
################################################################
################################################################
################################################################
################################################################

# CLASIFICADOR VARIABLES BASE

Varibles_Estadisticas <- colnames(BASE_NOnA[,c(56,58:72)])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"clase")

#[22] "PA"        "BtRuns"    "BtWins"   
#[25] "Plays"     "WPA"       "WPA."     
#[28] "WPA..1"    "aLI"       "WPA.LI"   
#[31] "Clutch"    "RE24"      "REW"      
#[34] "boLI"      "RE24.boLI" "PHlev"    
#[37] "AB.win"     "clase"
#####################################
# Selección de variables con arbol
######################################
Clasificador2 <-BASE_NOnA[,Varibles_Estadisticas]
Clasificador2$clase[Clasificador2$clase==-1] <-NA
Clasificador2 %>% dim # 222  17
Clasificador2 <- Clasificador2[complete.cases(Clasificador2),]
Clasificador2 %>% dim #  129  17

# quitandole las base son muy pocas observaciones

Clasificador2$clase <- Clasificador2$clase %>% as.factor()
Clasificador2 %>% names
Clasificador2_train <- sample_frac(Clasificador2, .8)
Clasificador2_test <- anti_join(Clasificador2,Clasificador2_train)
Clasificador2_train %>% dim # 103  17
Clasificador2_test %>% dim # 26 17

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
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador2_test)-1),
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

modelo_rf2 <- train(clase ~ ., data = Clasificador2_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)



SeleccionVariables2 <- modelo_rf2$finalModel$variable.importance %>% as.data.frame
SeleccionVariables2 <- SeleccionVariables2 %>% rownames_to_column()
colnames(SeleccionVariables2) <- c("variable","importance")

clasificador2_p1<- ggplot(SeleccionVariables2, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo  Básicas + Estadísticas")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

temp2 <- SeleccionVariables2 %>% filter(importance>1.3)
# Variables seleccionadas
temp2$variable
#[1]"PA"     "BtWins" "WPA..1" "AB.win"    

# varios modelos
Clasificador2_test <- Clasificador2_test[, c(temp2$variable,"clase")]
Clasificador2_train <- Clasificador2_train[, c(temp2$variable,"clase")]
Clasificador2_train %>% names
library("modelgrid")
grid_modelos2 <- model_grid()
grid_modelos2
grid_modelos2 <- grid_modelos2 %>%
  share_settings(
    y = Clasificador2_train$clase,
    x = Clasificador2_train[,-5],
    metric = "Accuracy",
    trControl = trainControl(method = "cv",
                             number = 10,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE
    )
  )


grid_modelos2 <- grid_modelos2 %>%
  add_model(
    model_name = "Reg_logistica",
    method     = "multinom",
    tuneGrid   = expand.grid(
      decay = c(0.0001, 0.1, 0.5)
    )
  )%>%
  add_model(
    model_name = "SVM",
    method = "svmRadial",
    tuneGrid   = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 0.5, 1),
      C = c(1 , 20, 50, 100, 200)
    )
  ) %>%
  add_model(
    model_name = "RandomForest",
    method     = "ranger",
    num.trees  = 200,
    tuneGrid   = expand.grid(
      mtry = c(1, 2, 3, 4, 5),
      min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
      splitrule = "gini"
    )
  ) %>%
  add_model(
    model_name = "RedNeuronal",
    method = "nnet",
    tuneGrid   = expand.grid(size = c(10, 20, 50, 80, 100, 120),
                             decay = c(0.0001, 0.1, 0.5)
    ),
    rang = c(-0.9, 0.9),
    MaxNWts = 2000,
    # Para que no se muestre cada iteración por pantalla
    trace = FALSE
  )

grid_modelos2$models


# Se emplean 4 cores en paralelo.
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

grid_modelos2 <- train(grid_modelos2, train_all = FALSE, resample_seed = 123)
grid_modelos2$model_fits

stopCluster(cl)

clasificador2_p5 <- ggplot(grid_modelos2$model_fits$RandomForest, highlight = TRUE) +
  labs(title = "Random Forest") +
  theme_bw()

clasificador2_p4 <- ggplot(grid_modelos2$model_fits$RedNeuronal, highlight = TRUE) +
  labs(title = "Red Neuronal") +
  theme_bw()

clasificador2_p3 <- ggplot(grid_modelos2$model_fits$Reg_logistica, highlight = TRUE) +
  labs(title = "Modelo Logístico Multinomial") +
  theme_bw()

clasificador2_p2 <- ggplot(grid_modelos2$model_fits$SVM, highlight = TRUE) +
  labs(title = "SVM") +
  theme_bw()

############################
# Comparando modelos
############################

modelos2 <- list(logistic = grid_modelos2$model_fits$Reg_logistica,
                 SVMradial =  grid_modelos2$model_fits$SVM,
                 NNET =  grid_modelos2$model_fits$RedNeuronal,
                 RF =  grid_modelos2$model_fits$RandomForest)

resultados_resamples <- resamples(modelos2)
resultados_resamples$values %>% head(10)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))


clasificacion2_p7 <- metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = .5, linetype = "dashed")  +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() 

clasificacion2_p8 <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed")  +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

Clasificador2_test %>% names
predicciones <- extractPrediction(
  models = modelos2,
  testX = Clasificador2_test[, -5],
  testY = Clasificador2_test$clase
)
predicciones %>% head()

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))


metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))



clasificacion2_p9 <-ggplot(data = metricas_predicciones,
                           aes(x = reorder(object, accuracy), y = accuracy,
                               color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")

# no se clasifica con estas variables

######################
# CONCLUSIÓN
#####################
# Si comienzo a agregar variables estad´siticas el error de entrenamiento disminuye


################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## Variables Valor del jugador
################################################################
################################################################
################################################################
################################################################
################################################################

# CLASIFICADOR VARIABLES BASE
BASE_NOnA %>% names
Variables_Valor <- colnames(BASE_NOnA[,c(35:42,44,45,51,117)])

# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "clase" 
#####################################
# Selección de variables con arbol
######################################
Clasificador3 <-BASE_NOnA[,Variables_Valor]
Clasificador3$clase[Clasificador3$clase==-1] <-NA
Clasificador3 %>% dim # 222 12
Clasificador3 <- Clasificador3[complete.cases(Clasificador3),]
Clasificador3 %>% dim # 141  12

# quitandole las base son muy pocas observaciones

Clasificador3$clase <- Clasificador3$clase %>% as.factor()
Clasificador3 %>% names
Clasificador3_train <- sample_frac(Clasificador3, .8)
Clasificador3_test <- anti_join(Clasificador3,Clasificador3_train)
Clasificador3_train %>% dim #  113  12
Clasificador3_test %>% dim # 28  12
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
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador3_test)-1),
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

modelo_rf3 <- train(clase ~ ., data = Clasificador3_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)



SeleccionVariables3 <- modelo_rf3$finalModel$variable.importance %>% as.data.frame
SeleccionVariables3 <- SeleccionVariables3 %>% rownames_to_column()
colnames(SeleccionVariables3) <- c("variable","importance")

clasificador3_p1<- ggplot(SeleccionVariables3, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo Valor del jugador")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

temp3 <- SeleccionVariables3 %>% filter(importance>2.7)
# Variables seleccionadas
temp3$variable
# "G.val"  "PA.val" "Rbat"   "Rfield"
#[5] "Rpos"

# varios modelos
Clasificador3_test <- Clasificador3_test[, c(temp3$variable,"clase")]
Clasificador3_train <- Clasificador3_train[, c(temp3$variable,"clase")]
# todos son enteros

Clasificador3_train %>% names
library("modelgrid")
grid_modelos3 <- model_grid()
grid_modelos3
grid_modelos3 <- grid_modelos3 %>%
  share_settings(
    y = Clasificador3_train$clase,
    x = Clasificador3_train[,-6],
    metric = "Accuracy",
    trControl = trainControl(method = "cv",
                             number = 10,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE
    )
  )


grid_modelos3 <- grid_modelos3 %>%
  add_model(
    model_name = "Reg_logistica",
    method     = "multinom",
    tuneGrid   = expand.grid(
      decay = c(0.0001, 0.1, 0.5)
    )
  )%>%
  add_model(
    model_name = "SVM",
    method = "svmRadial",
    tuneGrid   = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 0.5, 1),
      C = c(1 , 20, 50, 100, 200)
    )
  ) %>%
  add_model(
    model_name = "RandomForest",
    method     = "ranger",
    num.trees  = 200,
    tuneGrid   = expand.grid(
      mtry = 1:(ncol(Clasificador3_train)-1),
      min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
      splitrule = "gini"
    )
  ) %>%
  add_model(
    model_name = "RedNeuronal",
    method = "nnet",
    tuneGrid   = expand.grid(size = c(10, 20, 50, 80, 100, 120),
                             decay = c(0.0001, 0.1, 0.5)
    ),
    rang = c(-0.9, 0.9),
    MaxNWts = 2000,
    # Para que no se muestre cada iteración por pantalla
    trace = FALSE
  )

grid_modelos3$models


# Se emplean 4 cores en paralelo.
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

grid_modelos3 <- train(grid_modelos3, train_all = FALSE, resample_seed = 123)
grid_modelos3$model_fits

stopCluster(cl)

clasificador3_p5 <- ggplot(grid_modelos3$model_fits$RandomForest, highlight = TRUE) +
  labs(title = "Random Forest") +
  theme_bw()

clasificador3_p4 <- ggplot(grid_modelos3$model_fits$RedNeuronal, highlight = TRUE) +
  labs(title = "Red Neuronal") +
  theme_bw()

clasificador3_p3 <- ggplot(grid_modelos3$model_fits$Reg_logistica, highlight = TRUE) +
  labs(title = "Modelo Logístico Multinomial") +
  theme_bw()

clasificador3_p2 <- ggplot(grid_modelos3$model_fits$SVM, highlight = TRUE) +
  labs(title = "SVM") +
  theme_bw()



############################
# Comparando modelos
############################

modelos3 <- list(logistic = grid_modelos3$model_fits$Reg_logistica,
                 SVMradial =  grid_modelos3$model_fits$SVM,
                 NNET =  grid_modelos3$model_fits$RedNeuronal,
                 RF =  grid_modelos3$model_fits$RandomForest)

resultados_resamples <- resamples(modelos3)
resultados_resamples$values %>% head(10)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))


clasificacion3_p7 <- metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = .5, linetype = "dashed")  +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() 

clasificacion3_p8 <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed")  +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

Clasificador3_test %>% names
predicciones <- extractPrediction(
  models = modelos3,
  testX = Clasificador3_test[, -6],
  testY = Clasificador3_test$clase
)
predicciones %>% head()

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))


metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))



clasificacion3_p9 <-ggplot(data = metricas_predicciones,
                           aes(x = reorder(object, accuracy), y = accuracy,
                               color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")

# no se clasifica con estas variables

######################
# CONCLUSIÓN
#####################
# No aporta


################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## VariablesBATEADOR TODAS
################################################################
################################################################
################################################################
################################################################
################################################################
temp<- BASE_NOnA[,c(10:30,35:51,58:72,117)]
temp %>% names
#[1] "R"         "H"         "X2B.bateo" "X3B.bateo"
#[5] "HR"        "RBI"       "SB"        "CS"       
#[9] "BB"        "SO"        "BA"        "OBP"      
#[13] "SLG"       "OPS"       "OPS."      "TB"       
#[17] "GDP"       "HBP"       "SH"        "SF"       
#[21] "IBB"       "G.val"     "PA.val"    "Rbat"     
#[25] "Rbaser"    "Rdp"       "Rfield"    "Rpos"     
#[29] "RAA"       "WAA"       "Rrep"      "RAR"      
#[33] "WAR"       "waaWL."    "X162WL."   "oWAR"     
#[37] "dWAR"      "oRAR"      "BtRuns"    "BtWins"   
#[41] "Plays"     "WPA"       "WPA."      "WPA..1"   
#[45] "aLI"       "WPA.LI"    "Clutch"    "RE24"     
#[49] "REW"       "boLI"      "RE24.boLI" "PHlev"    
#[53] "AB.win"    "clase"   

Clasificador4 <-BASE_NOnA[,c(10:30,35:51,58:72,117)]
Clasificador4$clase[Clasificador4$clase==-1] <-NA
Clasificador4 %>% dim # 273  54
Clasificador4 <- Clasificador4[complete.cases(Clasificador4),]
Clasificador4 %>% dim # 159  54

# quitandole las base son muy pocas observaciones

Clasificador4$clase <- Clasificador4$clase %>% as.factor()
Clasificador4 %>% names
Clasificador4_train <- sample_frac(Clasificador4, .8)
Clasificador4_test <- anti_join(Clasificador4,Clasificador4_train)
Clasificador4_train %>% dim #  127  54
Clasificador4_test %>% dim # 32 54
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
# Hiperparámetros
hiperparametros <- expand.grid(mtry = 1:(ncol(Clasificador4_test)-1),
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

modelo_rf4 <- train(clase ~ ., data = Clasificador4_train,
                    method = "ranger",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    importance = "impurity",
                    trControl = control_train,
                    classification =TRUE,
                    # Número de árboles ajustados
                    num.trees = 200)



SeleccionVariables4 <- modelo_rf4$finalModel$variable.importance %>% as.data.frame
SeleccionVariables4 <- SeleccionVariables4 %>% rownames_to_column()
colnames(SeleccionVariables4) <- c("variable","importance")

clasificador4_p1<- ggplot(SeleccionVariables4, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importancia")+
  xlab("")+
  ggtitle("Variables Bateo  Básicas + Estadísticas")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

temp4<- SeleccionVariables4 %>% filter(importance>1)
# Variables seleccionadas
temp4$variable
#[1] "H"      "SO"     "BA"     "GDP"   
#[5] "WPA."   "Clutch" "PHlev" 

# varios modelos
Clasificador4_test <- Clasificador4_test[, c(temp4$variable,"clase")]
Clasificador4_train <- Clasificador4_train[, c(temp4$variable,"clase")]
# todos son enteros

Clasificador4_train %>% names
library("modelgrid")
grid_modelos4 <- model_grid()
grid_modelos4 
Clasificador4_train %>% names
grid_modelos4 <- grid_modelos4 %>%
  share_settings(
    y = Clasificador4_train$clase,
    x = Clasificador4_train[,-8],
    metric = "Accuracy",
    trControl = trainControl(method = "cv",
                             number = 10,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE
    )
  )


grid_modelos4 <- grid_modelos4 %>%
  add_model(
    model_name = "Reg_logistica",
    method     = "multinom",
    tuneGrid   = expand.grid(
      decay = c(0.0001, 0.1, 0.5)
    )
  )%>%
  add_model(
    model_name = "SVM",
    method = "svmRadial",
    tuneGrid   = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 0.5, 1),
      C = c(1 , 20, 50, 100, 200)
    )
  ) %>%
  add_model(
    model_name = "RandomForest",
    method     = "ranger",
    num.trees  = 200,
    tuneGrid   = expand.grid(
      mtry = c(1, 2, 3, 4, 5),
      min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
      splitrule = "gini"
    )
  ) %>%
  add_model(
    model_name = "RedNeuronal",
    method = "nnet",
    tuneGrid   = expand.grid(size = c(10, 20, 50, 80, 100, 120),
                             decay = c(0.0001, 0.1, 0.5)
    ),
    rang = c(-0.9, 0.9),
    MaxNWts = 2000,
    # Para que no se muestre cada iteración por pantalla
    trace = FALSE
  )

grid_modelos4$models


# Se emplean 4 cores en paralelo.
library("doParallel")
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

grid_modelos4 <- train(grid_modelos4, train_all = FALSE, resample_seed = 123)
grid_modelos4$model_fits

stopCluster(cl)

clasificador4_p5 <- ggplot(grid_modelos4$model_fits$RandomForest, highlight = TRUE) +
  labs(title = "Random Forest") +
  theme_bw()

clasificador4_p4 <- ggplot(grid_modelos4$model_fits$RedNeuronal, highlight = TRUE) +
  labs(title = "Red Neuronal") +
  theme_bw()

clasificador4_p3 <- ggplot(grid_modelos4$model_fits$Reg_logistica, highlight = TRUE) +
  labs(title = "Modelo Logístico Multinomial") +
  theme_bw()

clasificador4_p2 <- ggplot(grid_modelos4$model_fits$SVM, highlight = TRUE) +
  labs(title = "SVM") +
  theme_bw()

############################
# Comparando modelos
############################

modelos4 <- list(logistic = grid_modelos4$model_fits$Reg_logistica,
                 SVMradial =  grid_modelos4$model_fits$SVM,
                 NNET =  grid_modelos4$model_fits$RedNeuronal,
                 RF =  grid_modelos4$model_fits$RandomForest)

resultados_resamples <- resamples(modelos4)
resultados_resamples$values %>% head(10)


# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))


clasificacion4_p7 <- metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = .5, linetype = "dashed")  +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() 

clasificacion4_p8 <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed")  +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

# DE TODOS TUS MODELOS LO HACE PREDICCIÓN POR OBSERVACION
# EL OTRO TE LO HACE POR CADA MODELO, ESTE LOS AGREGA
Clasificador4_test %>% names
predicciones <- extractPrediction(
  models = modelos4,
  testX = Clasificador4_test[, -8],
  testY = Clasificador4_test$clase
)
predicciones %>% head()

metricas_predicciones %>% as.data.frame %>% filter(dataType=="Training")

###### ESTO ES LO QUE HACE, CON EL MEJOR MODELO
#a <-svm(clase~.,Clasificador4_train, list=(gamma=1/.1), cost=20)
#b <- predict(a,Clasificador4_train[,-9] )
#c <- table(b,Clasificador4_train$clase )
#sum(diag(c))/sum(c)
# y con datos de prueba también
# el primer gráfico solo es elegir parámetros.


metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))


metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))



clasificacion4_p9 <-ggplot(data = metricas_predicciones,
                           aes(x = reorder(object, accuracy), y = accuracy,
                               color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")

# no se clasifica con estas variables

######################
# CONCLUSIÓN
#####################
# Si comienzo a agregar variables estad´siticas el error de entrenamiento disminuye


x11()
clasificador1_p1
clasificador1_p2
clasificador1_p3
clasificador1_p4
clasificador1_p5
clasificacion1_p7
clasificacion1_p8
clasificacion1_p9


x11()
clasificador2_p1
clasificador2_p2
clasificador2_p3
clasificador2_p4
clasificador2_p5
clasificacion2_p7
clasificacion2_p8
clasificacion2_p9

x11()
clasificador3_p1
clasificador3_p2
clasificador3_p3
clasificador3_p4
clasificador3_p5
clasificacion3_p7
clasificacion3_p8
clasificacion3_p9


x11()
clasificador4_p1
clasificador4_p2
clasificador4_p3
clasificador4_p4
clasificador4_p5
clasificacion4_p7
clasificacion4_p8
clasificacion4_p9


