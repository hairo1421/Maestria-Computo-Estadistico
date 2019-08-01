###############################
#
# Obj. II 	Predecir salario Pitcher
#
###############################

############################
# Inciso a.ii
###########################

######################################################################
# 	Con datos de campo y posición defensiva (a) 9 veces)
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
BASE_NOnA <- BASE_NOnA %>% filter(Posicion=="C")
BASE_NOnA$clase[BASE_NOnA$clase==3] <- 2
BASE_NOnA$clase[BASE_NOnA$clase==4] <- 3
#############################
# Clasificadores Multiclase
#############################

################## BASE BATEO #########################

colnames(BASE_NOnA)[1:73] # base bateo
colnames(BASE_NOnA)[117]# base salario
colnames(BASE_NOnA)[74:116] # base campo


# VARIABLES BASE CAMPO Estandart
Varibles_campo <- names(BASE_NOnA)[c(85:99)]
Varibles_campo <- c(Varibles_campo,names(BASE_NOnA)[117])
#[1] "G.st.campo" "GS.st"      "CG"        
#[4] "Inn"        "Ch"         "PO"        
#[7] "A"          "E"          "DP"        
#[10] "Rtot"       "Rtot.yr"    "Rdrs"      
#[13] "Rdrs.yr"    "RF.9"       "RF.G"      
#[16] "clase" 

#####################################
# Selección de variables con arbol
######################################
Clasificador1 <-BASE_NOnA[,Varibles_campo]
Clasificador1$clase[Clasificador1$clase==-1] <-NA
Clasificador1 <- Clasificador1[complete.cases(Clasificador1),]
Clasificador1 %>% dim #183

Clasificador1$clase <- Clasificador1$clase %>% as.factor()
Clasificador1 %>% names
Clasificador1_train <- sample_frac(Clasificador1, .8)
Clasificador1_test <- anti_join(Clasificador1,Clasificador1_train)
Clasificador1_train %>% dim # 146   16
Clasificador1_test %>% dim #  37  16

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
library("caret")
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


temp1 <- SeleccionVariables1 %>% filter(importance>4.5)
# Variables seleccionadas
temp1$variable
#[1] "G.st.campo" "GS.st"      "CG"        
#[4] "Inn"        "Ch"         "PO"        
#[7] "Rtot.yr"    "Rdrs"       "Rdrs.yr"   
#[10] "RF.9"       "RF.G" 

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
    x = Clasificador1_train[,-12],
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
  testX = Clasificador1_test[, -12],
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
## Variables CAMPO TODAS LAS VARIABLES
################################################################
################################################################
################################################################
################################################################
################################################################


# VARIABLES BASE CAMPO Estandart
Varibles_campo <- names(BASE_NOnA)[c(85:99)]
Varibles_campo <- c(Varibles_campo,names(BASE_NOnA)[117])
#[1] "G.st.campo" "GS.st"      "CG"        
#[4] "Inn"        "Ch"         "PO"        
#[7] "A"          "E"          "DP"        
#[10] "Rtot"       "Rtot.yr"    "Rdrs"      
#[13] "Rdrs.yr"    "RF.9"       "RF.G"      
#[16] "clase" 

#####################################
# Selección de variables con arbol
######################################
Clasificador2 <-BASE_NOnA[,Varibles_campo]
Clasificador2$clase[Clasificador2$clase==-1] <-NA
Clasificador2 <- Clasificador2[complete.cases(Clasificador2),]
Clasificador2 %>% dim #183

Clasificador2$clase <- Clasificador2$clase %>% as.factor()
Clasificador2 %>% names
Clasificador2_train <- sample_frac(Clasificador2, .8)
Clasificador2_test <- anti_join(Clasificador2,Clasificador2_train)
Clasificador2_train %>% dim # 146   16
Clasificador2_test %>% dim #  37  16
# CLASIFICADOR

Clasificador2_train %>% names
library("modelgrid")
grid_modelos2 <- model_grid()
grid_modelos2
grid_modelos2 <- grid_modelos2 %>%
  share_settings(
    y = Clasificador2_train$clase,
    x = Clasificador2_train[,-16],
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
  testX = Clasificador2_test[, -16],
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

clasificador2_p2
clasificador2_p3
clasificador2_p4
clasificador2_p5
clasificacion2_p7
clasificacion2_p8

clasificacion2_p9

# Así compruebas el modelo
modelos2$RF$finalModel
a <- ranger::ranger(clase~.,Clasificador2_train, mtry=1, num.trees=200,
                    min.node.size =20, splitrule = "gini")
b <- predict(a, Clasificador2_test[,-16])
c <- table(b$predictions,Clasificador2_test$clase)
sum(diag(c))/sum(c)
