###############################
#
# Obj. II 	Predecir salario Pitcher 
#
###############################

############################
# Inciso c
###########################

######################################################################
# 	Con datos de pitcher clasificar salarios
######################################################################

# CLASIFICADOR 1  Variables base Pitcher

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
# todos menos bateador
base_picheo<- read.csv("dat_picheo_RL.csv", head=T) 
base_picheo


##############################
# VARIABLE SALARIO CONTINUA
##############################




# filtrar salario y hacerlo numerico
salarios<- base_picheo$Salary
salarios<- as.character(salarios)
salarios %>% head
i <- 1
for(i in 1:length(salarios)) {
  if(nchar(salarios[i])>0) {
    salarios[i] <- substr(salarios[i], 2, nchar(salarios[i])) 
    print(salarios[i])
  }
}
salarios<- as.numeric(salarios)
salarios
sum(is.na(salarios)) # 1845
sum(base_picheo$Salary=="")
base_picheo$Salary<-salarios
base_picheo$Salary %>% length() # 2299
##############################
# VARIABLE SALARIO CLASES
##############################

clase<-rep(-1,times=nrow(base_picheo))
quantiles <- base_picheo$Salary %>% na.omit %>% quantile(c(.25,.50,.75))
# 557125 1500000 5075000 


for(i in 1:nrow(base_picheo)) {
  if(!is.na(base_picheo$Salary[i])){
    if(base_picheo$Salary[i]<quantiles[1]){
      clase[i]<- 1
    }
    else {
      if(base_picheo$Salary[i]<quantiles[2]){
        clase[i]<- 2
      }
      else {
        if(base_picheo$Salary[i]<quantiles[3]){
          clase[i]<-3
        }
        else {
          clase[i]<-4
        }
      }
    }
  }
}
### Creacion de clase por salario
summary(base_picheo$Salary)
BASE <- cbind(base_picheo,clase)
library("xlsx")
# Write the first data set in a new workbook
write.xlsx(BASE, file = "Base Clasificador Pitcher.xlsx" , append = FALSE)

BASE %>% names
# En caso de importar la base, quitarle la primera columna
#BASE_NOnA <- Base_sin_valores_perdidos[,-1]

BASE <- BASE %>% as.data.frame()
# Cambiar la etiqueta de la clase
BASE$clase[BASE$clase==-1] <- NA
BASE$clase[BASE$clase==3] <- 2
BASE$clase[BASE$clase==4] <- 3

#############################
# Clasificadores Multiclase
#############################


# VARIABLES Pitcher Estandart
Varibles_base <- c(names(BASE)[7:36],"clase")
#[1] "W"     "L"     "W.L."  "ERA"  
#[5] "G.st"  "GS.st" "GF"    "CG"   
#[9] "SHO"   "SV"    "IP.st" "H"    
#[13] "R.st"  "ER"    "HR"    "BB"   
#[17] "IBB"   "SO"    "HBP"   "BK"   
#[21] "WP"    "BF"    "ERA."  "FIP"  
#[25] "WHIP"  "H9"    "HR9"   "BB9"  
#[29] "SO9"   "SO.W" "clase"

#####################################
# Selección de variables con arbol
######################################
Clasificador1 <-BASE[,Varibles_base]
Clasificador1 <- Clasificador1[complete.cases(Clasificador1),]
Clasificador1 %>% dim # 393  31

Clasificador1$clase <- Clasificador1$clase %>% as.factor()
Clasificador1 %>% names
Clasificador1_train <- sample_frac(Clasificador1, .8)
Clasificador1_test <- anti_join(Clasificador1,Clasificador1_train)
Clasificador1_train %>% dim # 314  31
Clasificador1_test %>% dim #  79 31


Clasificador1_train %>% names
library("modelgrid")
grid_modelos <- model_grid()
grid_modelos
grid_modelos <- grid_modelos %>%
  share_settings(
    y = Clasificador1_train$clase,
    x = Clasificador1_train[,-31],
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
  testX = Clasificador1_test[, -31],
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
BASE[40:57] %>% names
# VARIABLES Pitcher Estandart
Varibles_Estadisticas <- c(names(BASE)[40:57],"clase")
#[1] "IP.val"  "G.val"   "GS.val"  "R.val"  
#[5] "RA9"     "RA9opp"  "RA9def"  "RA9role"
#[9] "PPFp"    "RA9avg"  "RAA"     "WAA"    
#[13] "gmLI"    "WAAadj"  "WAR"     "RAR"    
#[17] "waaWL."  "X162WL." "clase"


#####################################
# Selección de variables con arbol
######################################
Clasificador2 <-BASE[,Varibles_Estadisticas]
Clasificador2 <- Clasificador2[complete.cases(Clasificador2),]
Clasificador2 %>% dim # 342  19

Clasificador2$clase <- Clasificador2$clase %>% as.factor()
Clasificador2 %>% names
Clasificador2_train <- sample_frac(Clasificador2, .8)
Clasificador2_test <- anti_join(Clasificador2,Clasificador2_train)
Clasificador2_train %>% dim # 274  19
Clasificador2_test %>% dim #  68 19


# CLASIFICADOR
Clasificador2_train %>% names
library("modelgrid")
grid_modelos2 <- model_grid()
grid_modelos2
grid_modelos2 <- grid_modelos2 %>%
  share_settings(
    y = Clasificador2_train$clase,
    x = Clasificador2_train[,-19],
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
      mtry = 1:(ncol(Clasificador2_train)-1),
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
  testX = Clasificador2_test[, -19],
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

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## Variables Win probability
################################################################
################################################################
################################################################
################################################################
################################################################
BASE[62:79] %>% names 

# filtrar variables
Variables_Valor<- c(colnames(BASE[c(62:79)]),"clase")
#[1] "IP"        "PtchR"    
#[3] "PtchW"     "Plays"    
#[5] "WPA"       "WPA."     
#[7] "WPA..1"    "aLI"      
#[9] "WPA.LI"    "Clutch"   
#[11] "RE24"      "REW"      
#[13] "boLI"      "RE24.boLI"
#[15] "LevHi"     "LevMd"    
#[17] "LevLo"     "THROWS",  "clase"    



#####################################
# Selección de variables con arbol
######################################
Clasificador3 <-BASE[,Variables_Valor]
Clasificador3 <- Clasificador3[complete.cases(Clasificador3),]
Clasificador3 %>% dim # 402  19

Clasificador3$clase <- Clasificador3$clase %>% as.factor()
Clasificador3 %>% names
Clasificador3 <-Clasificador3[,-18]
Clasificador3_train <- sample_frac(Clasificador3, .8)
Clasificador3_test <- anti_join(Clasificador3,Clasificador3_train)
Clasificador3_train %>% dim # 322  18
Clasificador3_test %>% dim #  80 18


# CLASIFICADOR
Clasificador3_train %>% names
library("modelgrid")
grid_modelos3 <- model_grid()
grid_modelos3
grid_modelos3 <- grid_modelos3 %>%
  share_settings(
    y = Clasificador3_train$clase,
    x = Clasificador3_train[,-18],
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
  testX = Clasificador3_test[, -18],
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

################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
## Variables Todas
################################################################
################################################################
################################################################
################################################################
################################################################
BASE[62:79] %>% names 
BASE[41:57] %>% names
BASE[7:36] %>% names # agrega salario

Variables_todas <-c(names(BASE[62:78]), names(BASE[41:57]), names(BASE[7:36]), "clase")
Variables_todas <- Variables_todas[-c(39, 40, 45, 47)]
#[1] "IP"        "PtchR"     "PtchW"    
#[4] "Plays"     "WPA"       "WPA."     
#[7] "WPA..1"    "aLI"       "WPA.LI"   
#[10] "Clutch"    "RE24"      "REW"      
#[13] "boLI"      "RE24.boLI" "LevHi"    
#[16] "LevMd"     "LevLo"     "G.val"    
#[19] "GS.val"    "R.val"     "RA9"      
#[22] "RA9opp"    "RA9def"    "RA9role"  
#[25] "PPFp"      "RA9avg"    "RAA"      
#[28] "WAA"       "gmLI"      "WAAadj"   
#[31] "WAR"       "RAR"       "waaWL."   
#[34] "X162WL."   "W"         "L"        
#[37] "W.L."      "ERA"       "GF"       
#[40] "CG"        "SHO"       "SV"       
#[43] "H"         "ER"        "HR"       
#[46] "BB"        "IBB"       "SO"       
#[49] "HBP"       "BK"        "WP"       
#[52] "BF"        "ERA."      "FIP"      
#[55] "WHIP"      "H9"        "HR9"      
#[58] "BB9"       "SO9"       "SO.W"     
#[61] "clase"  


#####################################
# Selección de variables con arbol
######################################
Clasificador4 <-BASE[,Variables_todas]
Clasificador4 <- Clasificador4[complete.cases(Clasificador4),]
Clasificador4 %>% dim # 285  61

Clasificador4$clase <- Clasificador4$clase %>% as.factor()
Clasificador4 %>% names
Clasificador4_train <- sample_frac(Clasificador4, .8)
Clasificador4_test <- anti_join(Clasificador4,Clasificador4_train)
Clasificador4_train %>% dim # 228  61
Clasificador4_test %>% dim #  57 61


# CLASIFICADOR
Clasificador4_train %>% names
library("modelgrid")
grid_modelos4 <- model_grid()
grid_modelos4
grid_modelos4 <- grid_modelos4 %>%
  share_settings(
    y = Clasificador4_train$clase,
    x = Clasificador4_train[,-61],
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
      mtry = 1:(ncol(Clasificador4_train)-1),
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

Clasificador4_test %>% names
predicciones <- extractPrediction(
  models = modelos4,
  testX = Clasificador4_test[, -61],
  testY = Clasificador4_test$clase
)
predicciones %>% head()

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



##############################
# Plots
##############################
x11()

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

x11()

clasificador3_p2
clasificador3_p3
clasificador3_p4
clasificador3_p5
clasificacion3_p7
clasificacion3_p8
clasificacion3_p9


x11()

clasificador4_p2
clasificador4_p3
clasificador4_p4
clasificador4_p5
clasificacion4_p7
clasificacion4_p8
clasificacion4_p9
