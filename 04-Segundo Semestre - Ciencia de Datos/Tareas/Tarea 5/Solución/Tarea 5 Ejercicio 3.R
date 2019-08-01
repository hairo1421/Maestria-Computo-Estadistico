############################################
############################################
############################################
#####   Tarea 5. Ciencia de datos
#####   Ejercicio 3. TSpam Clasificador
#####   Miranda Belmonte Hairo Ulise
#####   04 de Marzo del 2019
############################################
############################################
############################################

#################
#################
## Librerías
#################
#################

library("tm") # manipulación de texto
library("magrittr") # pipes
library("gridExtra") # plot 
library("RColorBrewer") # word cloud
library("wordcloud")  # word cloud
library("ggplot2") # visualización
library("tm.plugin.mail") # texto email
library("MASS") # lda; qda 
library("ROCR") # curva rock

##########################
##########################
##### Inciso a ###########
##########################
##########################

# Ingrese la ruta  de la dirección de los archivos de entrenamiento
rutTrain <- "C:/Users/h_air/Downloads/Tarea 5-20190404/email_train/email_train"
# corpus de textos entrenamiento
corpTrain <- Corpus(DirSource(rutTrain,recursive=TRUE),
               readerControl=list(language="en_US",reader=readMail))

# Generamos etiqueta
# 1400 elementos no spam -> valor cero
# 800 elementos spam -> valor uno
index1 <- rep(1,800) %>% as.data.frame
index2 <- rep(0,1400) %>% as.data.frame
spam <- rbind(index1,index2)


# Ingrese ruta de la dirección de los archivos de prueba
rutTest <- "C:/Users/h_air/Downloads/Tarea 5-20190404/email_test"
# corpus de textos de prueba
corpTest <- Corpus(DirSource(rutTest,recursive=TRUE),
                    readerControl=list(language="en_US",reader=readMail))

##############
# Pre proceso
##############

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

# limpia corpus de documentos de entrenamiento y de prueba
corpTrain <- limpiar(corpTrain)
corpTest <- limpiar(corpTest)

#  Corpus a matriz de terminos
train <-  TermDocumentMatrix(corpTrain) # archivos a matriz
test <-  TermDocumentMatrix(corpTest) # archivos a matriz

# remueve .1 de "sparsity"
train <- removeSparseTerms(train,.90) # eliminamos algo de esparsidad
# 82% esparcidad
test <- removeSparseTerms(test,.90) # eliminamos algo de esparsidad
# 84% esparcidad

# Se toman los 25 términos más frecuentes
# Se tienen 25 variable independientes

# Términos más frecuentes datos entrenamientos
term.freq <- rowSums(as.matrix(train)) # términos más frecuentes
sort.freq <- sort.int(term.freq, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp <- names(sort.freq$x[1:25]) # nombre


# Términos más frecuentes datos prueba
term.freq2 <- rowSums(as.matrix(test)) # términos más frecuentes
sort.freq2 <- sort.int(term.freq2, decreasing = TRUE, index.return = TRUE) # ordenamos los tperminos frecuentes
nterms.corp2 <- names(sort.freq2$x) # nombre

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
train<- count.terms(train,nterms.corp)
test2<- count.terms(test,nterms.corp2)

# Se entrena con datos de entrenamiento, por lo tanto
# variables que estimen el modelo son los términos 
# de los datos en entrenamiento. Se seleccionan esas mismas
# en la base de prueba
test2 <- test2 %>% as.data.frame
test2 <- test2[,nterms.corp[1:25]]   %>% as.matrix

# Estos son los nombres de los términos más frecuentes
set.seed(1234)
wordcloud(names(term.freq), term.freq,
          max.words = 25, scale = c(3, .2), colors =  c("grey80","darkgoldenrod1", "tomato"))

# Prepara la base de entrenamiento
emailsSparse <- as.data.frame(as.matrix(train)) # como dataframe
colnames(emailsSparse) <- make.names(colnames(emailsSparse)) # extrae nombres
colnames(spam) <- "spam2" # pon nombre
emailsSparse2 <- cbind(spam,emailsSparse)
emailsSparse2$spam2 <- as.factor(emailsSparse2$spam2) # variable dependiente como factor


###############
# Modelo Logit
###############

# Se ajusta modelo con datos de entrenamiento
spamLogaritmo <- glm(spam2~., data=as.data.frame(emailsSparse2), family="binomial")
summary(spamLogaritmo)
# AIC_ 1915.4

# R help indica el problema del mesaje que arroja al utilizar la
# función anteriro:

# did not converge and fitted probabilities numerically 0 or 1 
#occurred. Both of these messages often indicate overfitting and
# the first indicates particularly severe overfitting.
# predictions


# Valores ajustados
glm.probs <- predict(spamLogaritmo, as.data.frame(emailsSparse2), type="response")
# Tabla de confución
table(emailsSparse2$spam2, ifelse(glm.probs > 0.5, 1, 0))
# accuracy rate de entrenamiento
mean(ifelse(glm.probs > 0.5, 1,0) == emailsSparse2$spam2)
##  0.8272727

# error rate de entrenamiento
mean(ifelse(glm.probs > 0.5, 1, 0) != emailsSparse2$spam2)
## [1] 0.1727273

# Datos de Prueba
Prueba <- as.data.frame(as.matrix(test2))
colnames(Prueba) = make.names(colnames(Prueba))
# índicador
# 500 elementos no spam -> son los ceros
# 500 elementos spam -> son los unos
index1_prueba <- rep(1,500) %>% as.data.frame
index2_prueba <- rep(0,500) %>% as.data.frame
spam_prueba <- rbind(index1_prueba,index2_prueba)
colnames(spam_prueba) <- "spam2" 
Prueba2 <- cbind(spam_prueba,Prueba)

# logit modelo con datos de prueba
glm.probs <- predict(spamLogaritmo, as.data.frame(Prueba2), type="response")
# tabla de confución
table(Prueba2$spam2, ifelse(glm.probs > 0.5, 1, 0))
# accuracy rate
mean(ifelse(glm.probs > 0.5, 1,0) == Prueba2$spam2)
##  0.567
# error rate
mean(ifelse(glm.probs > 0.5, 1, 0) != Prueba2$spam2)
## [1]  0.433



##########
# LDA 
##########
lda.fit <- lda(spam2~., data=as.data.frame(emailsSparse2))
spamLogaritmo %>% summary
# AIC: 1915.4

# predictions datos de entrenamiento
test.predicted.lda <- predict(lda.fit, newdata=as.data.frame(emailsSparse2))
# tabla de confución
table(emailsSparse2$spam2, test.predicted.lda$class)
# accuracy rate datos de entrenamiento
mean(test.predicted.lda$class == emailsSparse2$spam2)
## [1] 0.7972727

# error rate de entrenamiento
mean(test.predicted.lda$class != emailsSparse2$spam2)
## [1] 0.2027273


# LDA modelo con datos de prueba
test.predicted.lda <- predict(lda.fit, newdata=as.data.frame(Prueba2))
# tabla de confución
table(Prueba2$spam2, test.predicted.lda$class)
# accuracy rate
mean(as.numeric(as.character(test.predicted.lda$class)) == Prueba2$spam2)
## [1] 0.525
# error rate
mean(test.predicted.lda$class != Prueba2$spam2)
## [1] 0.475
x11()
ldahist(test.predicted.lda$posterior[,1], g= test.predicted.lda$class)
par(mfrow=c(1,1))
plot(test.predicted.lda$posterior[,1],test.predicted.lda$class, col=Prueba2$spam2+10)

ggplot( ) + aes(x=test.predicted.lda$posterior[,1],y=test.predicted.lda$class) +
  geom_point(col=as.factor(Prueba2$spam2+1)) +
  labs(title="LDA",
       x="Prob.Posterior",
       y="clase",
       caption="Negro: No-Spam; Rojo: Spam")





################
# LDQ Quadratic
################
qda.fit <- qda(spam2~., data=as.data.frame(emailsSparse2))


# predictions datos de entrenamiento
test.predicted.qda <- predict(qda.fit, newdata=as.data.frame(emailsSparse2))
# tabla de confución
table(emailsSparse2$spam2, test.predicted.qda$class)
# accuracy rate de entrenamiento
mean(test.predicted.qda$class == emailsSparse2$spam2)
## [1] 0.6086364
# error rate de entrenamiento
mean(test.predicted.qda$class != emailsSparse2$spam2)
## [1] 0.3913636


# LDQ modelo con datos de prueba
test.predicted.qda <- predict(qda.fit, newdata=as.data.frame(Prueba2))
# tabla de confución
table(Prueba2$spam2, test.predicted.qda$class)
# accuracy rate de ´prueba
mean(test.predicted.qda$class == Prueba2$spam2)
## [1] 0.737
# error rate de prueba
mean(test.predicted.qda$class != Prueba2$spam2)
## [1] 0.263
x11()
ldahist(test.predicted.qda$posterior[,1], g= test.predicted.qda$class)
par(mfrow=c(1,1))
plot(test.predicted.qda$posterior[,1],test.predicted.qda$class, col=Prueba2$spam2+10)

ggplot( ) + aes(x=test.predicted.qda$posterior[,1],y=test.predicted.qda$class) +
  geom_point(col=as.factor(Prueba2$spam2+1)) +
  labs(title="LDQ",
       x="Prob.Posterior",
       y="clase",
       caption="Negro: No-Spam; Rojo: Spam")


#################
# Fisher con lm
#################
lm.fit <- lm(as.numeric(spam2)~., data=as.data.frame(emailsSparse2))
lm.fit %>% summary
AIC(lm.fit)
# AIC : 2437.415
# predictions
test.predicted.lm <- predict(lm.fit, newdata=as.data.frame(emailsSparse2))

table(emailsSparse2$spam2,  ifelse(test.predicted.lm> 0.5, 1,0) )
# accuracy rate
mean( ifelse(test.predicted.lm> 0.5, 1,0) == emailsSparse2$spam2)
## [1] 0.3677273

# error rate
mean( ifelse(test.predicted.lm> 0.5, 1,0) != emailsSparse2$spam2)
## [1] 0.6322727


# predictions Prueba
test.predicted.lm <- predict(lm.fit, newdata=as.data.frame(Prueba2))
# tabla confución
table(Prueba2$spam2,  ifelse(test.predicted.lm> 0.5, 1,0) )
# accuracy rate prueba
mean( ifelse(test.predicted.lm> 0.5, 1,0) == Prueba2$spam2)
## [1] 0.504
# error rate prueba
mean( ifelse(test.predicted.lm> 0.5, 1,0) != Prueba2$spam2)
## [1] 0.496

##########################
##########################
##### FIN Inciso a #######
##########################
##########################

##########################
##########################
##### Inciso b ###########
##########################
##########################

#############
# ROC curves
#############
# lm
p0 <- prediction(test.predicted.lm, Prueba2$spam2) %>%
  performance(measure = "tpr", x.measure = "fpr")
auc0 <- as.numeric(performance(prediction(test.predicted.lm, Prueba2$spam2) ,"auc")@y.values)

# logic
p1 <- prediction(glm.probs, Prueba2$spam2) %>%
  performance(measure = "tpr", x.measure = "fpr")
auc1 <- as.numeric(performance(prediction(glm.probs, Prueba2$spam2) ,"auc")@y.values)

# lda
p2 <- prediction(test.predicted.lda$posterior[,2], Prueba2$spam2) %>%
  performance(measure = "tpr", x.measure = "fpr")
auc2 <- as.numeric(performance(prediction(test.predicted.lda$posterior[,2], Prueba2$spam2) ,"auc")@y.values)


# ldq
p3 <- prediction(test.predicted.qda$posterior[,2], Prueba2$spam2) %>%
  performance(measure = "tpr", x.measure = "fpr")

auc3 <- as.numeric(performance(prediction(test.predicted.qda$posterior[,2], Prueba2$spam2) ,"auc")@y.values)



uno <- cbind(as.data.frame(p0@x.values),as.data.frame(p0@y.values))
colnames(uno) <- c("x","y")
dos <- cbind(as.data.frame(p1@x.values),as.data.frame(p1@y.values))
colnames(dos) <- c("x","y")
tres <- cbind(as.data.frame(p2@x.values),as.data.frame(p2@y.values))
colnames(tres) <- c("x","y")
cuatro <- cbind(as.data.frame(p3@x.values),as.data.frame(p3@y.values))
colnames(cuatro) <- c("x","y")
# roja logit
# morada lda
# azul qlda
plot1 <- ggplot() + aes(x=dos$x, y=dos$y) + geom_line(col="red") +
     geom_line(aes(x=tres$x, y=tres$y), col="purple") +
      geom_line(aes(x=cuatro$x, y=cuatro$y), col="blue") +
      labs(title="Curvas ROCs", 
           x="Falso positivo tasa",
           y="Verdadero positivo tasa",
           caption="Rojo:Logistica; Morado: LDA, Azul: LDQ") +
        geom_abline(slope=1, intercept = 0, size=.5,linetype = 2)

X11()
plot1

plot2 <- ggplot() + aes(x=uno$x, y=uno$y) + geom_line() +
  labs(title="Curvas ROCs", 
       x="Falso positivo tasa",
       y="Verdadero positivo tasa", 
       subtitle = "FDA",
       cption="Se utiliza solución MCO") +
  geom_abline(slope=1, intercept = 0, size=.5,linetype = 2)

plot3 <- ggplot() + aes(x=dos$x, y=dos$y) + geom_line(col="red") +
  labs(title="Curvas ROCs", 
       x="Falso positivo tasa",
       y="Verdadero positivo tasa", 
       subtitle = "Logistica",
       caption="") +
  geom_abline(slope=1, intercept = 0, size=.5,linetype = 2)

plot4 <- ggplot() + aes(x=tres$x, y=tres$y) + geom_line(col="purple") +
  labs(title="Curvas ROCs", 
       x="Falso positivo tasa",
       y="Verdadero positivo tasa", 
       subtitle = "LDA",
       caption="") +
  geom_abline(slope=1, intercept = 0, size=.5,linetype = 2)

plot5 <- ggplot() + aes(x=cuatro$x, y=cuatro$y) + geom_line(col="blue") +
  labs(title="Curvas ROCs", 
       x="Falso positivo tasa",
       y="Verdadero positivo tasa", 
       subtitle = "LDQ",
       caption="") +
  geom_abline(slope=1, intercept = 0, size=.5,linetype = 2)

X11()
grid.arrange(plot2, plot3, plot4, plot5, nrow=2)

##########################
##########################
##### FIN Inciso b #######
##########################
##########################
