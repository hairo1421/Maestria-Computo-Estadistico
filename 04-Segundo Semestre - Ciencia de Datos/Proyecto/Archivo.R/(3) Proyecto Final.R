###############################
# Library
###############################

library("tm")
library("magrittr")
library("tidyverse")
library("NMF")
library("knitr")
library("kableExtra")
library("ggplot2")
library("plotly")

##########################################
# Pre-procesamiennto de las minutas
##########################################

getwd()
setwd( "C:/Users/h_air/Desktop/Minutas/England/txt/1997Sep-2014Febrero")
# Ruta de la dirección de los archivos
rut.data <- "C:/Users/h_air/Desktop/Minutas/England/txt/1997Sep-2014Febrero"
# corpus de textos
corp <- Corpus(DirSource(rut.data,recursive=TRUE),readerControl=list(language="en"))

# retirar ciertas palabras que no aportan al análisis
rm_add_words <- c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY","JUNE","JULY",
                  "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
                  "eg","YEAR")

rm_add_words2 <- c("January", "February", "March","April","May","June","July",
                   "August","September","October","November","December","month",
                   "year", "Year")
corp <- tm_map(corp, removeWords, rm_add_words)
corp <- tm_map(corp, removeWords, rm_add_words2)

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
  corp <- tm_map(corp,removeWords,stopwords("en")) # stopwords
  corp <- tm_map(corp,stemDocument, language="english")  # stem
  return(corp)
} # end limpia



# Limpiar
corp <- limpiar(corp)
rm_add_words3 <-c("facil","stephen", "blanchflow","sentanc","giev","spare", "elev","posen","fisher","martin","weal","vat" ,                  
                  "dale","spencer" ,"broadbent","paul","adam","spare","annex",            
                  "figur","shown","per", "ago" , "grown","mean" ,"now",
                  "besley", "tim", "lomax","rachel", "barker", "kate",  
                  "john" , "andrew" , "premia", "edg" ,"ric" ,"mile",
                  "lambert","cunliff", "richard", "jon")               

corp <- tm_map(corp, removeWords, rm_add_words3)

##############################
# BAG OF WORDS
#############################

# tokennizar
doc_term2 <- DocumentTermMatrix(corp, control = list(weighting = function(x) weightTfIdf(x, normalize = T)))
doc_term <- DocumentTermMatrix(corp)
#doc_term2 <- tm::weightTfIdf(doc_term)

tdm <- removeSparseTerms(doc_term,.90) 
term.freq <- rowSums(as.matrix(tdm)) 

tdm2 <- removeSparseTerms(doc_term2,.90) 
term.freq2 <- rowSums(as.matrix(tdm2)) 

########################
# Representación nmf
########################

#############
# K=3
#############
set.seed(1)
# Matriz de df
nmf3_1 <- nmf(as.matrix(tdm), 3,"lee") 
set.seed(1)
nmf4_1 <- nmf(as.matrix(tdm), 4,"lee") 
set.seed(1)
nmf5_1 <- nmf(as.matrix(tdm), 5,"lee") 

# Matriz tf-idm
set.seed(1)
nmf3_2 <- nmf(as.matrix(tdm2), 3,"lee") 
set.seed(1)
nmf4_2 <- nmf(as.matrix(tdm2), 4,"lee") 
set.seed(1)
nmf5_2 <- nmf(as.matrix(tdm2), 5,"lee") 


w3_1 <- basis(nmf3_1)
h3_1 <- coef(nmf3_1) 
w4_1 <- basis(nmf4_1)
h4_1 <- coef(nmf4_1) 
w5_1 <- basis(nmf5_1)
h5_1 <- coef(nmf5_1) 

w3_2 <- basis(nmf3_2)
h3_2 <- coef(nmf3_2) 
w4_2 <- basis(nmf4_2)
h4_2 <- coef(nmf4_2) 
w5_2 <- basis(nmf5_2)
h5_2 <- coef(nmf5_2) 

######################
# Evolución minutas
######################


summary(corp)
a <- DocumentTermMatrix(corp)
a$ncol
pterminos <- rowSums(as.matrix(a))

getwd()
setwd( "C:/Users/h_air/Desktop/Minutas/England/txt/1997Sep-2014Febrero")
# Ruta de la dirección de los archivos
rut.data <- "C:/Users/h_air/Desktop/Minutas/England/txt/1997Sep-2014Febrero"
# corpus de textos
corp2 <- Corpus(DirSource(rut.data,recursive=TRUE),readerControl=list(language="en"))

summary(corp2)
a2 <- DocumentTermMatrix(corp2)
a2$ncol
pterminos2 <- rowSums(as.matrix(a2))


######################
# Tasa interes date
######################

# DATES TASA DE INTERES
setwd( "C:/Users/h_air/Desktop/Minutas/England")
library(readxl)
datos <- read_excel("Contraste.xlsx")
datos$DATE <- datos$DATE %>% as.Date()
datos <- datos %>% as.data.frame 
# Filtrar fecha que no se necesita
datos <- datos %>% filter(!DATE=="1998-10-21")

library("ggthemes")
# Tasa de interes
datos %>% filter(DATE>="1998-06-06" & DATE<="2014-03-19") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() + labs(title="Tasa de Interes; 1998-06-06 al 2014-03-19",
                     x="Años",
                     y="%") + theme_economist()

x11()
p1 <- datos %>% filter(DATE>="1998-06-06" & DATE<="2014-03-19") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line(col="black")  +
  labs(title="Tasa de Interes; 1998-06-06 al 2014-03-19",
                     x="Años",
                     y="%")
x11()
  p2 <- datos %>% filter(DATE>="1998-06-06" & DATE<="2014-03-19") %>% ggplot( aes(x=DATE, y=HOY,col="black")) +
  geom_point()   +
  geom_line( aes(x=DATE, y=ANTES) ) + 
  geom_line( aes(x=DATE, y=DESPUES, col="blue"))  + 
  scale_color_hue(l=40, c=35, name = "", labels = c("MINUTAS","ANTES", "DESPUES")) +
  labs(title="Tasa de Interes; 1998-06-06 al 2014-03-19",
       x="Años",
       y="%",
       caption = "Los puntos son las  fechas de las minutas")


datos$DATE %>% tail(100)



base2 <- datos %>% filter(DATE>="1997-10-15" &DATE<="2014-02-19")
base0 <- datos %>% filter(DATE>="1997-10-15" &DATE<="2014-02-19")
base0 <- base0$DATE
base0 <- base0 %>% sort()
pterminos %>% length()

bas <- cbind(as.data.frame(base0),as.data.frame(pterminos),as.data.frame(pterminos2))
x11()
p3 <- bas %>%  ggplot( aes(x=bas$base0, y=bas$pterminos,col="blue")) +
  geom_line() + geom_line(aes(x=bas$base0, y=bas$pterminos2,col="green")) +
  scale_color_hue(l=40, c=35, name = "", labels = c("Bruto", "Preproceso")) +
  labs(x="", y="# palabras") 

###########################
# Terminos en topicos
##########################

w1_k3 <- sort(h3_2[1,], decreasing = T) %>% head(15) %>% names
w2_k3 <- sort(h3_2[2,], decreasing = T) %>% head(15) %>% names
w3_k3 <- sort(h3_2[3,], decreasing = T) %>% head(15) %>% names

w1_k4 <- sort(h4_2[1,], decreasing = T) %>% head(15) %>% names
w2_k4 <- sort(h4_2[2,], decreasing = T) %>% head(15) %>% names
w3_k4 <- sort(h4_2[3,], decreasing = T) %>% head(15) %>% names
w4_k4 <- sort(h4_2[4,], decreasing = T) %>% head(15) %>% names

w1_k5 <- sort(h5_2[1,], decreasing = T) %>% head(15) %>% names
w2_k5 <- sort(h5_2[2,], decreasing = T) %>% head(15) %>% names
w3_k5 <- sort(h5_2[3,], decreasing = T) %>% head(15) %>% names
w4_k5 <- sort(h5_2[4,], decreasing = T) %>% head(15) %>% names
w5_k5 <- sort(h5_2[5,], decreasing = T) %>% head(15) %>% names

rank <- data.frame(Rank=1:15)
Tabla1 <- cbind(rank, w1_k3,w2_k3,w3_k3,
                   w1_k4,w2_k4,w3_k4,w4_k4,
                   w1_k5,w2_k5,w3_k5,w4_k5,w5_k5) 
x11()
colnames(Tabla1) <- c("Rank", "W1 K=3","W2 K=3","W3 K=3",
                      "W1 K=4","W2 K=4","W3 K=4","W4 K=4",
                      "W1 K=5","W2 K=5","W3 K=5","W4 K=5","W5 K=5")

Tabla1[1:10,] %>% kable() %>% kableExtra::kable_styling()

w1_k3%in%w1_k4 %>% mean #0
w1_k3%in%w1_k5 %>% mean #0

w2_k3%in%w2_k4 %>% mean *100#0.06666667
w2_k3%in%w2_k5 %>% mean  #0.7333333

w2_k3%in%w1_k4 %>% mean # 0.6666667
w1_k3%in%w2_k4 %>% mean # 0.3333333

w2_k3%in%w1_k5 %>% mean #  0.06666667
w1_k3%in%w2_k5 %>% mean # 0

w3_k3%in%w3_k4 %>% mean *100 #.1
w3_k3%in%w3_k5 %>% mean  #.46

w1_k3%in%w5_k5 %>% mean  


w4_k4%in%w4_k5 %>% mean # 1


###########################
#  Visualización 3 topicos
###########################
term3_1 <- sort(h3_2[1,], decreasing = T)
term3_2 <- sort(h3_2[2,], decreasing = T)
term3_3 <- sort(h3_2[3,], decreasing = T)

term3_1 %>% names
library(wordcloud)
require(RColorBrewer)
x11()
set.seed(1234)
pal2 <- brewer.pal(13,"Dark2")
wordcloud(names(term3_1[1:15]), term3_1[1:15],
          max.words = Inf, scale = c(6, .2), rot.per=.15, colors=pal2,random.order=FALSE)
set.seed(1234)
 wordcloud(names(term3_2[1:15]), term3_2[1:15],
           max.words = Inf, scale = c(6, .2), rot.per=.15, colors=pal2,random.order=FALSE)
set.seed(1234)
wordcloud(names(term3_3[1:15]), term3_3[1:15],
          max.words = Inf, scale = c(6, .2), rot.per=.15, colors=pal2,random.order=FALSE)

#############################
# PESOS DE LAS PALABRAS
############################

# Topic 1
wordMatrix <- as.data.frame(term3_1[1:15])
wordMatrix$word<-rownames(wordMatrix)

colnames(wordMatrix) <- c("Peso","Palabras")
wordMatrix %>% head(10)

newdata <-wordMatrix[order(-wordMatrix$Peso),] 
head(newdata)
d <- newdata
df <- as.data.frame(cbind(d[1:10,]$Palabras,as.numeric(d[1:10,]$Peso)))
colnames(df)<- c("Palabra","Peso")
df %>% head
# for ggplot to understand the order of words, you need to specify factor order
df$Palabra <- factor(df$Palabra, levels = df$Palabra[order(df$Peso)])
df$Peso <- df$Peso %>% as.character() %>% as.numeric()  
x11()
p4 <- ggplot(df, aes(x=Palabra, y=Peso)) + 
  geom_bar(stat="identity", fill="black")+
  coord_flip()+
  ggtitle("Topico 1") 
  

# Topic 2
wordMatrix <- as.data.frame(term3_2[1:15])
wordMatrix$word<-rownames(wordMatrix)

colnames(wordMatrix) <- c("Peso","Palabras")
wordMatrix %>% head(10)

newdata <-wordMatrix[order(-wordMatrix$Peso),] 
head(newdata)
d <- newdata
df <- as.data.frame(cbind(d[1:15,]$Palabras,as.numeric(d[1:15,]$Peso)))
colnames(df)<- c("Palabra","Peso")
df %>% head(15)
# for ggplot to understand the order of words, you need to specify factor order
df$Palabra <- factor(df$Palabra, levels = df$Palabra[order(df$Peso)])
df$Peso <- df$Peso %>% as.character() %>% as.numeric()  
x11()
p5 <-ggplot(df, aes(x=Palabra, y=Peso)) + 
  geom_bar(stat="identity", fill="black")+
  coord_flip()+
  ggtitle("Topico 2") 

# Topic 3
wordMatrix <- as.data.frame(term3_3[1:15])
wordMatrix$word<-rownames(wordMatrix)

colnames(wordMatrix) <- c("Peso","Palabras")
wordMatrix %>% head(10)

newdata <-wordMatrix[order(-wordMatrix$Peso),] 
head(newdata)
d <- newdata
df <- as.data.frame(cbind(d[1:10,]$Palabras,as.numeric(d[1:10,]$Peso)))
colnames(df)<- c("Palabra","Peso")
df %>% head
# for ggplot to understand the order of words, you need to specify factor order
df$Palabra <- factor(df$Palabra, levels = df$Palabra[order(df$Peso)])
df$Peso <- df$Peso %>% as.character() %>% as.numeric()  
x11()
p6 <- ggplot(df, aes(x=Palabra, y=Peso)) + 
  geom_bar(stat="identity", fill="black")+
  coord_flip()+
  ggtitle("Topico 3") 

library("ggExtra")
x11()
gridExtra::grid.arrange(p4,p5,p6, ncol=3)

#####################
# CORPUS
######################
base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=pterminos,col="blue")) +
  geom_line() + geom_line(aes(x=base0, y=pterminos2,col="green")) +
   scale_color_hue(l=40, c=35, name = "", labels = c("Bruto", "Preproceso")) +
  labs(x="", y="# palabras")



###########################
# Terminos en topicos
##########################

w1_k3 <- sort(h3_2[1,], decreasing = T) %>% head(15) %>% names
w2_k3 <- sort(h3_2[2,], decreasing = T) %>% head(15) %>% names
w3_k3 <- sort(h3_2[3,], decreasing = T) %>% head(15) %>% names

w1_k4 <- sort(h4_2[1,], decreasing = T) %>% head(15) %>% names
w2_k4 <- sort(h4_2[2,], decreasing = T) %>% head(15) %>% names
w3_k4 <- sort(h4_2[3,], decreasing = T) %>% head(15) %>% names
w4_k4 <- sort(h4_2[4,], decreasing = T) %>% head(15) %>% names

w1_k5 <- sort(h5_2[1,], decreasing = T) %>% head(15) %>% names
w2_k5 <- sort(h5_2[2,], decreasing = T) %>% head(15) %>% names
w3_k5 <- sort(h5_2[3,], decreasing = T) %>% head(15) %>% names
w4_k5 <- sort(h5_2[4,], decreasing = T) %>% head(15) %>% names
w5_k5 <- sort(h5_2[5,], decreasing = T) %>% head(15) %>% names

rank <- data.frame(Rank=1:15)
Tabla1 <- cbind(rank, w1_k3,w2_k3,w3_k3,
      w1_k4,w2_k4,w3_k4,w4_k4,
      w1_k5,w2_k5,w3_k5,w4_k5,w5_k5)


w1_k3%in%w1_k4 %>% mean
w1_k3%in%w1_k5 %>% mean

w2_k3%in%w2_k4 %>% mean
w2_k3%in%w2_k5 %>% mean

w3_k3%in%w3_k4 %>% mean
w3_k3%in%w3_k5 %>% mean

w4_k4%in%w4_k5 %>% mean


###########################
#  Visualización 3 topicos
###########################
term3_1 <- sort(h3_2[1,], decreasing = T)
term3_2 <- sort(h3_2[2,], decreasing = T)
term3_3 <- sort(h3_2[3,], decreasing = T)

x11()
set.seed(1234)
wordcloud(names(term3_1[1:15]), term3_1[1:15],
          max.words = 50, scale = c(1, 1), colors =  c("blue","black", "tomato"))
set.seed(1234)
wordcloud(names(term3_2[1:15]), term3_2[1:15],
          max.words = 50, scale = c(1, 1), colors =  c("blue","black", "tomato"))
set.seed(1234)
wordcloud(names(term3_3[1:15]), term3_3[1:15],
          max.words = 50, scale = c(1, 1), colors =  c("blue","black", "tomato"))

########################
# Selecicón de las Tres dimensiones
#########################

baseTopicos <- w3_2 %>% as.data.frame()
names(baseTopicos) 
set.seed(5)
#######
# PCA
#######
algo <- prcomp(as.matrix(tdm2))
A <- kmeans(algo$x,3)
plot(algo$x, col=A$cluster)
x11()
P000 <- algo$x %>% as.data.frame %>% ggplot() + 
  aes(x=PC1, y=PC2) + geom_point(col=A$cluster) +
  labs(title = "PCA TF-IDF")

P00 <- factoextra::fviz_nbclust(algo$x, FUNcluster = kmeans, method = "wss")
x11()
gridExtra::grid.arrange(P000,P00, nrow=1)

set.seed(1)
B00 <- factoextra::eclust(algo$x[,1:2],FUNcluster = c("kmeans"), k=2)
P0 <- factoextra::fviz_silhouette(B00)
# k=3 con 2 primeras componentes
set.seed(1)
B0 <- factoextra::eclust(algo$x[,1:2],FUNcluster = c("kmeans"), k=3)
P1 <- factoextra::fviz_silhouette(B0)
# k=4 con 2 primeras componentes
set.seed(1)
B1 <- factoextra::eclust(algo$x[,1:2],FUNcluster = c("kmeans"), k=4)
P2 <- factoextra::fviz_silhouette(B1)
x11()
gridExtra::grid.arrange(P0,P1,P2, nrow=3)



p7 <- plot_ly(baseTopicos,x=baseTopicos$V1,y=baseTopicos$V2,z=baseTopicos$V3,
              color = as.factor(A$cluster))%>%
  add_markers()  %>% add_text(x=baseTopicos$V1,y=baseTopicos$V2,z=baseTopicos$V3,text=rownames(w3_2),
                              color = as.factor(A$cluster)) %>% 
  layout(title="Topicos",
         scene = list(xaxis = list(title = 'Topico 1'),
                      yaxis = list(title = 'Topico 2'),
                      zaxis = list(title = 'Topico 3')))

htmlwidgets::saveWidget(as_widget(p7), "p7.html")


###########################
# Visualizar serie topicos
###########################

# DATES TASA DE INTERES
setwd( "C:/Users/h_air/Desktop/Minutas/England")
library(readxl)
datos <- read_excel("Contraste.xlsx")
datos$DATE <- datos$DATE %>% as.Date()
datos <- datos %>% as.data.frame 
# Filtrar fecha que no se necesita
datos <- datos %>% filter(!DATE=="1998-10-21")

# Tasa de interes
datos %>% filter(DATE>="1998-06-06" & DATE<="2014-03-19") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")
datos$DATE %>% tail(100)



base2 <- datos %>% filter(DATE>="1997-10-15" &DATE<="2014-02-19")
base0 <- datos %>% filter(DATE>="1997-10-15" &DATE<="2014-02-19")
base0 <- base0$DATE
base0 <- base0 %>% sort()

# Document term matrix
 base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_1[,1]*300)) +
  geom_line(col="red") + geom_line( aes(x=base0, y=w3_1[,2]*300), col="blue") +
  geom_line( aes(x=base0, y=w3_1[,3]*300))

# tf-idt Topico 1
p8 <- base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_2[,1])) +
  geom_line() + labs(x="", y="Peso", title="Topico 1")

# tf-idt Topico 2
p9 <- base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_2[,2])) +
  geom_line() + labs(x="", y="Peso", title="Topico 2")

# tf-idt Topico 3
p10 <- base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_2[,3])) +
  geom_line() + labs(x="", y="Peso", title="Topico 3")

# tf-idt sin tasa de interés
p11 <- base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_2[,1], col="red")) +
  geom_line() + geom_line( aes(x=base0, y=w3_2[,2], col="blue")) +
  geom_line( aes(x=base0, y=w3_2[,3], col="green")) +
  scale_color_hue(l=40, c=35, name = "", labels = c("Topico 2", "Topico 3",
                                                    "Topico 1")) +
  labs(x="", y="Peso", title="Topicos")

x11()
gridExtra::grid.arrange(p8,p9,p10,p11, ncol=2,nrow=2)

# tf-idt con tasa de interés
p12 <- base0 %>% as.data.frame %>% ggplot( aes(x=base0, y=w3_2[,1]*300, col="red")) +
  geom_line() + geom_line( aes(x=base0, y=w3_2[,2]*300, col="blue")) +
  geom_line( aes(x=base0, y=w3_2[,3]*300, col="orange")) +
  geom_line( aes(x=base2$DATE, y=base2$DESPUES, col="purple"), size=1) +
  scale_color_hue(name = "", labels = c("Topico 2", "Topico 3",
                                                     "Tasa de interés", "Topico 1")) + 
  labs(x="", y="Peso", title="Tasa de interés vs Topicos")
htmlwidgets::saveWidget(as_widget(plotly::ggplotly(p12)), "p12.html")
getwd()
plotly::ggplotly(p12)
################################################
#   Relación Tasa de Interés Documentos
################################################


ggplot(base2, aes(x=DATE, y=HOY,col="red")) +
  geom_line() + 
  geom_line( aes(x=DATE, y=DESPUES,col="blue")) + 
  labs(x="",
       y="") +
  scale_color_hue(l=40, c=35, name = "Tasa Interés", labels = c( "t+1","t"))


library("plotly")
plot1 <- base2 %>% filter(DATE>="1997-06-06" &DATE<="2004-10-20") %>% ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="black")+ 
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  geom_point( aes(x=DATE, y=DESPUES,col="blue")) +
  scale_color_hue(name = "Tasa Interés", labels = c("t+1", "t"))


htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot1)), "plot1.html")

plotly::ggplotly(plot1)


plot2 <-base2 %>% filter(DATE>="2004-11-17" & DATE<="2009-08-19") %>% ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="black")+ 
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  geom_point( aes(x=DATE, y=DESPUES,col="blue")) +
  scale_color_hue( name = "Tasa Interés", labels = c("t+1", "t"))


htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot2)), 
                        "plot2.html")

plotly::ggplotly(plot2)


plot3 <-base2 %>% filter(DATE>="2009-09-23"&DATE<="2014-02-19") %>%  ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="black")+ 
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  geom_point( aes(x=DATE, y=DESPUES,col="blue")) +
  scale_color_hue(name = "Tasa Interés", labels = c("t+1", "t"))


htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot3)), 
                        "plot3.html")

################################################################
# REPRESENTACIÓN POR PERIODOS MÁS CORTOS
################################################################

plotly::ggplotly(plot3)


plot4 <-base2 %>% filter(DATE>="2005-09-04"&DATE<="2008-09-04") %>% ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="black")+ 
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  geom_point( aes(x=DATE, y=DESPUES,col="blue")) +
  scale_color_hue(l=40, c=35, name = "Tasa Interés", labels = c("t+1", "t"))



htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot4)), 
                        "plot4.html")
plotly::ggplotly(plot4)

plot5 <-base2 %>% filter(DATE>="2008-10-08" & DATE<="2014-03-06") %>% ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="black")+ 
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  geom_point( aes(x=DATE, y=DESPUES,col="blue")) +
  scale_color_hue(l=40, c=35, name = "Tasa Interés", labels = c("t+1", "t"))

htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot5)), 
                        "plot5.html")
plotly::ggplotly(plot5)

plot6 <-base2  %>%  ggplot( aes(x=DATE, y=HOY, col="red")) +
  geom_line() +
  labs(caption = "Puntos en negro fecha de emisión de minuta",
       x="",
       y="%") +
  geom_line( aes(x=DATE, y=DESPUES, col="blue")) +
  scale_color_hue( name = "Tasa Interés", labels = c("t+1", "t"))

htmlwidgets::saveWidget(as_widget(plotly::ggplotly(plot6)), 
                        "plot6.html")


x11()
gridExtra::grid.arrange(plot1,plot2,plot3,plot6,nrow=2,ncol=2)

################################################################

# Guardar data frame de la base con la que se trabaja
library("xlsx")
write.csv(w3_2 %>% as.data.frame, file = "w3_2.csv")



################################################################
################################################################
####################### FIN               ######################
################################################################
################################################################
################################################################
