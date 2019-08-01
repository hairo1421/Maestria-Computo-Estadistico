##############################
# Construcción Tasa interés
##############################

# Librerías
library("magrittr")
library("readxl")
library("tidyverse")
library("strsplit")
library("xlsx")
library("plotly")
library("ggrepel")
library("gridExtra")

# introduzca dirección de la base
getwd()
setwd("C:/Users/h_air/Desktop/Minutas/England")

# Importa xlsx donde se ecuentra la información
minuta <- read_excel("minuta2.xlsx")
tasa <- read_excel("Bank of England  Database.xlsx")

############################
# modificarformato de fecha
############################
paste(tasa$Date[1], sep = " - ")

# tamaño de la muestra
n <- tasa$Date %>% length()

A<-matrix(0L,n,1) 
for (i in 1:n) {
  a=strsplit(tasa$Date[i], "\\s+")[[1]]
  if(a[2]=="Jan"){
    a[2] <- "01"
  } else if(a[2]=="Feb"){
    a[2]<- "02"
  }else if(a[2]=="Mar"){
    a[2]<- "03"
  }else if(a[2]=="Apr"){
    a[2]<- "04"
  }else if(a[2]=="May"){
    a[2]<- "05"
  }else if(a[2]=="Jun"){
    a[2]<- "06"
  }else if(a[2]=="Jul"){
    a[2]<- "07"
  }else if(a[2]=="Aug"){
    a[2]<- "08"
  }else if(a[2]=="Sep"){
    a[2]<- "09"
  }else if(a[2]=="Oct"){
    a[2]<- "10"
  }else if(a[2]=="Nov"){
    a[2]<- "11"
  }else{
    a[2]<- "12"
  }
  b <- c(a[3],a[2],a[1])
  A[i] <- paste(b, collapse="-")
}

A <- A %>% as.data.frame()
colnames(A) <- "data"
base <- cbind(A,tasa$tasa) %>% as.data.frame()
plot(A, tasa$tasa, type="l")
base %>% head
base$data <- base$data %>% as.character()

base$data <- as.Date(base$data,"%y-%m-%d")
names(base) <- c("data","tasa") 

##############################
# Formato fecha minuta
##############################

n <- minuta$DATE %>% length()

A<-matrix(0L,n,1) 
for (i in 1:n) {
  a=strsplit(minuta$DATE[i], "\\s+")[[1]]
  if(a[2]=="January"){
    a[2] <- "01"
  } else if(a[2]=="February"){
    a[2]<- "02"
  }else if(a[2]=="March"){
    a[2]<- "03"
  }else if(a[2]=="April"){
    a[2]<- "04"
  }else if(a[2]=="May"){
    a[2]<- "05"
  }else if(a[2]=="June"){
    a[2]<- "06"
  }else if(a[2]=="July"){
    a[2]<- "07"
  }else if(a[2]=="August"){
    a[2]<- "08"
  }else if(a[2]=="September"){
    a[2]<- "09"
  }else if(a[2]=="October"){
    a[2]<- "10"
  }else if(a[2]=="November"){
    a[2]<- "11"
  }else{
    a[2]<- "12"
  }
  b <- c(a[3],a[2],a[1])
  A[i] <- paste(b, collapse="-")
}

A <- A %>% character()
colnames(A) <- "data"

minuta$DATE <- A%>% as.Date

names(base) <- c("data","tasa") 


base2 <- base
base2$data <-base2$data %>% as.character()
minu <- minuta$DATE
BASE <- matrix(0L, length(minu), 2)

for (i in 1:length(minu)){
  BASE[i, ] <- base2[base2$data==minu[i],]  %>% as.matrix
}

BASE <- BASE %>% as.data.frame
BASE$V2 <- BASE$V2 %>% as.character() %>% as.numeric()
BASE$V1 <- BASE$V1 %>%  as.Date


# Guardar lo anterior en xlsx
write.xlsx(BASE, "TASA DE INTERES DIARIA.xlsx")


# Tasa interés un día antes de la minuta
Antes <- matrix(0L, length(minu), 1)
for (i in 1:length(minu)){
  Antes[i,1] <- base2[which(base2$data==minu[i]),]  %>% rownames()
}
Antes <- Antes %>% as.numeric
Antes <- base2[(Antes+1),]
write.xlsx(Antes, "TASA DE INTERES DIARIA un dia antes.xlsx")


#  Tasa interés un día despues de la minuta
Despues <- matrix(0L, length(minu), 1)
for (i in 1:length(minu)){
  Despues[i,1] <- base2[which(base2$data==minu[i]),]  %>% rownames()
}
Despues <- Despues %>% as.numeric
Despues <- base2[(Despues-1),]
write.xlsx(Despues, "TASA DE INTERES DIARIA un dia despues.xlsx")


# Importar las bases creadas
HOY <- readxl::read_excel("TASA DE INTERES DIARIA.xlsx")
HOY<- HOY[,2:3]
HOY$V1 <- HOY$V1 %>% as.Date()
ANTES <- readxl::read_excel("TASA DE INTERES DIARIA un dia antes.xlsx")
ANTES<- ANTES[,2:3]
DESPUES <- readxl::read_excel("TASA DE INTERES DIARIA un dia despues.xlsx")
DESPUES<- DESPUES[,2:3]

# Generar la tasa omitiendo efecto del día ques epública minuta
tasa <- (DESPUES$tasa- ANTES$tasa)/ ANTES$tasa
FUTURO <- cbind(as.data.frame(HOY$V1),as.data.frame(tasa))
write.xlsx(FUTURO, "TASA DE INTERES DIARIA paper.xlsx")
# Guardando la tasa
FUTURO <- readxl::read_excel("TASA DE INTERES DIARIA paper.xlsx")
FUTURO<- FUTURO[,2:3]
names(FUTURO) <- c("date","tasa")


#######################
#  Visualización
#######################

# es lo de hoy
ggplot(HOY, aes(x=V1, y=V2)) +
  geom_line() + 
  xlab("")

# es lo de ayer
ggplot(ANTES, aes(x=as.Date(data), y=tasa)) +
  geom_line() + 
  xlab("")
# es lo de despues
ggplot(DESPUES, aes(x=as.Date(data), y=tasa)) +
  geom_line() + 
  xlab("")
# es el que utilizamos

ggplot(FUTURO, aes(x=as.Date(date), y=tasa, label=FUTURO$date %>% as.character)) +
  geom_line() + geom_point(aes(x=as.Date(date), y=tasa), col="red") +
  geom_text_repel() + labs(title = "geom_text_repel()")


# Base que contienen todas
Contraste <-cbind(as.data.frame(HOY$V1), ANTES$tasa,
                  HOY$V2, DESPUES$tasa, FUTURO$tasa )

names(Contraste) <- c("DATE","ANTES","HOY","DESPUES","FUTURO")

# Guardar base
write.xlsx(Contraste, "Contraste.xlsx")

# vusualización
p1 <-Contraste %>% filter(DATE>="1997-06-06" &DATE<="1999-07-08") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")


plotly::ggplotly(p1)
# vusualización
p2 <-Contraste %>% filter(DATE>="1999-08-05" & DATE<="2002-01-10") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")


plotly::ggplotly(p2)

# vusualización
p3 <-Contraste %>% filter(DATE>="2002-02-07"&DATE<="2005-08-07") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")


plotly::ggplotly(p3)

# vusualización
p4 <-Contraste %>% filter(DATE>="2005-09-04"&DATE<="2008-09-04") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")


plotly::ggplotly(p4)
# vusualización
p5 <-Contraste %>% filter(DATE>="2008-10-08" & DATE<="2014-03-06") %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")
plotly::ggplotly(p5)
# vusualización
p6 <-Contraste  %>% ggplot( aes(x=DATE, y=HOY)) +
  geom_line() +
  geom_point( aes(x=DATE, y=HOY), col="red")+ 
  xlab("")+
  #geom_line( aes(x=DATE, y=ANTES), col="red")  + 
  #xlab("") +
  #geom_point( aes(x=DATE, y=ANTES), col="black") +
  geom_line( aes(x=DATE, y=DESPUES), col="blue")  + 
  xlab("") +
  geom_point( aes(x=DATE, y=DESPUES), col="black")


x11()
gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2)

###########################
# Clasificación
###########################
# En caso de que se desee realizar clasificación
Contraste$Clase <- sign(Contraste$FUTURO)
write.xlsx(Contraste, "Contraste_clasificador.xlsx")


#############################################################
#############################################################
#############################################################
##################### Fin            ########################
#############################################################
#############################################################
#############################################################
#############################################################