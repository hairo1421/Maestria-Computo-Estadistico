remove(list=ls())
setwd("C:/Users/angel/Desktop/MCE_CIMAT/Segundo_Semestre/Estadistica_Multivariada/ProyectoFinal/basesFinales")
# Lectura de tablas base
dat_bateo<- read.csv("dat_bateo_RL.csv")
dat_campo<- read.csv("dat_campo.csv")
dat_picheo<- read.csv("dat_picheo_RL.csv")

# Quitar filas dummy
dat_bateo<- dat_bateo[-which(is.na(dat_bateo$Id)),]
dat_campo<- dat_campo[-which(is.na(dat_campo$Id)),]


## Obtencion de las posiciones mas frecuentes de cada
## jugador
pos<-names(dat_campo)[32:40]
list<- dat_campo[,32:40] %>% apply(1,which.max)
posiciones_campo<- pos[list]
dat_campo<- cbind(dat_campo,posiciones_campo)
names(dat_campo)[45]<- "Posicion"


## Agregar posicion a dat_bateo
idcampo<- match(dat_bateo$Id,dat_campo$Id)
posiciones_campo<- dat_campo$Posicion[idcampo]
dat_bateo<- cbind(dat_bateo,Posicion=posiciones_campo)

# Mantener solo casos completos
dat_bateo<- dat_bateo[complete.cases(dat_bateo),]

#### Conversion del salario a numerico
salarios<- dat_bateo$Salary
salarios<- as.character(salarios)
for(i in 1:length(salarios)) {
  if(nchar(salarios[i])>0) {
    salarios[i]<- substr(salarios[i], 2, nchar(salarios[i])) 
  }
}
salarios<- as.numeric(salarios)
dat_bateo$Salary<- salarios

# Mantener solo casos completos (ahora quitando los que no tienen registro de salario)
dat_bateo<- dat_bateo[complete.cases(dat_bateo),]
variables<- c("Age","X2B","X3B","SB","CS","BB","SO","Salary","BATS" )
datos_var<- dat_bateo[,c(variables,"Posicion")]
variables

mean(dat_bateo$BA)
###################################################
###################################################
## Visualizacion de variables
par(mfrow=c(1,1))
# BA
var<- "BA"
encabezado<- "Distribucion del batting average de los bateadores"
d<- density(dat_bateo$BA)
plot(d,main=encabezado)
hist(dat_bateo$BA, breaks=20, col="red",main=encabezado,xlab=var)
which(dat_bateo$BA>0.4)
jug<- dat_bateo[which(dat_bateo$BA>0.4),]

# RBI
var<- "RBI"
encabezado<- "Distribucion de Runs Batted In de los bateadores"
d<- density(dat_bateo$RBI)
plot(d,main=encabezado)
hist(dat_bateo$RBI, breaks=20, col="red",main=encabezado,xlab=var)

# HR
var<- "HR"
encabezado<- "Distribucion de Home Runs de los bateadores"
d<- density(dat_bateo$HR)
plot(d,main=encabezado)
hist(dat_bateo$HR, breaks=30, col="red",main=encabezado,xlab=var)

mean(dat_bateo$BA)
sd(dat_bateo$BA)
#####  Mediante regresion lineal
# Hacer esta parte sin cargar la paralelizacion
### PARA BA
datos<- cbind(datos_var,BA=dat_bateo$BA)
fit<-lm(BA~., data=datos)
summary(fit)
# Calculate Relative Importance for Each Predictor
library(relaimpo)
imp<-calc.relimp(fit,rela=TRUE)
plot(imp)
### PARA RBI
datos<- cbind(datos_var,RBI=dat_bateo$RBI)
fit<-lm(RBI~., data=datos)
summary(fit)
# Calculate Relative Importance for Each Predictor
library(relaimpo)
imp<-calc.relimp(fit,rela=TRUE)
plot(imp)
### PARA HR
datos<- cbind(datos_var,HR=dat_bateo$HR)
fit<-lm(HR~., data=datos)
summary(fit)
par(mfrow=c(1,1))
# Calculate Relative Importance for Each Predictor
library(relaimpo)
imp<-calc.relimp(fit,rela=TRUE)
plot(imp)



### Visualizacion de triple crown contra otras variables
datos<-cbind(datos_var,BA=dat_bateo$BA,RBI=dat_bateo$RBI,HR=dat_bateo$HR)
#### Construir clase salarial
summary(datos$Salary)
#> summary(datos$Salary)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#100000   551300  2000000  4997903  7000000 35571429 
claseSalarial<- rep(0,times=nrow(datos))
for(i in 1:nrow(datos)) {
  if(datos$Salary[i]<551300) {
    claseSalarial[i]<-1
  }
  else {
    if(datos$Salary[i]<2000000) {
      claseSalarial[i]<-2
    } 
    else {
      if(datos$Salary[i]<7000000) {
        claseSalarial[i]<-3
      }
      else {
        claseSalarial[i]<-4
      }
    }
  }
}
datos<- cbind(datos, claseSalarial=claseSalarial)
datos$claseSalarial<- as.factor(datos$claseSalarial)
#### BOX PLOTS

### BA
# BA vs Age
ggplot(data = datos, aes(x =as.factor(Age) , y = BA, color = as.factor(Age)   )) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre BA")+xlab("Edad")+labs(color="Edad")

# BA vs Pos
ggplot(data = datos, aes(x =Posicion , y = BA, color = Posicion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre BA")+xlab("Posicion")

# BA vs BATS
ggplot(data = datos, aes(x =BATS , y = BA, color = BATS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre BA")+xlab("BATS")
## BA vs Salary
ggplot(data = datos, aes(x =claseSalarial , y = BA, color = claseSalarial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre BA")+xlab("Salario")

### RBI
# RBI vs Age
ggplot(data = datos, aes(x =as.factor(Age) , y = RBI, color = as.factor(Age)   )) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("Edad")+labs(color="Edad")

# RBI vs Pos
ggplot(data = datos, aes(x =Posicion , y = RBI, color = Posicion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("Posicion")

# RBI vs BATS
ggplot(data = datos, aes(x =BATS , y = RBI, color = BATS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("BATS")
## RBI vs Salary
ggplot(data = datos, aes(x =claseSalarial , y = RBI, color = claseSalarial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("Salario")

### HR
# HR vs Age
ggplot(data = datos, aes(x =as.factor(Age) , y = HR, color = as.factor(Age)   )) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("Edad")+labs(color="Edad")

# HR vs Pos
ggplot(data = datos, aes(x =Posicion , y = HR, color = Posicion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("Posicion")

# HR vs BATS
ggplot(data = datos, aes(x =BATS , y = HR, color = BATS)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("BATS")
## HR vs Salary
ggplot(data = datos, aes(x =claseSalarial , y = HR, color = claseSalarial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("Salario")



### Quitando a los pitcher
sum(datos$Posicion=="P")
table(datos$Posicion)
#### BOX PLOTS
ind<- which(datos$Posicion=="P")
datos<- datos[-ind,]

### RBI

# RBI vs Pos
ggplot(data = datos, aes(x =Posicion , y = RBI, color = Posicion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("Posicion")
## RBI vs Salary
ggplot(data = datos, aes(x =claseSalarial , y = RBI, color = claseSalarial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre RBI")+xlab("Salario")

### HR

# HR vs Pos
ggplot(data = datos, aes(x =Posicion , y = HR, color = Posicion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("Posicion")

## HR vs Salary
ggplot(data = datos, aes(x =claseSalarial , y = HR, color = claseSalarial)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +ggtitle("Box plots sobre HR")+xlab("Salario")



# Agregar scatter plot de Triple crown vs ella misma
library(cdata)
library(WVPlots)
PairPlot(d=dat_bateo,meas_vars =c("BA","RBI","HR"),title="Triple Crown",group_var = "BATS" )
PairPlot(d=dat_bateo,meas_vars =c("HR","SO"),title="HR vs SO" )
