###############################
#
# Obj. II 	Predecir salario 
#
###############################

############################
# Inciso a
###########################

######################################################################
# Solo con datos de bateo (Exploratorio y luego clasificadores, 
# no importa su posición defensiva, solo dados las estadísticas de 
# bateo, asignar salario)
######################################################################

# Jugadores de campo que batean Exploratorio
# Variable de contro, salarios.



# Librerías

library("tidyverse")
library("ggplot2")
library("magrittr")
library("factoextra")
library("ggrepel")
library("plotly")
library("htmlwidgets")



getwd()
setwd("C:/Users/h_air/Desktop/Datos")
base_campo <- read.csv("dat_campo.csv", head=T)
# todos menos bateador
base_bateo <- read.csv("dat_bateo_RL.csv", head=T) 
# Posición
list <-  base_campo[,32:40]   %>% apply(1,which.max)
list <- list %>% unlist
posicion <- list %>% names
Posicion <- posicion %>% as.data.frame()
# Se retiran las 3 observaciones que hacen referencia al promedio
# de las observaciones
Campo <- base_campo[-c(1377,2732,4076),]
Campo <- cbind(Campo, Posicion)
names(Campo)[45] <- "Posicion"
Campo %>% names

CampoEscalado <- Campo
# ordenar variables para escalar
CampoEscalado <- CampoEscalado[,c(1,2,3,4,5,6,16,23,24,25,26,27,45,
                                  7,8,9,10,11,12,13,14,15,17,18,19,20,21,
                                  22,28,29,30,31,32,33,34,35,36,37,38,39,40,
                                  41,42,43,44)]

###############################
#   Catcher 2018
###############################

# Filtrado por año 
base_campo2018 <- CampoEscalado %>% filter(year=="2018")

###############################
# Base Bateo 2018
###############################

## Generacion de dat_campo_bateo_2018
dat_bateo_RL_2018<- base_bateo[which(base_bateo$year=="2018"),]
# hacer match id base campo con id base bateo
idcampo<- match(base_campo2018$Id,dat_bateo_RL_2018$Id)
# tienes aquellos jugadores de campo  catcher que sí batean
dat_campo_bateo_2018<- dat_bateo_RL_2018[idcampo,] # filtras
# id de los de campo con su pisición pero las variables de bateo 
# solo pones a los jugadores que están en campo
dat_campo_bateo_2018<- cbind(base_campo2018$Id,base_campo2018$Posicion,dat_campo_bateo_2018)

dat_campo_bateo_2018<- dat_campo_bateo_2018[!is.na(dat_campo_bateo_2018$Id),]
dat_campo_bateo_2018<- dat_campo_bateo_2018[,-1]
names(dat_campo_bateo_2018)[1]<-"Posicion"

colnames(dat_campo_bateo_2018)

##############################
# VARIABLE SALARIO CONTINUA
##############################




# filtrar salario y hacerlo numerico
salarios<- dat_campo_bateo_2018$Salary
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
sum(is.na(salarios)) # 972
sum(dat_campo_bateo_2018$Salary=="")
dat_campo_bateo_2018$Salary<-salarios
dat_campo_bateo_2018$Salary %>% length() # 1268
##############################
# VARIABLE SALARIO CLASES
##############################

clase<-rep(-1,times=nrow(dat_campo_bateo_2018))
quantiles <- dat_campo_bateo_2018$Salary %>% na.omit %>% quantile(c(.25,.50,.75))
# 560750 1262500 6712500 


for(i in 1:nrow(dat_campo_bateo_2018)) {
  if(!is.na(dat_campo_bateo_2018$Salary[i])){
    if(dat_campo_bateo_2018$Salary[i]<quantiles[1]){
      clase[i]<- 1
    }
    else {
      if(dat_campo_bateo_2018$Salary[i]<quantiles[2]){
        clase[i]<- 2
      }
      else {
        if(dat_campo_bateo_2018$Salary[i]<quantiles[3]){
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
summary(dat_campo_bateo_2018$Salary)


########################################################
# SALARIO CONTINUO
########################################################


########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables básicas
########################################################
# Retirar id campo 

# VARIABLES BÁSICAS DE CAMPO
Varibles_base <- names(dat_campo_bateo_2018)[11:31]
Varibles_base <- c(Varibles_base,names(dat_campo_bateo_2018)[53])
#[1] "R"    "H"    "X2B"  "X3B"  "HR"   "RBI" 
#[7] "SB"   "CS"   "BB"   "SO"   "BA"   "OBP" 
#[13] "SLG"  "OPS"  "OPS." "TB"   "GDP"  "HBP" 
#[19] "SH"   "SF"   "IBB"  "Salary" 

temporal<- dat_campo_bateo_2018[,Varibles_base]
colnames(dat_campo_bateo_2018[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA

Campo_valores <- prcomp(temporal[, -22], scale=T)
scores<- as.data.frame(Campo_valores$x)

uno <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Salary"]) %>% 
  layout(title="Salario jugadores de campo que batean 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(uno), "uno.html")

#############
# RESUMEN
#############

# Salario general sin diferenciar por posición.
# Se observan tres grupos, posiblemente cuatro grupos utilizando las variables base
# en general si discriminan.
temporal %>% names 

# Se bserva ligeramente una agrupación de los jugadores (sin diferenciar posición) de
# campo que batean, de aquellos que ganan más.


# Introduciomos la posición

dos <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"] %>% as.factor) %>% 
  layout(title="Salario jugadores de campo que batean por posición 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(dos), "dos.html")

############
# RESUMEN
############
# Se observa que los pitcher forman un grupo distintos y entre ellos hay grupos de pitcher
# que ganan más que otros

# los catcher también se diferencian, de bajo de ellos se encuentran los CF
# los LF se confunden con los CF, RF, SS.
# los primera , segun, y tercera base se agrupa, sin embargo, se confunden
# con todos menos los pitcher.


##############
# conclusión
##############

# Estas variables diferenciaron los salarios en grupos, de salario alto y bajo
# y detectan que los pitcher se cuentan aparte


######################################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables base + Estadísticas
######################################################################
dat_campo_bateo_2018 %>% names
Varibles_Estadisticas <- colnames(dat_campo_bateo_2018[,c(11:31,57,59:73)])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"Salary")
# [1] "R"         "H"         "X2B"      
#[4] "X3B"       "HR"        "RBI"      
#[7] "SB"        "CS"        "BB"       
#[10] "SO"        "BA"        "OBP"      
#[13] "SLG"       "OPS"       "OPS."     
#[16] "TB"        "GDP"       "HBP"      
#[19] "SH"        "SF"        "IBB"

# Estas son las nuevas variables que se les incluye

#[22] "PA"        "BtRuns"    "BtWins"   
#[25] "Plays"     "WPA"       "WPA."     
#[28] "WPA..1"    "aLI"       "WPA.LI"   
#[31] "Clutch"    "RE24"      "REW"      
#[34] "boLI"      "RE24.boLI" "PHlev"    
#[37] "AB.win"   "Salary" 

temporal<- dat_campo_bateo_2018[,Varibles_Estadisticas]
colnames(dat_campo_bateo_2018[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal[, -38] %>% names
# PCA
Campo_valores <- prcomp(temporal[, -38], scale=T)
scores<- as.data.frame(Campo_valores$x)

tres <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Salary"]) %>% 
  layout(title="Salario jugadores de campo que batean 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(tres), "tres.html")

#############
# RESUMEN
#############
# Genera una figura convexa  donde no separa bien los grupos, solo de forma ligera
# en uno de los extremos pone a algunos jugadores que ganan más que el promedio.

# en general las variables básicas y de estadísitca no  discriminan los salarios de los
# jugadores.
temporal %>% names 


# Introduciomos la posición

cuatro <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"] %>% as.factor) %>% 
  layout(title="Salario jugadores de campo que batean por posición 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cuatro), "cutro.html")

####################
# RESUMEN
####################

# Se observa que con estas variables siguen separando a los pitcher, donde dentro
# de su grupo existen grupos de salarios altos y bajos

# La demás posiciones se confunden y no se diferencia el salario



##########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables de valor
##########################################################


# PCA ya con salary y variables del valor del jugador
colnames(dat_campo_bateo_2018)
# filtrar variables
Variables_Valor<- colnames(dat_campo_bateo_2018[,c(36:43,45,46,52,53)])
# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "Salary"

temporal<- dat_campo_bateo_2018[,Variables_Valor]
colnames(dat_campo_bateo_2018[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]

# PCA
Campo_valores <- prcomp(temporal[ ,-12], scale=T)
scores<- as.data.frame(Campo_valores$x)

cinco <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Salary"]) %>% 
  layout(title="Salario jugadores de campo que batean 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cinco), "cinco.html")

#############
# RESUMEN
#############
# Con las variables de valor del jugador no se logra diferenciar o discriminar el salario
# se confunden los salarios altos con los salarios bajos.

temporal %>% names 


# Introduciomos la posición

seis <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"] %>% as.factor) %>% 
  layout(title="Salario jugadores de campo que batean por posición 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(seis), "seis.html")

####################
# RESUMEN
####################

# Si se etiqueta la posición estas variables agrupan a los pitcher y dentro de ellos se observa
# como hay pitcher con ingreso alto y bajo que se separan con estas variablles
# las posiciones si se diferencian sin embargo no los salarios dentro de las otras posiciones.

##########################################
# Salario solo para pitcher
##########################################


# PCA ya con salary y variables del valor del jugador
dat_campo_bateo_2018_pitcher <- dat_campo_bateo_2018 %>% filter(Posicion=="P")
colnames(dat_campo_bateo_2018_pitcher)
# filtrar variables
Variables_Valor<- colnames(dat_campo_bateo_2018_pitcher[,c(36:43,45,46,52,53)])
# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "Salary"
Varibles_Estadisticas <- colnames(dat_campo_bateo_2018_pitcher[,c(11:31,57,59:73)])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"Salary")
#[1] "R"         "H"         "X2B"      
#[4] "X3B"       "HR"        "RBI"      
#[7] "SB"        "CS"        "BB"       
#[10] "SO"        "BA"        "OBP"      
#[13] "SLG"       "OPS"       "OPS."     
#[16] "TB"        "GDP"       "HBP"      
#[19] "SH"        "SF"        "IBB"      
#[22] "PA"        "BtRuns"    "BtWins"   
#[25] "Plays"     "WPA"       "WPA."     
#[28] "WPA..1"    "aLI"       "WPA.LI"   
#[31] "Clutch"    "RE24"      "REW"      
#[34] "boLI"      "RE24.boLI" "PHlev"    
#[37] "AB.win"    "Salary"  

Varibles_base <- names(dat_campo_bateo_2018_pitcher)[11:31]
Varibles_base <- c(Varibles_base,names(dat_campo_bateo_2018_pitcher)[53])
#[1] "R"      "H"      "X2B"    "X3B"   
#[5] "HR"     "RBI"    "SB"     "CS"    
#[9] "BB"     "SO"     "BA"     "OBP"   
#[13] "SLG"    "OPS"    "OPS."   "TB"    
#[17] "GDP"    "HBP"    "SH"     "SF"    
#[21] "IBB"    "Salary"

#############################
# PITCHER VARIABLES VALOR
#############################

temporal<- dat_campo_bateo_2018_pitcher[,Variables_Valor]
colnames(dat_campo_bateo_2018_pitcher[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA
temporal %>% names

Campo_valores <- prcomp(temporal[,-12], scale=F)
scores<- as.data.frame(Campo_valores$x)

siete <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018_pitcher[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018_pitcher[indComp,"Salary"]) %>% 
  layout(title="Salario Pitcher  2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(siete), "siete.html")

#####################
# RESUMEN
#####################

# A manera general el salario de los pitcher se dividen en dos, alto y bajo
# utilizando las variables del valor del jugador
Variables_Valor


#############################
# PITCHER VARIABLES BASE
#############################
temporal<- dat_campo_bateo_2018_pitcher[,Varibles_base]
colnames(dat_campo_bateo_2018_pitcher[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA
temporal %>% names

Campo_valores <- prcomp(temporal[,-22], scale=F)
scores<- as.data.frame(Campo_valores$x)

ocho <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018_pitcher[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018_pitcher[indComp,"Salary"]) %>% 
  layout(title="Salario Pitcher  2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(ocho), "ocho.html")

#####################
# RESUMEN
#####################

# No se  nota mejoría en  la separación de los grupos por salario al utilizar
Varibles_base


#####################################
# PITCHER VARIABLES Estadística
#####################################
temporal<- dat_campo_bateo_2018_pitcher[,Varibles_Estadisticas]
colnames(dat_campo_bateo_2018_pitcher[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA
temporal %>% names

Campo_valores <- prcomp(temporal[,-38], scale=F)
scores<- as.data.frame(Campo_valores$x)

nueve <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_campo_bateo_2018_pitcher[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018_pitcher[indComp,"Salary"]) %>% 
  layout(title="Salario Pitcher  2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(nueve), "nueve.html")

#####################
# RESUMEN
#####################

# No hay muchos pitcher con estos estadísticos que tengan información del salario y dentro de ellos
# si existen aquellos con salarios altos.

Varibles_Estadisticas


########################################################
########################################################
########################################################
## SALARIO Discreto
########################################################
########################################################
########################################################

########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables básicas
########################################################
# Retirar id campo 

# VARIABLES BÁSICAS DE CAMPO

Varibles_base <- names(dat_campo_bateo_2018)[11:31]
Varibles_base <- c(Varibles_base,names(dat_campo_bateo_2018)[53])
#[1] "R"    "H"    "X2B"  "X3B"  "HR"   "RBI" 
#[7] "SB"   "CS"   "BB"   "SO"   "BA"   "OBP" 
#[13] "SLG"  "OPS"  "OPS." "TB"   "GDP"  "HBP" 
#[19] "SH"   "SF"   "IBB"  "Salary" 

temporal<- dat_campo_bateo_2018[,Varibles_base]
colnames(dat_campo_bateo_2018[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal %>% names
temporal<- temporal[complete.cases(temporal),]
# PCA
Campo_valores <- prcomp(temporal[,-22], scale=T)
scores<- as.data.frame(Campo_valores$x)
clase<- as.factor(clase)

uno_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(uno_clase), "uno_clase.html")

#############
# RESUMEN
#############

# Con las variables básicas la clase alta  se diferencia un poco 
# clase alta se confunde con clase 3 y 2, esto le pasa  también a la clase 1

temporal %>% names 

# Se bserva ligeramente una agrupación de los jugadores (sin diferenciar posición) de
# campo que batean, de aquellos que ganan más.


# Introduciomos la posición

dos_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(dos_clase), "dos_clase.html")




############
# RESUMEN
############
# Se observa la dificultad de separar los salarios con estas variables,
# lo interesante es que dentro de el grupo de pitchers si se separan.



# Estas variables diferenciaron los salarios en grupos, de salario alto y bajo
# y detectan que los pitcher se cuentan aparte


######################################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables base + Estadísticas
######################################################################
dat_campo_bateo_2018 %>% names
Varibles_Estadisticas <- colnames(dat_campo_bateo_2018[,c(11:31,57,59:73)])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"Salary")
# [1] "R"         "H"         "X2B"      
#[4] "X3B"       "HR"        "RBI"      
#[7] "SB"        "CS"        "BB"       
#[10] "SO"        "BA"        "OBP"      
#[13] "SLG"       "OPS"       "OPS."     
#[16] "TB"        "GDP"       "HBP"      
#[19] "SH"        "SF"        "IBB"

# Estas son las nuevas variables que se les incluye

#[22] "PA"        "BtRuns"    "BtWins"   
#[25] "Plays"     "WPA"       "WPA."     
#[28] "WPA..1"    "aLI"       "WPA.LI"   
#[31] "Clutch"    "RE24"      "REW"      
#[34] "boLI"      "RE24.boLI" "PHlev"    
#[37] "AB.win"   "Salary" 

temporal<- dat_campo_bateo_2018[,Varibles_Estadisticas]
colnames(dat_campo_bateo_2018[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA
temporal %>% names

Campo_valores <- prcomp(temporal[,-38], scale=T)
scores<- as.data.frame(Campo_valores$x)
clase<- as.factor(clase)

tres_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(tres_clase), "tres_clase.html")

#############
# RESUMEN
#############

# Utilizando todas las  variables se logra diferenciar grupos altos de bajos
# los demas se confunden
temporal %>% names 


# Introduciomos la posición

cuatro_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(cuatro_clase), "cuatro_clase.html")




############
# RESUMEN
############
# Dentro de la representación se observa un grupo pitcher que dentro de ellos si se 
# logra diferenciar el tipo de salario


##########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables de valor
##########################################################


# PCA ya con salary y variables del valor del jugador
colnames(dat_campo_bateo_2018)
# filtrar variables
Variables_Valor<- colnames(dat_campo_bateo_2018[,c(36:43,45,46,52,53)])
# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "Salary"

temporal<- dat_campo_bateo_2018[,Variables_Valor]
colnames(dat_campo_bateo_2018[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% names
# PCA
Campo_valores <- prcomp(temporal[,-12], scale=T)
scores<- as.data.frame(Campo_valores$x)
clase<- as.factor(clase)

cinco_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(cinco_clase), "cinco_clase.html")

#############
# RESUMEN
#############

# Utilizando las variables del valor del jugador  se diferencia, no en su totalidad,
# aquellos jugadores con el salario alto respecto a los de salario bajo.
# los quantiles de en medio se confunde.

temporal %>% names 


# Introduciomos la posición

seis_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"Posicion"],
                              color=dat_campo_bateo_2018[indComp,"Posicion"]) %>% 
  layout(title="Salario de jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(seis_clase), "seis_clase.html")




############
# RESUMEN
############
# Dentro de la representación se observa un grupo pitcher que dentro de ellos si se 
# logra diferenciar el tipo de salario

# Se nota el efecto de un grupo dentro de los pitcher que con estas variables
# diferencia a pitcher que ganan un nivel salarial alto, respecto a los que ganan
# un nivel salareal bajo.

##########################################
# Salario solo para pitcher por clases
##########################################

base_temporal <- cbind(dat_campo_bateo_2018, clase)
# PCA ya con salary y variables del valor del jugador
dat_campo_bateo_2018_pitcher <- base_temporal %>% filter(Posicion=="P")
colnames(dat_campo_bateo_2018_pitcher)
# filtrar variables
Variables_Valor<- colnames(dat_campo_bateo_2018_pitcher[,c(36:43,45,46,52,53,75)])
# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "Salary" "clase" 
Varibles_Estadisticas <- colnames(dat_campo_bateo_2018_pitcher[,c(11:31,57,59:73)])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"Salary","clase")
#[1] "R"         "H"         "X2B"      
#[4] "X3B"       "HR"        "RBI"      
#[7] "SB"        "CS"        "BB"       
#[10] "SO"        "BA"        "OBP"      
#[13] "SLG"       "OPS"       "OPS."     
#[16] "TB"        "GDP"       "HBP"      
#[19] "SH"        "SF"        "IBB"      
#[22] "PA"        "BtRuns"    "BtWins"   
#[25] "Plays"     "WPA"       "WPA."     
#[28] "WPA..1"    "aLI"       "WPA.LI"   
#[31] "Clutch"    "RE24"      "REW"      
#[34] "boLI"      "RE24.boLI" "PHlev"    
#[37] "AB.win"    "Salary"  "clase"

Varibles_base <- names(dat_campo_bateo_2018_pitcher)[11:31]
Varibles_base <- c(Varibles_base,names(dat_campo_bateo_2018_pitcher)[53], "clase")
#[1] "R"      "H"      "X2B"    "X3B"   
#[5] "HR"     "RBI"    "SB"     "CS"    
#[9] "BB"     "SO"     "BA"     "OBP"   
#[13] "SLG"    "OPS"    "OPS."   "TB"    
#[17] "GDP"    "HBP"    "SH"     "SF"    
#[21] "IBB"    "Salary" "clase" 

#############################
# PITCHER VARIABLES VALOR
#############################

temporal<- dat_campo_bateo_2018_pitcher[,Variables_Valor]
colnames(dat_campo_bateo_2018_pitcher[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA

temporal %>% names
Campo_valores <- prcomp(temporal[,-c(12:13)], scale=F)
scores<- as.data.frame(Campo_valores$x)

clase<- temporal$clase %>% as.factor


siete_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario Pitcher jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(siete_clase), "siete_clase.html")
#####################
# RESUMEN
#####################

# Se observa separación de pitcher de clase alta respecto a la baja, en terminos del salario
# utilizando las variables del valor del jugador
Variables_Valor[-c(12:13)]


#############################
# PITCHER VARIABLES BASE
#############################

temporal<- dat_campo_bateo_2018_pitcher[,Varibles_base]
colnames(dat_campo_bateo_2018_pitcher[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA

temporal %>% names
Campo_valores <- prcomp(temporal[,-c(22:23)], scale=F)
scores<- as.data.frame(Campo_valores$x)

clase<- temporal$clase %>% as.factor


ocho_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario Pitcher jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(ocho_clase), "ocho_clase.html")
#####################
# RESUMEN
#####################

# No se tienen muchos pitchers con estas variables y la representación no es muy buena

Varibles_base[-c(22:23)]


#####################################
# PITCHER VARIABLES Estadística
#####################################

temporal<- dat_campo_bateo_2018_pitcher[,Varibles_Estadisticas]
colnames(dat_campo_bateo_2018_pitcher[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
# PCA

temporal %>% names
Campo_valores <- prcomp(temporal[,-c(38:39)], scale=F)
scores<- as.data.frame(Campo_valores$x)

clase<- temporal$clase %>% as.factor


nueve_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="Salario Pitcher jugadores PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(nueve_clase), "nueve_clase.html")
#####################
# RESUMEN
#####################
# no hay muchos pitcher con estas estadísitcas.

Varibles_Estadisticas[-c(38:39)]


########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables básicas BATS
########################################################
# Retirar id campo 

# VARIABLES BÁSICAS DE CAMPO

Varibles_base <- names(dat_campo_bateo_2018)[11:31]
Varibles_base <- c(Varibles_base,names(dat_campo_bateo_2018)[53])
#[1] "R"    "H"    "X2B"  "X3B"  "HR"   "RBI" 
#[7] "SB"   "CS"   "BB"   "SO"   "BA"   "OBP" 
#[13] "SLG"  "OPS"  "OPS." "TB"   "GDP"  "HBP" 
#[19] "SH"   "SF"   "IBB"  "Salary" 

temporal<- dat_campo_bateo_2018[,Varibles_base]
colnames(dat_campo_bateo_2018[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal %>% names
temporal<- temporal[complete.cases(temporal),]
# PCA
Campo_valores <- prcomp(temporal[,-22], scale=T)
scores<- as.data.frame(Campo_valores$x)
clase<- as.factor(clase)

diez_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"BATS"]) %>% 
  layout(title="Salario de jugadores por clase y brazo que batea PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(diez_clase), "diez_clase.html")

#############
# RESUMEN
#############

# Con las variables básicas la clase alta  no se separan los que batean
# los de salario alto y bajo que baten con alguna mano en particular


##########################################################
# JUGADORES DE CAMPO QUE BATEAN 2018 Variables de valor BATS
##########################################################


# PCA ya con salary y variables del valor del jugador
colnames(dat_campo_bateo_2018)
# filtrar variables
Variables_Valor<- colnames(dat_campo_bateo_2018[,c(36:43,45,46,52,53)])
# se análiza con base a estas variables 
#[1] "G.val"  "PA.val" "Rbat"   "Rbaser"
#[5] "Rdp"    "Rfield" "Rpos"   "RAA"   
#[9] "Rrep"   "RAR"    "oRAR"   "Salary"

temporal<- dat_campo_bateo_2018[,Variables_Valor]
colnames(dat_campo_bateo_2018[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% names
# PCA
Campo_valores <- prcomp(temporal[,-12], scale=T)
scores<- as.data.frame(Campo_valores$x)
clase<- as.factor(clase)

once_clase <-plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3,color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_campo_bateo_2018[indComp,"BATS"]) %>% 
  layout(title="Salario de jugadores por clase y brazo que batea PCA 3D",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))



htmlwidgets::saveWidget(as_widget(once_clase), "once_clase.html")

#############
# RESUMEN
#############
# no se rescata mucho del salario y carácteristicas del bateo.


