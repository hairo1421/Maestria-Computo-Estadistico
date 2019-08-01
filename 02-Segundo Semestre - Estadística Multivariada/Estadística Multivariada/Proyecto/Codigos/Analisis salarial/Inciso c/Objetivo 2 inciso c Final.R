###############################
#
# Obj. II 	Predecir salario 
#
###############################

############################
# Inciso c
###########################

######################################################################
# 	Para pitchers con datos de pitchers (Exp y clasi)
######################################################################

# Ptcher y salarios

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
# todos menos bateador
base_picheo<- read.csv("dat_picheo_RL.csv", head=T) 
base_picheo


###############################
# Base Bateo 2018
###############################

## Generacion de dat_campo_bateo_2018
dat_picheo_RL_2018<- base_picheo[which(base_picheo$year=="2018"),]



##############################
# VARIABLE SALARIO CONTINUA
##############################




# filtrar salario y hacerlo numerico
salarios<- dat_picheo_RL_2018$Salary
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
sum(is.na(salarios)) # 346
sum(dat_picheo_RL_2018$Salary=="")
dat_picheo_RL_2018$Salary<-salarios
dat_picheo_RL_2018$Salary %>% length() # 800
##############################
# VARIABLE SALARIO CLASES
##############################

clase<-rep(-1,times=nrow(dat_picheo_RL_2018))
quantiles <- dat_picheo_RL_2018$Salary %>% na.omit %>% quantile(c(.25,.50,.75))
# 557125 1500000 5075000 


for(i in 1:nrow(dat_picheo_RL_2018)) {
  if(!is.na(dat_picheo_RL_2018$Salary[i])){
    if(dat_picheo_RL_2018$Salary[i]<quantiles[1]){
      clase[i]<- 1
    }
    else {
      if(dat_picheo_RL_2018$Salary[i]<quantiles[2]){
        clase[i]<- 2
      }
      else {
        if(dat_picheo_RL_2018$Salary[i]<quantiles[3]){
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
summary(dat_picheo_RL_2018$Salary)


########################################################
# SALARIO CONTINUO
########################################################


########################################################
# Pitchers 2018 Variables básicas
########################################################

####################
# Variable estandar
####################

dat_picheo_RL_2018 %>% names
dat_picheo_RL_2018[1:36] %>% names# Estandar
#[1] "Rk.st" "Name"  "Id"    "Age"   "Tm.st" "Lg"   
#[7] "W"     "L"     "W.L."  "ERA"   "G.st"  "GS.st"
#[13] "GF"    "CG"    "SHO"   "SV"    "IP.st" "H"    
#[19] "R.st"  "ER"    "HR"    "BB"    "IBB"   "SO"   
#[25] "HBP"   "BK"    "WP"    "BF"    "ERA."  "FIP"  
#[31] "WHIP"  "H9"    "HR9"   "BB9"   "SO9"   "SO.W" 
dat_picheo_RL_2018[7:36] # agrega salario


####################
# Valor del jugador
####################

dat_picheo_RL_2018[37:59] %>% names # Play value
#[1] "year"     "Rk.val"   "Tm.val"   "IP.val"  
#[5] "G.val"    "GS.val"   "R.val"    "RA9"     
#[9] "RA9opp"   "RA9def"   "RA9role"  "PPFp"    
#[13] "RA9avg"   "RAA"      "WAA"      "gmLI"    
#[17] "WAAadj"   "WAR"      "RAR"      "waaWL."  
#[21] "X162WL."    
dat_picheo_RL_2018[40:57] %>% names


####################
# Win prob
####################

dat_picheo_RL_2018[60:79] %>% names# Win Probability
#[1] "Rk"        "Tm"        "IP"        "PtchR"    
#[5] "PtchW"     "Plays"     "WPA"       "WPA."     
#[9] "WPA..1"    "aLI"       "WPA.LI"    "Clutch"   
#[13] "RE24"      "REW"       "boLI"      "RE24.boLI"
#[17] "LevHi"     "LevMd"     "LevLo"    
dat_picheo_RL_2018[62:79] %>% names 

dat_picheo_RL_2018[79] %>% names #  "THROWS"   
dat_picheo_RL_2018[58]  %>% names #  "Salary" 

#################################
# VARIABLES BÁSICAS DE CAMPO PCA
#################################

Varibles_base <- names(dat_picheo_RL_2018)[7:36]
Varibles_base <- c(Varibles_base,names(dat_picheo_RL_2018)[58])
#[1] "W"      "L"      "W.L."   "ERA"    "G.st"  
#[6] "GS.st"  "GF"     "CG"     "SHO"    "SV"    
#[11] "IP.st"  "H"      "R.st"   "ER"     "HR"    
#[16] "BB"     "IBB"    "SO"     "HBP"    "BK"    
#[21] "WP"     "BF"     "ERA."   "FIP"    "WHIP"  
#[26] "H9"     "HR9"    "BB9"    "SO9"    "SO.W"  
#[31] "Salary"

temporal<- dat_picheo_RL_2018[,Varibles_base]
colnames(dat_picheo_RL_2018[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim # 393  31
# PCA

Campo_valores <- prcomp(temporal[, -31], scale=T)
scores<- as.data.frame(Campo_valores$x)

uno_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"Salary"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(uno_pitcher), "uno_pitcher.html")

#############
# RESUMEN
#############

# Con estas variables se observa ligeramente que los pitcher
# que ganan más se agrupan; sin embargo se confunde con algunos
# que ganan menos
temporal %>% names 

##########################
# BATS Variables báse
##########################

Campo_valores <- prcomp(temporal[, -31], scale=T)
scores<- as.data.frame(Campo_valores$x)

dos_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(dos_pitcher), "dos_pitcher.html")

######################
# RESUMEN
######################
# Con estas variables no se diferencia si el pitcher es izquierdo o derecha,
# tampoco se encuentra algún patron interesante sobre si los izquierdos
# ganan más que los derechos 

######################################################################
# Valor del jugador Pitcher
######################################################################
dat_picheo_RL_2018[40:57] %>% names
Varibles_Estadisticas <- colnames(dat_picheo_RL_2018[40:57])
Varibles_Estadisticas <- c(Varibles_Estadisticas,"Salary")
#[1] "IP.val"  "G.val"   "GS.val"  "R.val"  
#[5] "RA9"     "RA9opp"  "RA9def"  "RA9role"
#[9] "PPFp"    "RA9avg"  "RAA"     "WAA"    
#[13] "gmLI"    "WAAadj"  "WAR"     "RAR"    
#[17] "waaWL."  "X162WL." "Salary" 

temporal<- dat_picheo_RL_2018[,Varibles_Estadisticas]
colnames(dat_picheo_RL_2018[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim # 342  19
# PCA
Campo_valores <- prcomp(temporal[, -19], scale=T)
scores<- as.data.frame(Campo_valores$x)

tres_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"Salary"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(tres_pitcher), "tres_pitcher.html")

#############
# RESUMEN
#############

# Utilizando estas variables se observan dos grupos; no obstante,
# los grupos no están dado por el nivel salarial del jugador
temporal %>% names 

##########################
# THROWS Variables báse
##########################
temporal %>% names
Campo_valores <- prcomp(temporal[, -19], scale=T)
scores<- as.data.frame(Campo_valores$x)

cuatro_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cuatro_pitcher), "cuatro_pitcher.html")

######################
# RESUMEN
######################
# Al igual  que el caso anterior, los dos grupos que se forman
# no distinguen si el pitcher lanza con el brazo izquierdo o derecho;
# son embargo, los dos grupos siguen presentes.

#######################
# Por IP.val
#######################
# En base a lo anterior se observaron dos grupos, y se tiene la sospecha
# que los grupos se separan por el número total de entradas lanzadas
# por el pitcher IP. De esta forma se tienen por un lado que 
# estas variables discriminan a aquellos pitcher que son más recurridos
# en zona de lanzamiento.

# Para observar lo anterior se vuelve a realizar PCA sin contabilizar
# salario y la variable IP

temporal %>% names
Campo_valores <- prcomp(temporal[, -c(1,19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

cinco_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"IP.val"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA  pitcher total de entradas lanzadas temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cinco_pitcher), "cinco_pitcher.html")



################
# Resumen
################
# Con las variables del valor del jugador o estadísticas
# el grupo que se genera se distingue por la variable IP

# En clasificación este grupo de variables puede ser importante
# ya que captura algo en los pitchers. 

#######################
# Retirando IP.val
#######################
# Se retira IP para observar el comportamiento del salario y brazo
temporal %>% names
Campo_valores <- prcomp(temporal[, -c(1,19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

seis_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(seis_pitcher), "seis_pitcher.html")

##############
# Resumen
#############
# extrayendo IP de la base los dos grupos no son por el nivel salarial.

##########################################################
#  Pitcher variables Probability Win
##########################################################
dat_picheo_RL_2018[62:79] %>% names 

# filtrar variables
Variables_Valor<- colnames(dat_picheo_RL_2018[c(62:79,58)] )
#[1] "IP"        "PtchR"    
#[3] "PtchW"     "Plays"    
#[5] "WPA"       "WPA."     
#[7] "WPA..1"    "aLI"      
#[9] "WPA.LI"    "Clutch"   
#[11] "RE24"      "REW"      
#[13] "boLI"      "RE24.boLI"
#[15] "LevHi"     "LevMd"    
#[17] "LevLo"     "THROWS", [19] "Salary"   

temporal<- dat_picheo_RL_2018[,Variables_Valor]
colnames(dat_picheo_RL_2018[,Variables_Valor])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim # 588  19
# PCA

Campo_valores <- prcomp(temporal[, -c(18,19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

siete_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"Salary"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(siete_pitcher), "siete_pitcher.html")

#############
# RESUMEN
#############

# Utilizando estas variables se observan dos grupos; no obstante,
# los grupos tratan de dividirse por nivel salarial pero se tiene problemas 
# dado a que se confunden

temporal %>% names 

##########################
# THROWS Variables báse
##########################


ocho_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(ocho_pitcher), "ocho_pitcher.html")

######################
# RESUMEN
######################
# Al igual  que el caso anterior, los dos grupos que se forman
# no distinguen si el pitcher lanza con el brazo izquierdo o derecho;
# sin embargo, los dos grupos siguen presentes.

#######################
# Por IP.val
#######################
# En base a lo anterior se observaron dos grupos, y se tiene la sospecha
# que los grupos se separan por el número total de entradas lanzadas
# por el pitcher IP. De esta forma se tienen por un lado que 
# estas variables tambipen discriminan a aquellos pitcher que son más recurridos
# en zona de lanzamiento.

# Para observar lo anterior se vuelve a realizar PCA sin contabilizar
# salario y la variable IP

temporal %>% names
Campo_valores <- prcomp(temporal[, -c(1,18,19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

nueve_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"IP.val"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA entradas lanzadas por pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(nueve_pitcher), "nueve_pitcher.html")



################
# Resumen
################
# Con las variablesde probability to win 
# el grupo que se genera se distingue por la variable IP

# En clasificación este grupo de variables puede ser importante
# ya que captura algo en los pitchers. 

############################################
# Retirando IP.val y por salary y brazo
#############################################
# Se retira IP para observar el comportamiento del salario y brazo
temporal %>% names
Campo_valores <- prcomp(temporal[, -c(1,18, 19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

diez_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"Salary"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(diez_pitcher), "diez_pitcher.html")

##############
# Resumen
#############
# extrayendo IP de la base se observa que con estas variables,
# ligeramente se separan aquellos pitcher que ganan más
# que es de esperarse ya  que si juegan más entonces, registran más
# información sobre las variables de win probability y por ende
# al jugar más tienen la oportunidad de mejor desempeño y con ellos
# mejor salario.

# Por lo tanto estas variables y IP son importantes




########################################################
########################################################
########################################################
## SALARIO Discreto 
########################################################
########################################################
########################################################



Varibles_base <- names(dat_picheo_RL_2018)[7:36]
Varibles_base <- c(Varibles_base,names(dat_picheo_RL_2018)[58])
#[1] "W"      "L"      "W.L."   "ERA"    "G.st"  
#[6] "GS.st"  "GF"     "CG"     "SHO"    "SV"    
#[11] "IP.st"  "H"      "R.st"   "ER"     "HR"    
#[16] "BB"     "IBB"    "SO"     "HBP"    "BK"    
#[21] "WP"     "BF"     "ERA."   "FIP"    "WHIP"  
#[26] "H9"     "HR9"    "BB9"    "SO9"    "SO.W"  
#[31] "Salary"

temporal<- dat_picheo_RL_2018[,Varibles_base]
colnames(dat_picheo_RL_2018[,Varibles_base])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim # 393  31
# PCA

Campo_valores <- prcomp(temporal[, -31], scale=T)
scores<- as.data.frame(Campo_valores$x)

uno_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="PCA salarios por clase pitchers temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(uno_clase_pitcher), "uno_clase_pitcher.html")

#############
# RESUMEN
#############
# Se observan ligeramente dos grupos pero el salario no los diferencia.
# existe otra variable dentro de estas que hace este efecto.

# Con estas variables se observa ligeramente que los pitcher que ganan
# la cuarta clase (amás alta), se agrupan a un lado del plano; sin embargo,
# esto no es muy claro ya que se confunden con aquellos que s salario
# es de la primera clase. 

# La clase 3 y 2 se confunden bastante.

temporal %>% names 

##########################
# BATS Variables báse
##########################

Campo_valores <- prcomp(temporal[, -31], scale=T)
scores<- as.data.frame(Campo_valores$x)

dos_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=clase[indComp])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(dos_clase_pitcher), "dos_clase_pitcher.html")

######################
# RESUMEN
######################
# Este grupo no se diferencia por el brazo con el que lanza el pitcher, tampoco el 
# salario muestra algun patron sobre si los izquierdos ganan más que los que batean con
# el brazo derecho.



# La agrupación se debe por las variables W, H, L, que son las que discriminan
# el tipo de pitcher, sin embargo aún así no se diferencia el salario

# Se utiliza PCA en los 3 casos y sin contemplar las variables W, H, L, 
# para observar los grupos y ver el salario de nuevo
temporal %>% names
Campo_valores <- prcomp(temporal[, -c(1,2,12,31)], scale=T)
scores<- as.data.frame(Campo_valores$x)

# Número de hits permitidos por un pitcher
prueba0 <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"H"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="PCA hits permitidos por un pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(prueba0), "prueba0.html")

#Número de juegos ganados acreditados al pitcher
prueba1 <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"W"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="PCA juegos ganados y acreditados al pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(prueba1), "prueba1.html")

#Número de juegos perdidos debidos al pitcher
prueba2 <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=dat_picheo_RL_2018[indComp,"L"])%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=clase[indComp]) %>% 
  layout(title="PCA  juegos perdidos y acreditados al pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(prueba2), "prueba2.html")

######################
# Resumen
######################
# Al realizar lo anterior y retirar las variables, los dos grupos 
# se presentan de una forma más compacta; no obstante, al observa 
# por las variables H,W, L, estas variablas discriminan el desempeño
# del valor del jugador en términos de aportar a su equipo, 
# pero se debe tener en cuenta que estas variables no
# tienen algun tipo de consecuencia en su salario.


################################
## CONCLUSIÓN BUENA
###############################
# De esta manera, estas variables os ayudan a encontrar algunos jugadores
# que aporten a las victorias de sus equipos y que puedan ser contratados
# a salarios no tan altos

# muchos no aportan a su equipo y ganan mucho
# no obstante, estas variables no son buenas predictoras del salario
# se necesitan unas mejores para cruzar esta información, 

# te predice que tan bueno es aportando al equipo
# te predice su salario (se necesita encontrar)
# entonces podemos ver que si tiene carácteristicas de salarios
# bajos podriamos buscar entre ellos aquellos que aportan más, en base
# a estas variables.


######################################################################
# Valor del jugador Pitcher
######################################################################
dat_picheo_RL_2018[40:57] %>% names
Varibles_Estadisticas <- colnames(dat_picheo_RL_2018[40:57])

#[1] "IP.val"  "G.val"   "GS.val"  "R.val"  
#[5] "RA9"     "RA9opp"  "RA9def"  "RA9role"
#[9] "PPFp"    "RA9avg"  "RAA"     "WAA"    
#[13] "gmLI"    "WAAadj"  "WAR"     "RAR"    
#[17] "waaWL."  "X162WL." 

temporal<- cbind(dat_picheo_RL_2018[,Varibles_Estadisticas], clase)
temporal$clase <- temporal$clase %>% as.character %>% as.numeric()
temporal$clase[temporal$clase==-1] <- NA
colnames(dat_picheo_RL_2018[,Varibles_Estadisticas])
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim #  342  19
# PCA
temporal %>% names
Campo_valores <- prcomp(temporal[,-19], scale=T)
scores<- as.data.frame(Campo_valores$x)

tres_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=as.factor(temporal$clase))%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=as.factor(temporal$clase)) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(tres_clase_pitcher), "tres_clase_pitcher.html")


#############
# RESUMEN
#############

# Utilizando estas variables se observan dos grupos; no obstante,
# los grupos no están dado por el nivel salarial del jugador, posiblemente al igual
# que  al utiliza los salarios continuos la variable IP es la discrimina
# al grupo de pitchers ( sí se ocnfirmó lo dicho, solo que nno se guardó la imagen)

temporal %>% names 


##########################
# THROWS Variables báse
##########################
temporal %>% names
Campo_valores <- prcomp(temporal[, -19], scale=T)
scores<- as.data.frame(Campo_valores$x)

cuatro_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=as.factor(temporal$clase))%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cuatro_clase_pitcher), "cuatro_clase_pitcher.html")

######################
# RESUMEN
######################
# Al igual  que el caso anterior, los dos grupos que se forman
# no distinguen si el pitcher lanza con el brazo izquierdo o derecho;
# sin embargo, los dos grupos siguen presentes dado a la variable IP, las veces 
# que lanza el pitcher en la temporada.


##########################################################
#  Pitcher variables Probability Win
##########################################################
dat_picheo_RL_2018[62:79] %>% names 

# filtrar variables
Variables_Valor<- colnames(dat_picheo_RL_2018[c(62:79)] )
#[1] "IP"        "PtchR"    
#[3] "PtchW"     "Plays"    
#[5] "WPA"       "WPA."     
#[7] "WPA..1"    "aLI"      
#[9] "WPA.LI"    "Clutch"   
#[11] "RE24"      "REW"      
#[13] "boLI"      "RE24.boLI"
#[15] "LevHi"     "LevMd"    
#[17] "LevLo"     "THROWS"   [19] "clase" 

temporal<- cbind(dat_picheo_RL_2018[,Variables_Valor], clase)
temporal$clase <- temporal$clase %>% as.character %>% as.numeric()
temporal$clase[temporal$clase==-1] <- NA
indComp<-complete.cases(temporal)
indComp %>% length()
# solo casos completos
temporal<- temporal[complete.cases(temporal),]
temporal %>% dim # 402  19
temporal %>% names

# PCA
Campo_valores <- prcomp(temporal[, -c(18,19)], scale=T)
scores<- as.data.frame(Campo_valores$x)

cinco_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=as.factor(temporal$clase))%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=as.factor(temporal$clase)) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(cinco_clase_pitcher), "cinco_clase_pitcher.html")

#############
# RESUMEN
#############

# Utilizando estas variables se observan dos grupos; no obstante,
# los grupos tratan de dividirse por nivel salarial pero se tiene problemas 
# dado a que se confunden

# Nota: los grupos de enmedio se confunden bastante

temporal %>% names 

##########################
# THROWS Variables báse
##########################


seis_clase_pitcher <- plot_ly(scores,x=scores$PC1,y=scores$PC2,z=scores$PC3, color=as.factor(temporal$clase))%>%
  add_markers()  %>% add_text(x=scores$PC1,y=scores$PC2,z=scores$PC3,text=dat_picheo_RL_2018[indComp,"THROWS"]) %>% 
  layout(title="PCA salarios pitcher temporada 2018",
         scene = list(xaxis = list(title = 'Dim 1'),
                      yaxis = list(title = 'Dim 2'),
                      zaxis = list(title = 'Dim 3')))

htmlwidgets::saveWidget(as_widget(seis_clase_pitcher), "seis_clase_pitcher.html")

######################
# RESUMEN
######################
# Al igual  que el caso anterior, los dos grupos que se forman
# no distinguen si el pitcher lanza con el brazo izquierdo o derecho;
# sin embargo, los dos grupos siguen presentes.
