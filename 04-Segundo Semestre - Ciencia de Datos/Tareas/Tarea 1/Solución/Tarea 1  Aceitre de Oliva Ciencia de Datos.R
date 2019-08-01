# # # # # # # # # # # # # # # # # # # # # # # #  # #
# 
#  Ciencia de Datos
#
# Tarea 1.1. ACEITES DE OLIVO EN ITALIA
#
# HAIRO ULISES MIRANDA BELMONTE
# VERSION 1.0
# 04 DE FEBRERO DEL 2019
#
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE I     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# LIBRERIAS
getwd()
setwd("C:/Users/h_air/Desktop")
library("rggobi")
library("magrittr")
library("tidyverse")
library("pipeR")
library("knitr")
library("dplyr")
library("DescribeDisplay")
library("formattable")
library("grid")
library("gridExtra")

# Exportando Archivo csv
g <- ggobi(ggobi_find_file("olive.csv"))
base <- as.data.frame(g$olive.csv) # GGoobject a Data Frame
base %>% is.na %>% sum # no hay valores perdidos

# Frecuencia por región
cuadro <- base %>% group_by(Region) %>%
  summarise(n =n()) %>% 
  as.data.frame
rownames(cuadro) <- c("South", "Island", "North") 
colnames(cuadro)<- c("Region", "Frecuencia")
cuadro[2] %>% rownames_to_column(var = "Región") %>% 
  formattable

# Frecuencia por área
cuadro1 <- base %>% group_by(Area) %>% summarise(n =n())
cuadro2 <- rep(c("South", "Island", "North"), c(4,2,3)) %>% 
  as.matrix %>%
  cbind(cuadro1) %>% 
  as.data.frame
colnames(cuadro2)<- c("Region", "Area","Frecuencia")
cuadro2 %>% formattable
base %>% group_by(Region) %>% summarise(n =n()) # cuenta observaciones

# Por región: Media
tabla1 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region) %>% summarise_all(mean)  %>% 
  as.data.frame
rownames(tabla1) <- c("South", "Island", "North") 
tabla1[,2:dim(tabla1)[2]]  %>%
  rownames_to_column(var = "Región")  %>% formattable

# Por región: Varianza
tabla2 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region) %>% summarise_all(var)  %>% 
  as.data.frame
rownames(tabla2) <- c("South", "Island", "North") 
tabla2[,2:dim(tabla2)[2]]  %>%
  rownames_to_column(var = "Región")  %>% formattable
options(digits = 2)

# Por región: Mínimo
tabla3 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region) %>% summarise_all(min)  %>% 
  as.data.frame
rownames(tabla3) <- c("South", "Island", "North") 
tabla3[,2:dim(tabla3)[2]]  %>%
  rownames_to_column(var = "Región")  %>% formattable

# Por región: Máximo
tabla4 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region) %>% summarise_all(max)  %>% 
  as.data.frame
rownames(tabla4) <- c("South", "Island", "North") 
tabla4[,2:dim(tabla4)[2]]  %>%
  rownames_to_column(var = "Región")  %>% formattable

#  Por región y área: Media
tabla11 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region, base$Area) %>% summarise_all(mean)
colnames(tabla11)[1:2] <- c("Region", "Area") 
tabla11 %>% formattable
tableRep11 <- rep(c("South", "Island", "North"), c(4,2,3)) %>% 
  as.data.frame 
tableRep11 %>% cbind(tabla11[,2:dim(tabla22)[2]]) %>% formattable

#  Por región y área: Varianza
tabla22 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region, base$Area) %>% summarise_all(var)
colnames(tabla22)[1:2] <- c("Region", "Area") 
tabla22 %>% formattable
tableRep22 <- rep(c("South", "Island", "North"), c(4,2,3)) %>% 
  as.data.frame 
tableRep22 %>% cbind(tabla22[,2:dim(tabla22)[2]]) %>% formattable

#  Por región y área: Mínimo
  tabla33 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region, base$Area) %>% summarise_all(min)
colnames(tabla33)[1:2] <- c("Region", "Area") 
tabla33 %>% formattable
tableRep33 <- rep(c("South", "Island", "North"), c(4,2,3)) %>%
  as.data.frame 
tableRep33 %>% cbind(tabla33[,2:dim(tabla22)[2]]) %>% formattable

#  Por región y área: Máximo
tabla44 <-base[, 3:dim(base)[2]] %>%
  group_by(base$Region, base$Area) %>% summarise_all(max)
colnames(tabla44)[1:2] <- c("Region", "Area") 
tabla44 %>% formattable
tableRep44 <- rep(c("South", "Island", "North"), c(4,2,3)) %>% 
  as.data.frame 
tableRep44 %>% cbind(tabla44[,2:dim(tabla22)[2]]) %>% formattable

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE IT    ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#  Graficos Ggobit interfaz R rrgobi
regiones <- dd_load("C:/Users/h_air/Desktop/region") # importando imagen

# Gráfico de barras
r <- ggplot(regiones) + 
  labs(title = "Olive Oils", 
       subtitle = "Regions", 
       caption = "Rojo: Morado: Verde") + 
  labs( y ="Frecuencia") 
  
areas <- dd_load("C:/Users/h_air/Desktop/areas") # importando imagen

# Gráfico de barras
ar <- ggplot(areas) + 
  labs(title = "Olive Oils by Region", 
       subtitle = "Areas", 
       caption ="Rojo: North: Morado:  Island; Verde: South") + 
  labs( y ="Frecuencia") 

grid.arrange(r,ar) # uniendo gráficos

plot1 <- dd_load("C:/Users/h_air/Desktop/plot1")  # importando imagen

p1 <- ggplot(plot1) + 
  labs(title = "Olive Oils by Region", 
       subtitle = "All fatty acids", 
       caption = "Rojo: North: Morado:  Island; Verde: South") 

# Cuadro de coeficientes y escalares de la matriz de proyección
pesos <- matrix(plot1$plots[[1]]$params$F,8,2)
escalar <- as.matrix(c(1143,265,233,2110,1022,74,105,57))
pesos <- cbind(pesos,escalar)
colnames(pesos) <- c("Dim1","Dim2","Escalar")
rownames(pesos) <- 3:10
as.data.frame(pesos) %>% formattable

plot2 <- dd_load("C:/Users/h_air/Desktop/plot2")  # importando imagen

p2 <- ggplot(plot2) + 
  labs(title = "Olive Oils by Region", 
       subtitle = "All fatty acids", 
       caption = "Rojo: North: Morado:  Island; Verde: South") 

plot3 <- dd_load("C:/Users/h_air/Desktop/plot3")  # importando imagen

p3 <- ggplot(plot3) + 
  labs(title = "Olive Oils by Region", 
       caption = "Rojo: North: Morado:  Island; Verde: South") 

plot4 <- dd_load("C:/Users/h_air/Desktop/plot4")  # importando imagen

p4 <-ggplot(plot4) + 
  labs(title = "Olive Oils by Region", 
       subtitle = "Matrix Corraletion", 
       caption = "Rojo: North: Morado:  Island") 

plot5 <- dd_load("C:/Users/h_air/Desktop/plot5")  # importando imagen

p5 <- ggplot(plot5) + 
  labs(title = "Olive Oils by Region", 
       caption = "Rojo: North: Morado:  Island") 

plot6 <- dd_load("C:/Users/h_air/Desktop/plot6")  # importando imagen

p6 <- ggplot(plot6) + 
  labs(title = "Olive Oils by Region", 
       caption = "Rojo: North: Morado:  Island") 

grid.arrange(p5,p6) # uniendo gráficos

plot7 <- dd_load("C:/Users/h_air/Desktop/plot7")  # importando imagen
p7 <- ggplot(plot7) + 
  labs(title = "Olive Oils by Region South", 
       subtitle = "All fatty acids", 
       caption = " Verde: South") 

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               FIN        ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
