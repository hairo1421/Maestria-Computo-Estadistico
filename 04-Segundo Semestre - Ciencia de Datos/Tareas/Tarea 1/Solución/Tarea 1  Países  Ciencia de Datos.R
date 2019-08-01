# # # # # # # # # # # # # # # # # # # # # # # #  # #
# 
#  Ciencia de Datos
#
# Tarea 1.2. BASE DE DATOS SUICIDE_NO
#
# HAIRO ULISES MIRANDA BELMONTE
# VERSION 1.0
# 04 DE FEBRERO DEL 2019
#
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
#
# CONTENIDO DEL ARCHIVO.
#
# De la PARTE 1 a 4 , se realiza lo que se observa en el archivo 
# pdf "Tarea 1 Países Ciencia de datos".
#
# En la parte 5, se realiza lo que se observa en el archivo 
# pdf "Tarea 1 Países México Ciencia de datos"
#
# En la parte 6, se realiza lo que se observa en el archivo 
# pdf "Tarea 1 Países USA Ciencia de datos"
#
#
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE I     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 


# LIBRERIAS NECESARIAS
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

# Importando base de datos
datos <- read.csv("C:/Users/h_air/Desktop/suicide_data.csv")


# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE II     ## # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

###############################################
###############################################
###########Describiendo base de datos##########
###############################################


# dimensión de la base
datos %>% dim

# Nombre de las variables
tabla1 <- datos %>% colnames %>%  as.matrix
NombreVariables <- c("País", "Año", "Sexo", "Edad por Rangos", 
                     "Número de suicidios", "Población", 
                     "Suicidios por cada 100k", "País_Año",
                     "índice de desarrollo humano",
                     "GDP","GDP per capita","Generación")  
tabla1 <- cbind(tabla1, NombreVariables) 
colnames(tabla1) <- c("Variable", "Descripción") 
tabla1 %>%  as.data.frame  %>%  formattable

# número de observaciones
tabla2 <- datos %>% count(ï..country) 
colnames(tabla2) <- c("Países","Frecuencia") 
tabla2 <- tabla2 %>% arrange(desc(Frecuencia))

# países y nombres
datos[,1] %>% factor %>% levels %>% length

# años, son 32, de 1985 al 2016
datos[,2] %>% factor %>% levels 

# sexo, mujer y hombre
datos[,3] %>% factor %>% levels 

# sexo como dummi hombre 0, mujer 1
sexInt <- as.integer(datos$sex)
datos <- cbind(datos,sexInt)
datos <- mutate(datos, sexInt1= ifelse(sexInt == 2, 0, 1))
datos <- datos %>% select(c(-13))
head(datos[,c(3,13)])

# Edad rangos 6 rangos
datos[,4] %>% factor %>% levels

# número de suicidios observaciones registradas en todos los años
datos[,5] %>% is.na %>% sum # cero valores perdidos
tabla3 <-aggregate(datos$suicides_no, by=list(datos$ï..country),
          FUN=sum) 
colnames(tabla3) <- c("Países","Número de suicidios")
tabla3 %>%  formattable

# 15 países que han registrado el mayor crimen
top15 <- head(arrange(tabla3,desc(tabla3$`Número de suicidios`)),15)  %>%  formattable

# No se está contrlando por años
p1 <-ggplot(algo)+geom_point(aes(x=algo$`Número de suicidios`,y=algo$Países), color=algo$`Número de suicidios`) +   labs(title = "Píases con mayor número de suicidio", 
            y="Países", x="# de Suicidios")

# Agregado de suicidios por país y año
tabla4 <-aggregate(datos$suicides_no, by=list(datos$ï..country, datos$year),
         FUN=sum) 
tabla4 <-tabla4 %>% arrange(desc(x))  
tabla4 %>%  formattable

# la información NA no es que tuvieran NA
# en la base, si no que no registraron ese años
# No estás cuadrado el panel

# Filtro para los países con mayor número de sucidio
# por año  
top15_2 <- as.vector(top15[,1])
tabla4 %>% filter(tabla4$Group.1==top15_2[1]|
                        tabla4$Group.1==top15_2[2]|
                        tabla4$Group.1==top15_2[3]|
                        tabla4$Group.1==top15_2[4]|
                        tabla4$Group.1==top15_2[5]|
                        tabla4$Group.1==top15_2[6]|
                        tabla4$Group.1==top15_2[7]|
                        tabla4$Group.1==top15_2[8]|
                        tabla4$Group.1==top15_2[9]|
                        tabla4$Group.1==top15_2[10]|
                        tabla4$Group.1==top15_2[11]|
                        tabla4$Group.1==top15_2[12]|
                        tabla4$Group.1==top15_2[13]|
                        tabla4$Group.1==top15_2[14]|
                        tabla4$Group.1==top15_2[15]) %>%
  spread(Group.2 , x) %>%  formattable

# Filtro para los países con mayor número de sucidio
# por año  
tabla5 <-tabla4 %>% filter(tabla4$Group.1==top15_2[1]|
                    tabla4$Group.1==top15_2[2]|
                    tabla4$Group.1==top15_2[3]|
                    tabla4$Group.1==top15_2[4]|
                    tabla4$Group.1==top15_2[5]|
                    tabla4$Group.1==top15_2[6]|
                    tabla4$Group.1==top15_2[7]|
                    tabla4$Group.1==top15_2[8]|
                    tabla4$Group.1==top15_2[9]|
                    tabla4$Group.1==top15_2[10]|
                    tabla4$Group.1==top15_2[11]|
                    tabla4$Group.1==top15_2[12]|
                    tabla4$Group.1==top15_2[13]|
                    tabla4$Group.1==top15_2[14]|
                    tabla4$Group.1==top15_2[15])

# Gráfico de burbujas, representa la tabla de arriba
tabla5 %>% spread(Group.2 , x) %>%  formattable
colnames(tabla5) <-c("Pais","Año","Suicidios")
buble <- ggplot(tabla5, aes(x=Año, y=Pais, size=Suicidios, color=Pais)) +
  geom_point(alpha=0.7) + labs(title="Número de suicidios")

# Población
datos[,5] %>% is.na %>% sum 

# dominica tiene suicidio cero y Saint Kitts and Nevis Ponerl0
aggregate(datos$suicides_no, by=list(datos$ï..country),
          FUN=sum) %>% filter(x==0)

# sí tienen población
aggregate(datos$population, by=list(datos$ï..country),
          FUN=sum) %>% filter(Group.1 =="Dominica" | Group.1 ==
                                "Saint Kitts and Nevis")
tabla5 <- aggregate(datos$suicides_no, by=list(datos$ï..country, datos$year),
                    FUN=sum) 
tabla5 %>% spread(Group.2 , x) %>%  formattable 


# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE III     ## # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#######################################################
#######################################################
###########sección de panles###########################
#######################################################
#######################################################

# Se realiza el agregado de algunas variables
datos2 <- aggregate(datos$population, by=list(datos$ï..country, datos$year),
                    FUN=sum)
head(datos2)
datos2 <- aggregate(datos$suicides_no, by=list(datos$ï..country, datos$year),
                    FUN=sum) %>% cbind(datos2[,3])
colnames(datos2) <- c("País", "Año",
                      "suicides_no","population")
datos2 <- mutate(datos2, suicidesRatio=suicides_no/population)
head(datos2)

# Transformar GDP Y GDP PERCAPITA A NUMERICO
# se realiza un left_join 
# para realizar match con los plots y solo tener
# un gdp para un año en cada país
dim(distinct(datos, gdp_for_year....))
gdp <- datos[c(1,2,9,10,11)] 
colnames(gdp) <- c("País","Año","HDI","GDP","GDP_pc")
dim(gdp)
duplicated(gdp)
c <-seq(1,dim(gdp)[1],12)
gdp <- gdp[c,]
z <- arrange(gdp,desc(-gdp$Año))
head(z)

# Base con gdp y gdp percapita como númerico
datos3 <-left_join(datos2, z, by=c("País","Año"))
head(datos3)
options(scipen=999)
as.numeric(gsub("," ,"", as.character(datos3$GDP)))
datos3 <-mutate(datos3, gdp_pc2 = as.numeric(gsub("," ,"", as.character(datos3$GDP)))/population)
datos3 <-mutate(datos3, gdp2 = as.numeric(gsub("," ,"", as.character(datos3$GDP))))
head(datos3)

# relación entre los indicadores de desarrollo social
# gdp y suicidios
ggplot(as.data.frame(na.omit(datos3)), aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title="", x="GDP per capita")                                                                                      

# limpiando valores perdidos
datos4 <- datos3
datos4$HDI[is.na(datos3$HDI)] <- 0
datos4$gdp2[is.na(datos4$gdp2)] <- 0

# Correlación entre HDI y gdp2
correlacion <- as.data.frame(cor(datos4$gdp2,datos4$HDI)) 
colnames(correlacion) <- c("Correlación")
correlacion %>%  formattable

# Número de NA como valores perdidos
length(datos3$HDI)-sum(is.na(datos3$HDI))
length(datos3$gdp_pc2)-sum(is.na(datos3$gdp_pc2))
dev.off()
head(datos4)

# plot por años HDI respecto GDP_PER-CAPITA
g05 <-datos4 %>% filter(datos4$Año=="2005" & (datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title="HDI~GDP per-capita",
                               subtitle = "2005",
                               x="GDP per-capita")
g10<-datos4 %>% filter(datos4$Año=="2010" & (datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title= "2010", x="GDP per-capita")
g14<-datos4 %>% filter(datos4$Año=="2014" & ( datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title= "2014", x="GDP per-capita")
grid.arrange(g05,g10, g14)

# plot por años HDI respecto GDP_PER-CAPITA contemplando suicidios
g11<-datos4 %>% filter(datos4$Año=="2005" & ( datos4$HDI>0.1)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2005")
g12<-datos4 %>% filter(datos4$Año=="2010" & ( datos4$HDI>0.1)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2010")
g13 <- datos4 %>% filter(datos4$Año=="2014" & ( datos4$HDI>0.1)) %>%
ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2014")
grid.arrange(g11,g12, g13)

# Un gráfico de correlación sin diferenciar años
head(datos4 %>% filter(datos4$HDI > 0) %>%arrange(desc(-HDI)),15)
head(datos4)
ggcorr(datos4[c(3,4,5,6,9,10)])

# con filtro de años
datos4_14 <-datos4 %>% filter(datos4$Año=="2014" & ( datos4$HDI>0.1))
cor1 <-   ggcorr(datos4_14[c(3,4,5,6,9,10)])  + labs(title="2014")
datos4_10 <-datos4 %>% filter(datos4$Año=="2010" & ( datos4$HDI>0.1))
cor2 <-   ggcorr(datos4_10[c(3,4,5,6,9,10)])  + labs(title="2010")
grid.arrange(cor1,cor2)
ggpairs(df,  diag = list(continuous = "naDiag")) 

# 2010
cplot1 <- ggpairs(datos4_10[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
  labs(title="2010")
# 2014
cplot2<-ggpairs(datos4_14[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
labs(title="2014")

# toma todos los años por factor tiempo
datos4 %>%
  ggplot()+ aes(y=datos4$suicides_no, x=datos4$País, group=País) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  datos4 %>%
  ggplot()+ aes(y=datos4$suicidesRatio, x=datos4$País, group=País) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE IV     ## # # # # # #  
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#####################################################
###############EJERCICIO DE REGRESIÓN#################
######################################################
library(foreign)
library("jtools")
install.packages("plm")
library(plm)
library(lmtest)

# PANEL POOLED
ols <-lm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3)
ols %>% summ(confint = TRUE, digits = 3)
plot_summs(ols)

# OLS con dummy
fixed.dum <-lm(datos3$gdp_pc2~ datos3$HDI +datos3$suicidesRatio+ factor(datos3$País) - 1, data=datos3)
fixed.dum %>% summ(confint = TRUE, digits = 3)

# Efectos fijos
fixed <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("País", "Año"), model="within")
fixed %>% summary
fixef(fixed)

pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed
# no rechazo la nula, entonces es mejor ols que fija

# EFECTOS ALEATORIOS
ran <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("País", "Año"), model="random")
ran %>% summary

# Prueba F es signficativa lo cual nos indica un buen modelo
phtest(fixed, ran)
# los datos sugieren utilizar efectos aleatorios
# Ho errores unicos correlacionadas con la regresara vs
# no lo están <.05 se rechaza la nula y utilizo random

# REGRESION TIPO  POOLED
pool <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("País", "Año"), model="pooling")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))
# no es necesario el panel

# AUTOCORRELACIÓN SERIAL
pbgtest(fixed)
pbgtest(ran) # no correlación
pbgtest(pool) # no correlación

# PRUEBA DE HETEROCEDASTICIDAD
bptest(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data = datos3, studentize=F)
#existe heterocedasticidad

# ROBUSTES EN LOS COEFICIENTES 
coeftest(ran) 
coeftest(ran, vcovHC) # # Heteroskedasticity consistent coefficients
coeftest(pool) 
coeftest(pool, vcovHC) # # Heteroskedasticity consistent coefficients

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE V     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#############################################################
###############    Mexico  ##################################
#############################################################
#############################################################
# Tarea 1 Países MEXICO Ciencia de datos

# Filtrando para México
MEXICO <- datos %>% filter(ï..country =="Mexico") 
head(MEXICO)
MEXICO %>% count(generation) 
MEXICO %>% count(sex) 
MEXICO %>% dim
mexico <- datos3 %>% filter(País =="Mexico") 

# POBLACIÓN MÉXICANA
head(mexico)

# Poblción por edad
pie14 <-MEXICO %>% filter(year=="2014") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Año 2014", x="Población", y="")
pie14M <-MEXICO %>% filter(year=="2014" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres Año 2014", x="Población", y="")
pie14H <-MEXICO %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Hombres Año 2014", x="Población", y="")
pie94 <-MEXICO %>% filter(year=="1994") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Año 1994", x="Población", y="")
pie94M <-MEXICO %>% filter(year=="1994" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres Año 1994", x="Población", y="")
pie94H <-MEXICO %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) 
grid.arrange(pie14,pie94) 
grid.arrange(pie14M,pie94M)
grid.arrange(pie14H,pie94H)

# HDI repotado en México
na.omit(mexico[c(2,6)]) %>% formattable

# GRAFICS TIPO SERIE DE TIEMPO
plot1 <-ggplot(mexico, aes(x=mexico$Año,y=mexico$suicidesRatio)) +
  geom_line() +
  labs(title="Tasa de suicidio por cada cien mil habitantes",
       x="Porcentaje",
       y="Año")
plot2 <- ggplot(mexico, aes(x=mexico$Año,y=mexico$gdp_pc2)) + 
  geom_line() + 
  labs(title="Producto Interno Bruto per-capita Millones",
                    x="Porcentaje",
                    y="Año")
grid.arrange(plot1,plot2)

# HDI Serie de tiempo
plot3 <- ggplot(mexico, aes(x=mexico$Año,y=mexico$HDI)) + 
  geom_point() + 
  labs(title="ïndice de Desarrollo Humano",
       x="Índice",
       y="Año")
plot4 <- ggplot(mexico, aes(x=mexico$Año,y=mexico$HDI, size=gdp_pc2)) +
  geom_point() + 
  labs(title="ïndice de Desarrollo Humano",
       x="Año",
       y="índice",
       caption="Los puntos increaumentan de tamaño en funcion al PIB per capita")
plot5 <- ggplot(mexico, aes(x=mexico$Año,y=mexico$gdp_pc2, size=HDI)) + 
  geom_point()  + 
  labs(title="ïndice de Desarrollo Humano",
       x="Año",
       y="PIB per-capita",
       caption="Los puntos increaumentan de tamaño en funcion al IDH")
grid.arrange(plot4,plot5)

ggplot(mexico, aes(x=mexico$HDI,y=mexico$gdp_pc2, size=suicides_no)) + 
  geom_point()  + 
  labs(title="",
       x="HDI",
       y="PIB per-capita",
       z="suicidios",
       caption="Los puntos increaumentan de tamaño en funcion al PIB per-capita")

# GRAFICO DE AUTOCORRELACION
datos4_85 <-mexico %>% filter(( mexico$HDI>0.1))
ggcorr(datos4_85[c(3,4,5,6,9,10)]) +
  labs(title="Correlación")

# filtrar valores del HDI mayores a .1
mexico2 <- mexico %>% filter(HDI>.1) 

# 1990
cplot2mx<-ggpairs(mexico2[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
  labs(title="1990")

# 2000
cplot1mx <-ggpairs(mexico2[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
  labs(title="2000")

# 2014
cplot2<-ggpairs(mexico2[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
  labs(title="2014")
dim(mexico)



# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE VI     ## # # # # # #  
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#############################################################
###############        USA   ################################
#############################################################
#############################################################
#Tarea 1 Países USA Ciencia de datos

# Filtrando para USA
USA <- datos %>% filter(ï..country =="United States") 
head(USA)
USA %>% count(generation) 
USA %>% count(sex) 
USA %>% dim
usa <- datos3 %>% filter(País =="United States") 

# POBLACIÓN USA
head(usa)
usa %>% dim

# poblción por edad
pie14USA <-USA %>% filter(year=="2014") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Año 2014", x="Población", y="")
pie14MUSA <-USA %>% filter(year=="2014" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres Año 2014", x="Población", y="")
pie14HUSA <-USA %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Hombres Año 2014", x="Población", y="")
pie94USA <-USA %>% filter(year=="1994") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Año 1994", x="Población", y="")
pie94MUSA <-USA %>% filter(year=="1994" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres Año 1994", x="Población", y="")
pie94HUSA <-USA %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) +  labs(title="Hombres Año 1994", x="Población", y="")
grid.arrange(pie14USA,pie94USA) 
grid.arrange(pie14MUSA,pie94MUSA)
grid.arrange(pie14HUSA,pie94HUSA)


# Base de datos agregada para Estados Unidos
usa <- datos4 %>% filter(País=="United States") 

#HDI repotado en USA 
# NOTA tener precaución con los ceros de HDI, ya que eran valores perdidos
usa[c(2,6)] %>% filter(HDI!=0) %>% formattable

# GRAFICOS TIPO SERIES DE TIEMPO
plot1usa <-ggplot(usa, aes(x=usa$Año,y=usa$suicidesRatio)) +
  geom_line() +
  labs(title="Tasa de suicidio por cada cien mil habitantes",
       x="Porcentaje",
       y="Año")
plot2usa <- ggplot(usa, aes(x=usa$Año,y=usa$gdp_pc2)) + 
  geom_line() + 
  labs(title="Producto Interno Bruto per-capita Millones",
       x="Porcentaje",
       y="Año")
grid.arrange(plot1usa,plot2usa)

# HDI Serie de tiempo
plot3usa <- usa %>% filter(HDI > 0) %>%  ggplot(aes(x=Año,y=HDI)) + 
  geom_point() + 
  labs(title="ïndice de Desarrollo Humano",
       x="Índice",
       y="Año")
plot4usa <-usa %>% filter(HDI > 0) %>%  ggplot( aes(x=Año,y=HDI, size=gdp_pc2)) +
  geom_point() + 
  labs(title="ïndice de Desarrollo Humano",
       x="Año",
       y="índice", z="PIB per-capita",
       caption="Los puntos increaumentan de tamaño en funcion al PIB per capita")

plot5usa <- usa %>% filter(HDI > 0) %>% ggplot(aes(x=Año,y=gdp_pc2, size=HDI)) + 
  geom_point()  + 
  labs(title="ïndice de Desarrollo Humano",
       x="Año",
       y="PIB per-capita",
       caption="Los puntos increaumentan de tamaño en funcion al IDH")
grid.arrange(plot4usa,plot5usa)
usa %>% filter(HDI > 0) %>% ggplot(aes(x=HDI,y=gdp_pc2, size=suicides_no)) + 
  geom_point()  + 
  labs(title="",
       x="HDI",
       y="PIB per-capita",
       z="suicidios",
       caption="Los puntos increaumentan de tamaño en funcion al PIB per-capita")

# Gráfico de autocorrelación
datos4_85 <-usa %>% filter(( usa$HDI>0.1))
ggcorr(datos4_85[c(3,4,5,6,9,10)]) +
  labs(title="Correlación")

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               FIN    ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
