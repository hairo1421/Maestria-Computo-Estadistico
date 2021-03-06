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
# pdf "Tarea 1 Pa�ses Ciencia de datos".
#
# En la parte 5, se realiza lo que se observa en el archivo 
# pdf "Tarea 1 Pa�ses M�xico Ciencia de datos"
#
# En la parte 6, se realiza lo que se observa en el archivo 
# pdf "Tarea 1 Pa�ses USA Ciencia de datos"
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


# dimensi�n de la base
datos %>% dim

# Nombre de las variables
tabla1 <- datos %>% colnames %>%  as.matrix
NombreVariables <- c("Pa�s", "A�o", "Sexo", "Edad por Rangos", 
                     "N�mero de suicidios", "Poblaci�n", 
                     "Suicidios por cada 100k", "Pa�s_A�o",
                     "�ndice de desarrollo humano",
                     "GDP","GDP per capita","Generaci�n")  
tabla1 <- cbind(tabla1, NombreVariables) 
colnames(tabla1) <- c("Variable", "Descripci�n") 
tabla1 %>%  as.data.frame  %>%  formattable

# n�mero de observaciones
tabla2 <- datos %>% count(�..country) 
colnames(tabla2) <- c("Pa�ses","Frecuencia") 
tabla2 <- tabla2 %>% arrange(desc(Frecuencia))

# pa�ses y nombres
datos[,1] %>% factor %>% levels %>% length

# a�os, son 32, de 1985 al 2016
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

# n�mero de suicidios observaciones registradas en todos los a�os
datos[,5] %>% is.na %>% sum # cero valores perdidos
tabla3 <-aggregate(datos$suicides_no, by=list(datos$�..country),
          FUN=sum) 
colnames(tabla3) <- c("Pa�ses","N�mero de suicidios")
tabla3 %>%  formattable

# 15 pa�ses que han registrado el mayor crimen
top15 <- head(arrange(tabla3,desc(tabla3$`N�mero de suicidios`)),15)  %>%  formattable

# No se est� contrlando por a�os
p1 <-ggplot(algo)+geom_point(aes(x=algo$`N�mero de suicidios`,y=algo$Pa�ses), color=algo$`N�mero de suicidios`) +   labs(title = "P�ases con mayor n�mero de suicidio", 
            y="Pa�ses", x="# de Suicidios")

# Agregado de suicidios por pa�s y a�o
tabla4 <-aggregate(datos$suicides_no, by=list(datos$�..country, datos$year),
         FUN=sum) 
tabla4 <-tabla4 %>% arrange(desc(x))  
tabla4 %>%  formattable

# la informaci�n NA no es que tuvieran NA
# en la base, si no que no registraron ese a�os
# No est�s cuadrado el panel

# Filtro para los pa�ses con mayor n�mero de sucidio
# por a�o  
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

# Filtro para los pa�ses con mayor n�mero de sucidio
# por a�o  
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

# Gr�fico de burbujas, representa la tabla de arriba
tabla5 %>% spread(Group.2 , x) %>%  formattable
colnames(tabla5) <-c("Pais","A�o","Suicidios")
buble <- ggplot(tabla5, aes(x=A�o, y=Pais, size=Suicidios, color=Pais)) +
  geom_point(alpha=0.7) + labs(title="N�mero de suicidios")

# Poblaci�n
datos[,5] %>% is.na %>% sum 

# dominica tiene suicidio cero y Saint Kitts and Nevis Ponerl0
aggregate(datos$suicides_no, by=list(datos$�..country),
          FUN=sum) %>% filter(x==0)

# s� tienen poblaci�n
aggregate(datos$population, by=list(datos$�..country),
          FUN=sum) %>% filter(Group.1 =="Dominica" | Group.1 ==
                                "Saint Kitts and Nevis")
tabla5 <- aggregate(datos$suicides_no, by=list(datos$�..country, datos$year),
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
###########secci�n de panles###########################
#######################################################
#######################################################

# Se realiza el agregado de algunas variables
datos2 <- aggregate(datos$population, by=list(datos$�..country, datos$year),
                    FUN=sum)
head(datos2)
datos2 <- aggregate(datos$suicides_no, by=list(datos$�..country, datos$year),
                    FUN=sum) %>% cbind(datos2[,3])
colnames(datos2) <- c("Pa�s", "A�o",
                      "suicides_no","population")
datos2 <- mutate(datos2, suicidesRatio=suicides_no/population)
head(datos2)

# Transformar GDP Y GDP PERCAPITA A NUMERICO
# se realiza un left_join 
# para realizar match con los plots y solo tener
# un gdp para un a�o en cada pa�s
dim(distinct(datos, gdp_for_year....))
gdp <- datos[c(1,2,9,10,11)] 
colnames(gdp) <- c("Pa�s","A�o","HDI","GDP","GDP_pc")
dim(gdp)
duplicated(gdp)
c <-seq(1,dim(gdp)[1],12)
gdp <- gdp[c,]
z <- arrange(gdp,desc(-gdp$A�o))
head(z)

# Base con gdp y gdp percapita como n�merico
datos3 <-left_join(datos2, z, by=c("Pa�s","A�o"))
head(datos3)
options(scipen=999)
as.numeric(gsub("," ,"", as.character(datos3$GDP)))
datos3 <-mutate(datos3, gdp_pc2 = as.numeric(gsub("," ,"", as.character(datos3$GDP)))/population)
datos3 <-mutate(datos3, gdp2 = as.numeric(gsub("," ,"", as.character(datos3$GDP))))
head(datos3)

# relaci�n entre los indicadores de desarrollo social
# gdp y suicidios
ggplot(as.data.frame(na.omit(datos3)), aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title="", x="GDP per capita")                                                                                      

# limpiando valores perdidos
datos4 <- datos3
datos4$HDI[is.na(datos3$HDI)] <- 0
datos4$gdp2[is.na(datos4$gdp2)] <- 0

# Correlaci�n entre HDI y gdp2
correlacion <- as.data.frame(cor(datos4$gdp2,datos4$HDI)) 
colnames(correlacion) <- c("Correlaci�n")
correlacion %>%  formattable

# N�mero de NA como valores perdidos
length(datos3$HDI)-sum(is.na(datos3$HDI))
length(datos3$gdp_pc2)-sum(is.na(datos3$gdp_pc2))
dev.off()
head(datos4)

# plot por a�os HDI respecto GDP_PER-CAPITA
g05 <-datos4 %>% filter(datos4$A�o=="2005" & (datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title="HDI~GDP per-capita",
                               subtitle = "2005",
                               x="GDP per-capita")
g10<-datos4 %>% filter(datos4$A�o=="2010" & (datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title= "2010", x="GDP per-capita")
g14<-datos4 %>% filter(datos4$A�o=="2014" & ( datos4$HDI!=0)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI) )+
  geom_point(alpha=0.7) + labs(title= "2014", x="GDP per-capita")
grid.arrange(g05,g10, g14)

# plot por a�os HDI respecto GDP_PER-CAPITA contemplando suicidios
g11<-datos4 %>% filter(datos4$A�o=="2005" & ( datos4$HDI>0.1)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2005")
g12<-datos4 %>% filter(datos4$A�o=="2010" & ( datos4$HDI>0.1)) %>%
  ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2010")
g13 <- datos4 %>% filter(datos4$A�o=="2014" & ( datos4$HDI>0.1)) %>%
ggplot(aes(x=gdp_pc2, y=HDI, size= suicidesRatio)) +
  geom_point(alpha=0.7) + labs(title="2014")
grid.arrange(g11,g12, g13)

# Un gr�fico de correlaci�n sin diferenciar a�os
head(datos4 %>% filter(datos4$HDI > 0) %>%arrange(desc(-HDI)),15)
head(datos4)
ggcorr(datos4[c(3,4,5,6,9,10)])

# con filtro de a�os
datos4_14 <-datos4 %>% filter(datos4$A�o=="2014" & ( datos4$HDI>0.1))
cor1 <-   ggcorr(datos4_14[c(3,4,5,6,9,10)])  + labs(title="2014")
datos4_10 <-datos4 %>% filter(datos4$A�o=="2010" & ( datos4$HDI>0.1))
cor2 <-   ggcorr(datos4_10[c(3,4,5,6,9,10)])  + labs(title="2010")
grid.arrange(cor1,cor2)
ggpairs(df,  diag = list(continuous = "naDiag")) 

# 2010
cplot1 <- ggpairs(datos4_10[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
  labs(title="2010")
# 2014
cplot2<-ggpairs(datos4_14[c(3,4,5,6,9,10)],  diag = list(continuous = "naDiag")) +
labs(title="2014")

# toma todos los a�os por factor tiempo
datos4 %>%
  ggplot()+ aes(y=datos4$suicides_no, x=datos4$Pa�s, group=Pa�s) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  datos4 %>%
  ggplot()+ aes(y=datos4$suicidesRatio, x=datos4$Pa�s, group=Pa�s) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               PARTE IV     ## # # # # # #  
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

#####################################################
###############EJERCICIO DE REGRESI�N#################
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
fixed.dum <-lm(datos3$gdp_pc2~ datos3$HDI +datos3$suicidesRatio+ factor(datos3$Pa�s) - 1, data=datos3)
fixed.dum %>% summ(confint = TRUE, digits = 3)

# Efectos fijos
fixed <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("Pa�s", "A�o"), model="within")
fixed %>% summary
fixef(fixed)

pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed
# no rechazo la nula, entonces es mejor ols que fija

# EFECTOS ALEATORIOS
ran <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("Pa�s", "A�o"), model="random")
ran %>% summary

# Prueba F es signficativa lo cual nos indica un buen modelo
phtest(fixed, ran)
# los datos sugieren utilizar efectos aleatorios
# Ho errores unicos correlacionadas con la regresara vs
# no lo est�n <.05 se rechaza la nula y utilizo random

# REGRESION TIPO  POOLED
pool <- plm(datos3$gdp_pc2~ datos3$HDI+datos3$suicidesRatio, data=datos3, index=c("Pa�s", "A�o"), model="pooling")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))
# no es necesario el panel

# AUTOCORRELACI�N SERIAL
pbgtest(fixed)
pbgtest(ran) # no correlaci�n
pbgtest(pool) # no correlaci�n

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
# Tarea 1 Pa�ses MEXICO Ciencia de datos

# Filtrando para M�xico
MEXICO <- datos %>% filter(�..country =="Mexico") 
head(MEXICO)
MEXICO %>% count(generation) 
MEXICO %>% count(sex) 
MEXICO %>% dim
mexico <- datos3 %>% filter(Pa�s =="Mexico") 

# POBLACI�N M�XICANA
head(mexico)

# Poblci�n por edad
pie14 <-MEXICO %>% filter(year=="2014") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="A�o 2014", x="Poblaci�n", y="")
pie14M <-MEXICO %>% filter(year=="2014" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres A�o 2014", x="Poblaci�n", y="")
pie14H <-MEXICO %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Hombres A�o 2014", x="Poblaci�n", y="")
pie94 <-MEXICO %>% filter(year=="1994") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="A�o 1994", x="Poblaci�n", y="")
pie94M <-MEXICO %>% filter(year=="1994" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres A�o 1994", x="Poblaci�n", y="")
pie94H <-MEXICO %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) 
grid.arrange(pie14,pie94) 
grid.arrange(pie14M,pie94M)
grid.arrange(pie14H,pie94H)

# HDI repotado en M�xico
na.omit(mexico[c(2,6)]) %>% formattable

# GRAFICS TIPO SERIE DE TIEMPO
plot1 <-ggplot(mexico, aes(x=mexico$A�o,y=mexico$suicidesRatio)) +
  geom_line() +
  labs(title="Tasa de suicidio por cada cien mil habitantes",
       x="Porcentaje",
       y="A�o")
plot2 <- ggplot(mexico, aes(x=mexico$A�o,y=mexico$gdp_pc2)) + 
  geom_line() + 
  labs(title="Producto Interno Bruto per-capita Millones",
                    x="Porcentaje",
                    y="A�o")
grid.arrange(plot1,plot2)

# HDI Serie de tiempo
plot3 <- ggplot(mexico, aes(x=mexico$A�o,y=mexico$HDI)) + 
  geom_point() + 
  labs(title="�ndice de Desarrollo Humano",
       x="�ndice",
       y="A�o")
plot4 <- ggplot(mexico, aes(x=mexico$A�o,y=mexico$HDI, size=gdp_pc2)) +
  geom_point() + 
  labs(title="�ndice de Desarrollo Humano",
       x="A�o",
       y="�ndice",
       caption="Los puntos increaumentan de tama�o en funcion al PIB per capita")
plot5 <- ggplot(mexico, aes(x=mexico$A�o,y=mexico$gdp_pc2, size=HDI)) + 
  geom_point()  + 
  labs(title="�ndice de Desarrollo Humano",
       x="A�o",
       y="PIB per-capita",
       caption="Los puntos increaumentan de tama�o en funcion al IDH")
grid.arrange(plot4,plot5)

ggplot(mexico, aes(x=mexico$HDI,y=mexico$gdp_pc2, size=suicides_no)) + 
  geom_point()  + 
  labs(title="",
       x="HDI",
       y="PIB per-capita",
       z="suicidios",
       caption="Los puntos increaumentan de tama�o en funcion al PIB per-capita")

# GRAFICO DE AUTOCORRELACION
datos4_85 <-mexico %>% filter(( mexico$HDI>0.1))
ggcorr(datos4_85[c(3,4,5,6,9,10)]) +
  labs(title="Correlaci�n")

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
#Tarea 1 Pa�ses USA Ciencia de datos

# Filtrando para USA
USA <- datos %>% filter(�..country =="United States") 
head(USA)
USA %>% count(generation) 
USA %>% count(sex) 
USA %>% dim
usa <- datos3 %>% filter(Pa�s =="United States") 

# POBLACI�N USA
head(usa)
usa %>% dim

# poblci�n por edad
pie14USA <-USA %>% filter(year=="2014") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="A�o 2014", x="Poblaci�n", y="")
pie14MUSA <-USA %>% filter(year=="2014" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres A�o 2014", x="Poblaci�n", y="")
pie14HUSA <-USA %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Hombres A�o 2014", x="Poblaci�n", y="")
pie94USA <-USA %>% filter(year=="1994") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="A�o 1994", x="Poblaci�n", y="")
pie94MUSA <-USA %>% filter(year=="1994" & sex=="female") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) + labs(title="Mujeres A�o 1994", x="Poblaci�n", y="")
pie94HUSA <-USA %>% filter(year=="2014" & sex=="male") %>% ggplot(aes(x="", y=population, fill=age))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + scale_fill_grey() + 
  theme(axis.text.x=element_blank()) +  labs(title="Hombres A�o 1994", x="Poblaci�n", y="")
grid.arrange(pie14USA,pie94USA) 
grid.arrange(pie14MUSA,pie94MUSA)
grid.arrange(pie14HUSA,pie94HUSA)


# Base de datos agregada para Estados Unidos
usa <- datos4 %>% filter(Pa�s=="United States") 

#HDI repotado en USA 
# NOTA tener precauci�n con los ceros de HDI, ya que eran valores perdidos
usa[c(2,6)] %>% filter(HDI!=0) %>% formattable

# GRAFICOS TIPO SERIES DE TIEMPO
plot1usa <-ggplot(usa, aes(x=usa$A�o,y=usa$suicidesRatio)) +
  geom_line() +
  labs(title="Tasa de suicidio por cada cien mil habitantes",
       x="Porcentaje",
       y="A�o")
plot2usa <- ggplot(usa, aes(x=usa$A�o,y=usa$gdp_pc2)) + 
  geom_line() + 
  labs(title="Producto Interno Bruto per-capita Millones",
       x="Porcentaje",
       y="A�o")
grid.arrange(plot1usa,plot2usa)

# HDI Serie de tiempo
plot3usa <- usa %>% filter(HDI > 0) %>%  ggplot(aes(x=A�o,y=HDI)) + 
  geom_point() + 
  labs(title="�ndice de Desarrollo Humano",
       x="�ndice",
       y="A�o")
plot4usa <-usa %>% filter(HDI > 0) %>%  ggplot( aes(x=A�o,y=HDI, size=gdp_pc2)) +
  geom_point() + 
  labs(title="�ndice de Desarrollo Humano",
       x="A�o",
       y="�ndice", z="PIB per-capita",
       caption="Los puntos increaumentan de tama�o en funcion al PIB per capita")

plot5usa <- usa %>% filter(HDI > 0) %>% ggplot(aes(x=A�o,y=gdp_pc2, size=HDI)) + 
  geom_point()  + 
  labs(title="�ndice de Desarrollo Humano",
       x="A�o",
       y="PIB per-capita",
       caption="Los puntos increaumentan de tama�o en funcion al IDH")
grid.arrange(plot4usa,plot5usa)
usa %>% filter(HDI > 0) %>% ggplot(aes(x=HDI,y=gdp_pc2, size=suicides_no)) + 
  geom_point()  + 
  labs(title="",
       x="HDI",
       y="PIB per-capita",
       z="suicidios",
       caption="Los puntos increaumentan de tama�o en funcion al PIB per-capita")

# Gr�fico de autocorrelaci�n
datos4_85 <-usa %>% filter(( usa$HDI>0.1))
ggcorr(datos4_85[c(3,4,5,6,9,10)]) +
  labs(title="Correlaci�n")

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #               FIN    ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
