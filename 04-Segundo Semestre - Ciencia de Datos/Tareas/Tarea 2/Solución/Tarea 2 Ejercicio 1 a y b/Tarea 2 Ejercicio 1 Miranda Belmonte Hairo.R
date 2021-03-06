###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Preguta 1 Ejercicio b
# Versi�n 1
#
###############################

# Librerias
library("DataExplorer") # Reporte base de datos
library("knitr") # tablas data frame
library("kableExtra") # formato kan�ble tablas
library("tidyverse") 
library("magrittr") # pipes
library("ggplot2") # gr�ficos
library("plotly") # gr�ficos din�micos
library("gridExtra")
getwd()
setwd("C:/Users/h_air/Desktop")
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 2-20190215")
# Importando datos
datos <- read.csv("ushealth.csv")
datos %>% class # clase

###############################
###### EXPLORACION DE LA BASE #
######### PARTE 1 #############
###############################

# Estructura de la base
plot_intro(datos)

# Estructura de la base (variables)
datos  %>% introduce %>% t %>% kable %>% kable_styling
plot_str(datos)

#Valores perdidos por variable
plot_missing(datos)

# Explorndo variables discretas
# Regiones 
datos$reg %>% levels %>% kable %>% kable_styling 
# divisiones
datos$div %>% levels %>% kable %>% kable_styling 
# Etiquetas
DivisionBase <- datos %>% select(reg,div, X) %>% as.data.frame
# Etiquetas con poblaci�n
DivisionBasePoblacion <- datos %>% select(reg,div, X, popu.1985) %>% as.data.frame

# Resumen   Explorndo variables discretas

# N�mero de observaciones por reg�n y divisi�n
plot_bar( datos %>% select(reg,div))
# regi�n
p1 <-ggplot(data = region) +
  aes(x = reg, fill = reg, color = reg, weight = n) +
  geom_bar() +
  labs(title = "N�mero de Obs. por regi�n",
       x = "Regi�n",
       y = "Observaciones") +
  theme_grey() +
  coord_flip()
# divisi�n
p2 <- ggplot(data = DivisionBase[, 1:2]) +
  aes(x = div, fill = reg, color = reg) +
  geom_bar() +
  labs(title = "N�mero de obs. por divisi�n",
       x = "Divisi�n",
       y = " Observaciones") +
  theme_grey() +
  coord_flip()
grid.arrange(p1, p2, nrow = 1)

# Poblaci�n por regi�n y divisi�n
plot_bar( datos[c(3,13,14)], with = "popu.1985")
# BOX PLOT pob Poblaci�n por regi�n y divisi�n
# regi�n
p3 <- ggplot(data = DivisionBasePoblacion) +
  aes(x = reg, y = popu.1985, fill = reg) +
  geom_boxplot() +
  labs(title = "Poblaci�n 1985 por regi�n",
       x = "Regi�n") +
  theme_grey()

# divisi�n
p4 <-ggplot(data = DivisionBasePoblacion) +
  aes(x = div, y = popu.1985, fill = reg) +
  geom_boxplot() +
  labs(title = "Poblaci�n 1985 por regi�n",
       x = "Divisi�n") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p3, p4, nrow = 1)

# Accidentes por regi�n y divisi�n
plot_bar( datos[c(3,13,4)], with = "acc")
# boxplot
# Muertes cardiacas por regi�n y divisi�n
plot_bar( datos[c(3,13,5)], with = "card")
# Muertes c�ncer por regi�n y divisi�n
plot_bar( datos[c(3,13,6)], with = "canc")
# Muertes pulmonia por regi�n y divisi�n
plot_bar( datos[c(3,13,7)], with = "pul")
# Muertes neumonia por regi�n y divisi�n
plot_bar( datos[c(3,13,8)], with = "pneu")
# Muertes diabetes por regi�n y divisi�n
plot_bar( datos[c(3,13,9)], with = "diab")
# Muertes h�gado por regi�n y divisi�n
plot_bar( datos[c(3,13,10)], with = "liv")

# nota: es l�gico que el sur se encuentre con un mayor 
# n�mero de muertes por diferentes causas, ya que el n�mero
# de ciudades muestradas de esa regi�n es mayo respecto
# a las demas.


# Gr�ficos box plot Muertes por �rea y divisi�n

# accidentes
p4 <- ggplot(data = datos) +
  aes(x = div, y = acc, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por accidentes",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# cardiacas
p5 <- ggplot(data = datos) +
  aes(x = div, y = card, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes cardiacas",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p4, p5, nrow = 1)

# c�ncer
p6 <- ggplot(data = datos) +
  aes(x = div, y = canc, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por c�ncer",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# pulmonar
p7 <- ggplot(data = datos) +
  aes(x = div, y = pul, fill = reg) +
  geom_boxplot() +
  labs(title = "Muerte pulmonar",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p6, p7, nrow = 1)


# neumonia
p8 <- ggplot(data = datos) +
  aes(x = div, y = pneu, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por neumonia",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# diabetes
p9 <- ggplot(data = datos) +
  aes(x = div, y = diab, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por diabetes",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p8, p9, nrow = 1)
# h�gado
ggplot(data = datos) +
  aes(x = div, y = liv, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por enfermedades del h�gado",
       x = "divisi�n",
       y = "N�mero") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Resumen   Explorndo variables continuas

plot_histogram(datos)
plot_qq(datos[c(3,4,5,6,7,8,9,10)])
# continuo
plot_correlation(na.omit(datos[c(3,4,5,6,7,8,9,10)]), 
                 maxcat = 5L,  type = "c")


# PCA  100%  varianza explicada  cr�terio
plot_prcomp(datos[c(4,5,6,7,8,9,10)],  variance_cap = 1)
# PCA  80%  varianza explicada  cr�terio
plot_prcomp(datos[c(4,5,6,7,8,9,10)],  variance_cap = .8)

# Resumen (OPCIONAL)
#Elabora resumen de la base de datos
resumen <- datos[c(4,5,6,7,8,9,10)] %>% as.data.frame
#create_report(resumen)


###############################
##### Componentes Principales #
######### PARTE 2 #############
###############################

####### Parte 2.1 PCA a Mano ##############

#######  PCA sin escalar  ##############

# Tomando base con variables continuas
base <- resumen
X <- base %>% as.matrix
X %>% dim # dimensi�n matriz

# Valores propios
valoresPropios <- X %>% cov %>% eigen
# varianza
Varianza <- X %>% cov %>% diag %>% as.data.frame
colnames(Varianza) <- c("varianza")
Varianza %>% kable %>%kable_styling
# Nota: variabilidad en las dos primeras variables

# Reportando Output funci�n eigen
tabla <- data.frame(std=sqrt(valoresPropios$values),
                    prop.var=valoresPropios$values/sum(valoresPropios$values),
                    pro.acum=cumsum(valoresPropios$values/sum(valoresPropios$values)),
                    valor.prop=valoresPropios$values)
rownames(tabla) <- paste("pc",1:nrow(tabla))
tabla %>% kable %>% kable_styling

# screeplot Desviaci�n est�ndar
screeplot <- tabla %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=std), color="blue") +
  geom_line(aes(x=c(1:7), y=std)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="desviaci�n estandar")
ggplotly(screeplot)
# Nota: seleccionar dos componentes principales de acuerdo al screeplot

# screeplot porcentaje varianza 
screeplotPropVar <- tabla %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=prop.var), color="blue") +
  geom_line(aes(x=c(1:7), y=prop.var)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="porp.varianza %")
ggplotly(screeplotPropVar)

# screeplot: varianza acumulada
screeplotVarCum <- tabla %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=pro.acum), color="blue") +
  geom_line(aes(x=c(1:7), y=pro.acum)) +
  labs(title="Varianza Acumulda", 
       x="componentes principales",
       y="pro.acumulada")
ggplotly(screeplotVarCum)

# cr�terio de la varianza proporcional
valPropios <- valoresPropios$values
sum(valPropios[1:2])/sum(valPropios)
# cr�terio de kaiser
kaiser <- valPropios > mean(valPropios) 
kaiser <- kaiser %>% as.data.frame
colnames(kaiser) <- c("Mayor a mean eigenvalue")
rownames(kaiser) <- paste("pc",1:nrow(kaiser))
kaiser %>% kable %>% kable_styling
# Nota: cr�terio de kaiser se sesga al tomar el valor medio
# datos muy gr�ndes en los primeros tres valores propios

# Componentes (loadings)
loadings <- valoresPropios$vectors %>% round(4) %>% as.data.frame
rownames(loadings) <- names(base)
colnames(loadings) <- paste("PC",1:ncol(loadings))
loadings %>% kable %>% kable_styling
loadings[,1:2] %>% kable %>% kable_styling

# Scores

loadingsM <- loadings %>% as.matrix 
loadingsM %>% dim
baseM <- base %>% as.matrix
baseM  %>% dim
scores <- baseM%*%t(loadingsM)
scores   %>% dim

####### Parte 2.2 PCA en R ##############

#######  PCA sin escalar  ##############

pais <- datos$X 
base <- base %>% cbind(pais)
base <- column_to_rownames(base, var="pais")
# sin escalar
valores <- princomp(base, cor = F)
# importancia de los componentes
summary(valores)
# valores propios
eigenValues <- valores$sdev^2
# loadings
eigenVectors <- valores$loadings
# scores
scores <- valores$scores
# biplot
options(repr.plot.width=8,repr.plot.height=8)
biplot(valores)


####### Parte 2.3 PCA a Mano ##############

#######  PCA scale X   ##############

# Tomando base con variables continuas
baseS <- resumen
XS <- baseS %>% as.matrix
XS %>% dim # dimensi�n matriz

# Valores propios
valoresPropiosS <- XS %>% cor %>% eigen

# Reportando Output funci�n eigen
tablaS <- data.frame(std=sqrt(valoresPropiosS$values),
                    prop.var=valoresPropiosS$values/sum(valoresPropiosS$values),
                    pro.acum=cumsum(valoresPropiosS$values/sum(valoresPropiosS$values)),
                    valor.prop=valoresPropiosS$values)
rownames(tablaS) <- paste("pc",1:nrow(tablaS))
tablaS %>% kable %>% kable_styling

# screeplot Desviaci�n est�ndar
screeplotS <- tablaS %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=std), color="blue") +
  geom_line(aes(x=c(1:7), y=std)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="desviaci�n estandar")
ggplotly(screeplotS)
# Nota: seleccionar dos componentes principales de acuerdo al screeplot



# screeplot porcentaje varianza 
screeplotPropVarS <- tablaS %>% round(4) %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=prop.var), color="blue") +
  geom_line(aes(x=c(1:7), y=prop.var)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="porp.varianza %")
ggplotly(screeplotPropVarS)

# screeplot: varianza acumulada
screeplotVarCumS <- tablaS %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=pro.acum), color="blue") +
  geom_line(aes(x=c(1:7), y=pro.acum)) +
  labs(title="Varianza Acumulda", 
       x="componentes principales",
       y="pro.acumulada")
ggplotly(screeplotVarCumS)

# cr�terio de la varianza proporcional
valPropiosS <- valoresPropiosS$values
sum(valPropiosS[1:2])/sum(valPropiosS)
sum(valPropiosS[1:3])/sum(valPropiosS)
#Nota: scale obs values keep 3 PCA

# cr�terio de kaiser
kaiserS <- valPropiosS > mean(valPropiosS) 
kaiserS <- kaiserS %>% as.data.frame
colnames(kaiserS) <- c("Mayor a mean eigenvalue")
rownames(kaiserS) <- paste("pc",1:nrow(kaiserS))
kaiserS %>% kable %>% kable_styling

# Componentes (loadings)
loadingsS <- valoresPropiosS$vectors %>% round(4) %>% as.data.frame
rownames(loadingsS) <- names(baseS)
colnames(loadingsS) <- paste("PC",1:ncol(loadingsS))
loadingsS %>% kable %>% kable_styling
loadingsS[,1:3] %>% kable %>% kable_styling

# Scores
loadingsMS <- loadingsS %>% as.matrix 
loadingsMS %>% dim
baseMS <- baseS %>% as.matrix
baseMS  %>% dim
scoresS <- baseMS%*%t(loadingsMS)
scoresS   %>% dim



####### Parte 2.4 PCA en R ##############

#######  PCA scale X  ##############
base2 <- base 
valoresEscalados <- princomp(base2, cor = T)
# importancia de los componentes
summary(valoresEscalados)
# valores propios
eigenValuesE <- valoresEscalados$sdev^2
# loadings
eigenVectorsE <- valoresEscalados$loadings
# scores
scoresE <- valoresEscalados$scores
# biplot
options(repr.plot.width=8,repr.plot.height=8)
biplot(valoresEscalados)
biplot(valoresEscalados, choices=c(4,5),cex=c(.5,1),pc.biplot=F)
cor(base2) 
###############################
######### FIN #################
###############################