###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Preguta 1 Ejercicio b
# Versión 1
#
###############################

# Librerias
library("DataExplorer") # Reporte base de datos
library("knitr") # tablas data frame
library("kableExtra") # formato kan¿ble tablas
library("tidyverse") 
library("magrittr") # pipes
library("ggplot2") # gráficos
library("plotly") # gráficos dinámicos
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
# Etiquetas con población
DivisionBasePoblacion <- datos %>% select(reg,div, X, popu.1985) %>% as.data.frame

# Resumen   Explorndo variables discretas

# Número de observaciones por regón y división
plot_bar( datos %>% select(reg,div))
# región
p1 <-ggplot(data = region) +
  aes(x = reg, fill = reg, color = reg, weight = n) +
  geom_bar() +
  labs(title = "Número de Obs. por región",
       x = "Región",
       y = "Observaciones") +
  theme_grey() +
  coord_flip()
# división
p2 <- ggplot(data = DivisionBase[, 1:2]) +
  aes(x = div, fill = reg, color = reg) +
  geom_bar() +
  labs(title = "Número de obs. por división",
       x = "División",
       y = " Observaciones") +
  theme_grey() +
  coord_flip()
grid.arrange(p1, p2, nrow = 1)

# Población por región y división
plot_bar( datos[c(3,13,14)], with = "popu.1985")
# BOX PLOT pob Población por región y división
# región
p3 <- ggplot(data = DivisionBasePoblacion) +
  aes(x = reg, y = popu.1985, fill = reg) +
  geom_boxplot() +
  labs(title = "Población 1985 por región",
       x = "Región") +
  theme_grey()

# división
p4 <-ggplot(data = DivisionBasePoblacion) +
  aes(x = div, y = popu.1985, fill = reg) +
  geom_boxplot() +
  labs(title = "Población 1985 por región",
       x = "División") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p3, p4, nrow = 1)

# Accidentes por región y división
plot_bar( datos[c(3,13,4)], with = "acc")
# boxplot
# Muertes cardiacas por región y división
plot_bar( datos[c(3,13,5)], with = "card")
# Muertes cáncer por región y división
plot_bar( datos[c(3,13,6)], with = "canc")
# Muertes pulmonia por región y división
plot_bar( datos[c(3,13,7)], with = "pul")
# Muertes neumonia por región y división
plot_bar( datos[c(3,13,8)], with = "pneu")
# Muertes diabetes por región y división
plot_bar( datos[c(3,13,9)], with = "diab")
# Muertes hígado por región y división
plot_bar( datos[c(3,13,10)], with = "liv")

# nota: es lógico que el sur se encuentre con un mayor 
# número de muertes por diferentes causas, ya que el número
# de ciudades muestradas de esa región es mayo respecto
# a las demas.


# Gráficos box plot Muertes por área y división

# accidentes
p4 <- ggplot(data = datos) +
  aes(x = div, y = acc, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por accidentes",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# cardiacas
p5 <- ggplot(data = datos) +
  aes(x = div, y = card, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes cardiacas",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p4, p5, nrow = 1)

# cáncer
p6 <- ggplot(data = datos) +
  aes(x = div, y = canc, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por cáncer",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# pulmonar
p7 <- ggplot(data = datos) +
  aes(x = div, y = pul, fill = reg) +
  geom_boxplot() +
  labs(title = "Muerte pulmonar",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p6, p7, nrow = 1)


# neumonia
p8 <- ggplot(data = datos) +
  aes(x = div, y = pneu, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por neumonia",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# diabetes
p9 <- ggplot(data = datos) +
  aes(x = div, y = diab, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por diabetes",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
grid.arrange(p8, p9, nrow = 1)
# hígado
ggplot(data = datos) +
  aes(x = div, y = liv, fill = reg) +
  geom_boxplot() +
  labs(title = "Muertes por enfermedades del hígado",
       x = "división",
       y = "Número") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Resumen   Explorndo variables continuas

plot_histogram(datos)
plot_qq(datos[c(3,4,5,6,7,8,9,10)])
# continuo
plot_correlation(na.omit(datos[c(3,4,5,6,7,8,9,10)]), 
                 maxcat = 5L,  type = "c")


# PCA  100%  varianza explicada  críterio
plot_prcomp(datos[c(4,5,6,7,8,9,10)],  variance_cap = 1)
# PCA  80%  varianza explicada  críterio
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
X %>% dim # dimensión matriz

# Valores propios
valoresPropios <- X %>% cov %>% eigen
# varianza
Varianza <- X %>% cov %>% diag %>% as.data.frame
colnames(Varianza) <- c("varianza")
Varianza %>% kable %>%kable_styling
# Nota: variabilidad en las dos primeras variables

# Reportando Output función eigen
tabla <- data.frame(std=sqrt(valoresPropios$values),
                    prop.var=valoresPropios$values/sum(valoresPropios$values),
                    pro.acum=cumsum(valoresPropios$values/sum(valoresPropios$values)),
                    valor.prop=valoresPropios$values)
rownames(tabla) <- paste("pc",1:nrow(tabla))
tabla %>% kable %>% kable_styling

# screeplot Desviación estándar
screeplot <- tabla %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=std), color="blue") +
  geom_line(aes(x=c(1:7), y=std)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="desviación estandar")
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

# críterio de la varianza proporcional
valPropios <- valoresPropios$values
sum(valPropios[1:2])/sum(valPropios)
# críterio de kaiser
kaiser <- valPropios > mean(valPropios) 
kaiser <- kaiser %>% as.data.frame
colnames(kaiser) <- c("Mayor a mean eigenvalue")
rownames(kaiser) <- paste("pc",1:nrow(kaiser))
kaiser %>% kable %>% kable_styling
# Nota: críterio de kaiser se sesga al tomar el valor medio
# datos muy grándes en los primeros tres valores propios

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
XS %>% dim # dimensión matriz

# Valores propios
valoresPropiosS <- XS %>% cor %>% eigen

# Reportando Output función eigen
tablaS <- data.frame(std=sqrt(valoresPropiosS$values),
                    prop.var=valoresPropiosS$values/sum(valoresPropiosS$values),
                    pro.acum=cumsum(valoresPropiosS$values/sum(valoresPropiosS$values)),
                    valor.prop=valoresPropiosS$values)
rownames(tablaS) <- paste("pc",1:nrow(tablaS))
tablaS %>% kable %>% kable_styling

# screeplot Desviación estándar
screeplotS <- tablaS %>% as.data.frame  %>% rownames_to_column(var="PC")  %>%
  ggplot() + geom_point(aes(x=PC, y=std), color="blue") +
  geom_line(aes(x=c(1:7), y=std)) +
  labs(title="Screeplot", 
       x="componentes principales",
       y="desviación estandar")
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

# críterio de la varianza proporcional
valPropiosS <- valoresPropiosS$values
sum(valPropiosS[1:2])/sum(valPropiosS)
sum(valPropiosS[1:3])/sum(valPropiosS)
#Nota: scale obs values keep 3 PCA

# críterio de kaiser
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