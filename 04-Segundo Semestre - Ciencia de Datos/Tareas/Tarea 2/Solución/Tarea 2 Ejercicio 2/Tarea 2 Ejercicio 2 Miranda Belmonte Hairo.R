###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Preguta 2 
# Versión 1
#
###############################

######################
## Librerias
######################
library("readxl")
library("DataExplorer") # Reporte base de datos
library("knitr") # tablas data frame
library("kableExtra") # formato kable tablas
library("tidyverse") 
library("magrittr") # pipes
library("ggplot2") # gráficos
library("plotly") # gráficos dinámicos
library("gridExtra")
library("stratification") # estratificación

setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 2-20190215/Ejercicio 2")
getwd()

######################
## importando datos
######################
censoNL <- read_excel("baseNL2010.xlsx")
localidad <- read_excel("LocalidadEtiquetas.xlsx")

############################################
############################################
## Parte 1 Explorar base
############################################
############################################

localidad %>% dim # dimensió
censoNL %>% dim # dimensió


plot_intro(censoNL) # estructura de base
censoNL %>% names 
censoNL[censoNL<0]<-NA # valeres negativos a cero
plot_intro(censoNL) # estructura de base

censoNL %>% introduce %>% kable %>% kable_styling # tabla

Conapo <- left_join(localidad,censoNL, by="CVEGEO") # join labels localidad
base <-  Conapo %>% as.data.frame # como dataframe

base[base<0]<-NA # valores negativos a missin values
plot_intro(base) # estructura de base
base %>% introduce %>% kable %>% kable_styling # tabla
base <- base %>% drop_na # Ver con cuantas te quedas si decides tirar missing
# filtrar de esa manera solo nos deja 12 filas

# filtra por las variables que reporta CONAPO
base2 <-  Conapo %>% as.data.frame
base2[base2<0]<-NA # valores negativos a missin values
base2 %>% names 
# filtrado
base2 <- base2 %>% select(CVEGEO,  NOMBRE,
                 POB20,EDU28,EDU28_R,
                 EDU31,EDU34,EDU37,EDU43,
                 VIV2,VIV19,VIV20_R,
                 VIV15,VIV14,VIV15_R,
                 VIV17,VIV16,VIV17_R,VIV16_R,
                 VIV5_R,
                 VIV6,VIV2,VIV6_R,
                 VIV2,VIV26,VIV26_R)
base2 %>% plot_intro # estructura datos
base2 %>% introduce %>% kable %>% kable_styling # tabla
#base2 %>% dim 
base2 %>% drop_na %>% dim # Ver con cuantas te quedas si decides tirar missing
# existen muchos valores perdidos en  serie
base2 %>% plot_missing # porcentaje de missing values por variable

############################################
############################################
## Parte 2 Construcción de indicador
############################################
############################################

 # INDICADOR 1. % población 15 o más analfabeta
# filtro
indicador1 <- base2 %>% select(CVEGEO,  NOMBRE,
                               POB20,EDU28,EDU28_R)
# descriptivo
indicador1 %>% plot_intro 
indicador1 %>% drop_na %>% dim
indicador1 %>% plot_missing
# cálculo
indicador1 <- indicador1 %>% mutate(I1 = round((EDU28/POB20)*100,1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador1$I1 %>% is.na %>% sum # 0
#  le asignamos el valor mediana
#mediaI1 <- indicador1$I1 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI1 <- indicador1$I1 %>%  as.matrix %>% na.omit %>% median
indicador1 <- indicador1 %>% replace_na(list(I1=medianaI1))
indicador1 %>% dim 
indicador1 %>% plot_missing

###############################################################
# INDICADOR 2.  % población 15 o más sin primaria completa
# Le hacen falta variables a la base, descargamos de nuevo la base
# https://www.inegi.org.mx/programas/ccpv/2010/default.html

censoNLCompleto <- read_excel("censoCompletoNL.xlsx")
censoNLCompleto$CVEGEO <- censoNLCompleto$CVEGEO %>% as.numeric
censoNLCompleto <- left_join(localidad,censoNLCompleto, by="CVEGEO") 

censoNLCompleto %>% names
censoNLCompleto%>% introduce %>% kable %>% kable_styling
# censoNL tiene 5265 filas y 625 variables
# censoNLCompleto 5417 filas y 200 variables
baseCompleta <-  censoNLCompleto %>% as.data.frame
# valores negativos a missin values
baseCompleta[baseCompleta<0]<-NA
plot_intro(baseCompleta)
# de 680 y algo subió a 604196 valores perdidos
baseCompleta %>% introduce %>% kable %>% kable_styling
baseCompleta %>% names

# filtro
indicador2 <- baseCompleta %>% select(CVEGEO,entidad,  nom_ent,mun,nom_mun,loc,
                               nom_loc,longitud,latitud,altitud,
                               p15ym_se,p15pri_in,p15pri_co,
                               p15sec_in,p15sec_co,
                               p18ym_pb)
# descriptivo
indicador2 %>% class
indicador2 %>% names
indicador2 %>% plot_intro
indicador2 %>% drop_na %>% dim
indicador2 %>% plot_missing
# cálculo e imputación
indicador2[,10:16] <- sapply(indicador2[,10:16], as.numeric)
indicador2 <- indicador2 %>% 
  mutate(I2 = round((( p15ym_se +p15pri_in)/(p15ym_se+p15pri_in+p15pri_co+
                                      p15sec_in+p15sec_co+
                                      p18ym_pb))*100,1))


# le vamos a agregar el valor medio del índice sin contar los na
indicador2$I2 %>% is.na %>% sum # 1
#  le asignamos el valor medio
#mediaI2 <- indicador2$I2 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI2 <- indicador2$I2 %>%  as.matrix %>% na.omit %>% median
indicador2 <- indicador2 %>% replace_na(list(I2=medianaI2))
indicador2 %>% dim 
indicador2 %>% plot_missing

###############################################################
# INDICADOR 3. % viviendas particulares sin excusado

# filtro
indicador3 <-  base2 %>% select(CVEGEO,  NOMBRE,
                                VIV2,VIV19,VIV20_R)
# descriptivos
indicador3 %>% plot_intro
indicador3 %>% drop_na %>% dim
indicador3 %>% plot_missing
indicador3 <- indicador3 %>% mutate(I3 = round(((VIV2-VIV19)/(VIV2))*100,1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador3$I3 %>% is.na %>% sum # 154
# cálculo e imputación
mediaI3 <- indicador3$I3 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI3 <- indicador3$I3 %>%  as.matrix %>% na.omit %>% median
indicador3 <- indicador3 %>% replace_na(list(I3=medianaI3))
indicador3 %>% dim 
indicador3 %>% plot_missing
indicador3 %>% head(10) 
###############################################################
# INDICADOR 4. % viviendas particulares sin energía eléctrica

# filtro
indicador4 <-  base2 %>% select(CVEGEO,  NOMBRE,
                                         VIV15,VIV14,VIV15_R)
# descriptivos
indicador4 %>% plot_intro
indicador4 %>% drop_na %>% dim
indicador4 %>% plot_missing
# cálculo e imputación
indicador4 <- indicador4 %>% 
mutate(I4 = round(((VIV15)/(VIV15+VIV14)*100),1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador4$I4 %>% is.na %>% sum # 534
# le asignamos el valor mediana
#mediaI4 <- indicador4$I4 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI4 <- indicador4$I4 %>%  as.matrix %>% na.omit %>% median
indicador4 <- indicador4 %>% replace_na(list(I4=medianaI4))
indicador4 %>% dim 
indicador4 %>% plot_missing
# nota, la mediana era de cero
indicador4 %>% head


###############################################################33
# INDICADOR 5. % viviendas particulares sin agua entubada 

# filtro
indicador5 <-  base2 %>% select(CVEGEO, NOMBRE,
                                VIV17,VIV16,VIV17_R,VIV16_R)
# descriptivo
indicador5 %>% plot_intro
indicador5 %>% drop_na %>% dim
indicador5 %>% plot_missing
# cálculo e imputación
indicador5 <- indicador5 %>% 
  mutate(I5 = round(((VIV17/(VIV17+VIV16))*100),1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador5$I5 %>% is.na %>% sum # 614
#  le asignamos el valor mediana
mediaI5 <- indicador5$I5 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI5 <- indicador5$I5 %>%  as.matrix %>% na.omit %>% median
indicador5 <- indicador5 %>% replace_na(list(I5=medianaI5))
indicador5 %>% dim 
indicador5 %>% plot_missing
indicador5 %>% head

###############################################################33
# INDICADOR 6. promedio de ocupantes por cuarto

# filtro
indicador6 <-  base2 %>% select(CVEGEO,  NOMBRE,
                                VIV5_R)
# descriptivo
indicador6 %>% plot_intro
indicador6 %>% drop_na %>% dim
indicador6 %>% plot_missing
# cálculo e imputación
indicador6 <- indicador6 %>% 
mutate(I6 = VIV5_R)
# le vamos a agregar el valor medio del índice sin contar los na
indicador6$I6 %>% is.na %>% sum # 137
# le asignamos el valor medio
#mediaI6 <- indicador6$I6 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI6 <- indicador6$I6 %>%  as.matrix %>% na.omit %>% median
indicador6 <- indicador6 %>% replace_na(list(I6=medianaI6))
indicador6 %>% dim 
indicador6 %>% plot_missing
indicador6 %>% head
###############################################################33
# INDICADOR 7. % viviendas con piso de tierra 

# filtro
indicador7 <-  base2 %>% select(CVEGEO,  NOMBRE,
                                VIV6,VIV2,VIV6_R)
# descriptivo
indicador7 %>% plot_intro
indicador7 %>% drop_na %>% dim
indicador7 %>% plot_missing
# cálculo e imputación
indicador7 <- indicador7 %>% 
  mutate(I7 = round((VIV6/(VIV6+(VIV2-VIV6)))*100,1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador7$I7 %>% is.na %>% sum #611
#  le asignamos el valor medio
# mediaI7 <- indicador7$I7 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI7 <- indicador7$I7 %>%  as.matrix %>% na.omit %>% median
indicador7 <- indicador7 %>% replace_na(list(I7=medianaI7))
indicador7 %>% dim 
indicador7 %>% plot_missing
# mediana de cero
indicador7 %>% head

###############################################################
# INDICADOR 8. % personas que no disponen de refrigerador 
# filtro
indicador8 <-  base2 %>% select(CVEGEO,  NOMBRE,
                                VIV2,VIV26,VIV26_R)
# descriptivo
indicador8 %>% plot_intro
indicador8 %>% drop_na %>% dim
indicador8 %>% plot_missing
# cálculo e imputación
indicador8 <- indicador8 %>% 
  mutate(VIV26_R_menos_uno =100-VIV26_R )
indicador8 <- indicador8 %>% 
  mutate(I8 = round((VIV2-VIV26)/(VIV2)*100,1))
# le vamos a agregar el valor medio del índice sin contar los na
indicador8$I8 %>% is.na %>% sum #193
#  le asignamos el valor medio
# mediaI8 <- indicador8$I8 %>%  as.matrix %>% na.omit %>% colMeans()
medianaI8 <- indicador8$I8 %>%  as.matrix %>% na.omit %>% median
indicador8 <- indicador8 %>% replace_na(list(I8=medianaI8))
indicador8 %>% dim 
indicador8 %>% plot_missing
indicador8 %>% head



###############################################################
# iner join, índice debe ser númerico
indicador1$CVEGEO %>% class
indicador2$CVEGEO <- indicador2$CVEGEO %>% as.numeric
# Uniendo índices
indices <- inner_join(indicador1, indicador2, by="CVEGEO") 
indices <- indices %>% inner_join(indicador3, by="CVEGEO") 
indices <- indices %>% inner_join(indicador4, by="CVEGEO") 
indices <- indices %>% inner_join(indicador5, by="CVEGEO") 
indices <- indices %>% inner_join(indicador6, by="CVEGEO") 
indices <- indices %>% inner_join(indicador7, by="CVEGEO") 
indices <- indices %>% inner_join(indicador8, by="CVEGEO") 
indices %>% dim
indices %>% head
# Tomando variables de relevancia
indices <- indices %>% select(CVEGEO, NOMBRE.x, entidad, nom_ent , mun, nom_mun ,loc ,nom_loc,
                   longitud, latitud, altitud,
                   I1,I2,I3,I4,I5,I6,I7,I8)

indices %>% dim
indices %>% names
indices %>% head

###############################################################
###############################################################
############## PARTE 3 COMPONENTES PRINCIPALES ################
###############################################################
###############################################################

####### Parte 2.4 PCA en R ##############

#######  PCA scale X  ##############

X <- indices[,12:19] 
# descriptivos
resumen <- X %>% summary %>% as.data.frame 
resumen <- resumen %>% separate(Freq, into = c("label","freq"), sep = ":")
resumen <- resumen %>% spread(label, freq)
resumen <- resumen[,2:8]
colnames(resumen) <- c("Indicador","1er Qtl","3er Qtl",
                       "Max","Media","Mediana","Min")
resumen <- resumen[c(1,2,3,7,5,6,4)]
resumen %>% kable %>% kable_styling
# correlacipon
X %>% plot_correlation

# PCA
valores <- princomp(X, cor = T) # se utiliza matriz de cor
# importancia de los componentes
valores %>% summary 
Tabla1 <- data.frame(Componentes = 1:8,
           ValoresPropios =round(valores$sdev^2,2),
           DesviaciónEstándar=round(valores$sdev,2),
           PorcVar = round(valores$sdev^2/sum(valores$sdev^2),2),
           PorAcum =round(cumsum(valores$sdev^2)/sum(valores$sdev^2),2))

colnames(Tabla1) <- c("Componentes", "Valores Propios",
                  "Desviación Estándar",
                  "% Varianza","% Varianza Acum.")

Tabla1 <- Tabla1[,2:5] %>% rownames_to_column(var="Componentes")

Tabla1 %>% kable %>% kable_styling

# valores propios
eigenValues <- valores$sdev^2
# loadings
eigenVectors <- valores$loadings
eigenVectors %>% class
eigenVectors <- with(valores, unclass(loadings)) %>% as.data.frame
# TOMO EL PRIMER COMPONENTE
PC1 <- eigenVectors$Comp.1 %>% as.data.frame
rownames(PC1) <- c("I1","I2","I3","I4",
                    "I5","I6","I7","I8")

# Ponderado de lectura
ponderadorLectura <- as.matrix(eigenVectors[,1])*eigenValues[1] 
ponderadorLectura <- ponderadorLectura %>% as.data.frame
PC1 <- cbind(PC1, ponderadorLectura)
colnames(PC1) <- c("Pesos", "Ponderador de lectura")
PC1 %>% round(3) %>% 
  rownames_to_column(var ="Indicador socioeconómico") %>%
  kable %>% kable_styling

# índice de marginación (IMG)
IMG <- valores$scores  %>%  as.data.frame
IMG %>% dim
IMG <- IMG$Comp.1 %>%  as.data.frame
IMG <- cbind(indices$NOMBRE.x, IMG)
head(IMG, 100)
IMG %>% head
localidad2 <- 1:length(IMG[,1])
cbind(localidad2, IMG)

# gráfico IMG
IMGplot <- IMG %>% ggplot() + geom_point(aes(x=localidad2, 
                              y =sort(IMG[,2]))) +
  labs(title="Índice de Marginalización", 
       x="",
       y="Localidad") +
  geom_hline(yintercept=0, 
             color = "black", size=.3)

ggplotly(IMGplot)

#  tabla ordenada IMG
Tabla2 <- IMG %>% arrange(desc(.))
colnames(Tabla2) <- c("Localidades", "IM")
#Tabla2 %>% View

# IMG INDICE ESCALA
minimo <-min(IMG$.)
maximo <-max(IMG$.)
IME <- ((IMG$.-minimo)/(maximo-minimo))*100
Tabla3 <- cbind(localidad2, IMG,IME)
Tabla3 %>% head

# gráfico IMG porcentaje

IMGE_plot <- Tabla3 %>% ggplot() + geom_point(aes(x=localidad2, 
                                             y =sort(Tabla3[,4]))) +
  labs(title="Índice de Marginalización", 
       x="%",
       y="Localidad") 

ggplotly(IMGE_plot)

IMEOrdenado <- Tabla3[,4]



###############################################################
###############################################################
############## PARTE 4 Estratificación, grados ################
###############################################################
###############################################################

# estratificación IM porcentajes
Estratificacion <- strata.cumrootf(IMEOrdenado, CV=0.05, Ls=5)
# límites superiores
Estratificacion$bh
# frecuencia
Estratificacion$Nh
# intervalos
MuyBajo <- c(0,Estratificacion$bh[1])
Bajo <- c(Estratificacion$bh[1],Estratificacion$bh[2])
Medio <- c(round(Estratificacion$bh[2],0),
           round(Estratificacion$bh[3],0))
Alto <- c(round(Estratificacion$bh[3],0),
          round(Estratificacion$bh[4],0))
MuyAlto <- c(Estratificacion$bh[4],100)
# Tabla intervalos
TablaLimites <- rbind(MuyBajo,Bajo,Medio,Alto,MuyAlto)
TablaLimites <- cbind(TablaLimites, as.data.frame(Estratificacion$Nh))
TablaLimites <- TablaLimites[c(3,1,2)]
TablaLimites <- TablaLimites %>% rownames_to_column(var="Grado de Marginación")
colnames(TablaLimites) <- c("Grado de marginación",
                            "Número de localidades",
                            "Límite inferior",
                            "Límite superior")

TablaLimites %>% kable %>% kable_styling

###############################################################
###############################################################
############## PARTE 5 Tablas Anexo ###########################
###############################################################
###############################################################

# diccionario variables
uno <- data.frame(variable=c("Población de 15 años o más",
                      "Población de 15 años o más analfabeta"),
           abreviacion=c("EDU28","POB20"))

dos <- data.frame(variable=c("Población de 15 años o más sin escolaridad",
                             "Población de 15 años o más con primaria incompleta",
                             "Población de 15 años o más con primaria completa",
                             "Población de 15 años o más con secundaria incompleta",
                             "Población de 15 años o más con secundaria completa",
                             "Población de 18 años o más con educación pos-básica"),
                  abreviacion=c("p15ym_se",
                                "p15pri_in",
                                "p15pri_co",
                                "p15sec_in",
                                "p15sec_co",
                                "p18ym_pb"))

tres <-  data.frame(variable=c("Viviendas particulares habitadas totales",
                               "Viviendas particulares habitadas que disponen de sanitario"),
                    abreviacion=c("VIV2","VIV19"))

cuatro <-  data.frame(variable=c("Viviendas particulares habitadas que disponen de luz eléctrica",
                               "Viviendas particulares habitadas que no disponen de luz eléctrica"),
                    abreviacion=c("VIV15","VIV14"))

cinco <-  data.frame(variable=c("Viviendas particulares habitadas que disponen de agua entubada fuera de la vivienda",
                                 " Viviendas particulares habitadas que disponen de agua entubada dentro de la vivienda"),
                      abreviacion=c("VIV17","VIV16"))

seis <-  data.frame(variable=c("Promedio de ocupantes por cuarto en viviendas particulares habitadas"),
                     abreviacion=c("VIV5_R"))

siete <- data.frame(variable=c("Viviendas particulares habitadas con piso de tierra",
                               " Viviendas particulares habitadas con piso diferente de tierra"),
                    abreviacion=c("VIV6","VIV2-VIV6"))

ocho <- data.frame(variable=c("Viviendas particulares habitadas totales",
                               "Viviendas particulares habitadas que disponen de refrigerador"),
                    abreviacion=c("VIV2","VIV26"))



TABLAVARIABLES <-  rbind(uno,dos,tres,cuatro,cinco,seis,siete,ocho)
colnames(TABLAVARIABLES) <- c("Descripción", "Variable")
TABLAVARIABLES %>% kable  %>% kable_styling

###############################################################
###############################################################
############## PARTE 6 Contraste con IMG de CONAPO ############
###############################################################
###############################################################
localidad3 <- 1:dim(CONAPOindicadores)[1]
# importando base
CONAPOindicadores <- read_excel("ConapoContraste.xlsx")
CONAPOindicadores %>% dim 
CONAPOindicadores %>% names
CONAPOindicadores <- CONAPOindicadores %>% as.data.frame
CONAPOindicadores %>%  class
# gráfico IMG %
coni <- CONAPOindicadores %>% ggplot() + geom_point(aes(x=localidad3, 
                                             y =sort(IMGporciento),colour="Original")) +
  labs(title="Índice de Marginalización", 
       x="",
       y="Localidad") +
  geom_hline(yintercept=0, 
             color = "black", size=.3)

# gráfico IMG
CONAPOindicadores %>% ggplot() + geom_point(aes(x=localidad3, 
                                                y =sort(IMG))) +
  labs(title="Índice de Marginalización", 
       x="",
       y="Localidad") +
  geom_hline(yintercept=0, 
             color = "black", size=.3)

EstatificacióConapo <- strata.cumrootf(CONAPOindicadores$IMGporciento, CV=0.05, Ls=5)

# intervalo estratificación
fqconapo1<- sum(CONAPOindicadores$IMGporciento >= 0 &
      CONAPOindicadores$IMGporciento <= 12)
fqconapo2<- sum(CONAPOindicadores$IMGporciento > 12 &
      CONAPOindicadores$IMGporciento <= 20)
fqconapo3<- sum(CONAPOindicadores$IMGporciento > 20 &
      CONAPOindicadores$IMGporciento <= 31)
fqconapo4<- sum(CONAPOindicadores$IMGporciento > 31 &
      CONAPOindicadores$IMGporciento <= 48)
fqconapo5<- sum(CONAPOindicadores$IMGporciento > 48 &
      CONAPOindicadores$IMGporciento <= 100)

fqconapo <- rbind(fqconapo1,fqconapo2,fqconapo3,fqconapo4,fqconapo5) %>%  as.data.frame
fqconapo <- rownames_to_column(fqconapo)
fqconapo <- fqconapo[-1]

TablaTodos <- cbind(fqconapo,TablaLimites) 
TablaTodos <- TablaTodos[c(2,1,3,4,5)]


###############################################################
###############################################################
############## PARTE 7 contruir IMG con indicadores   #########
############## socioeconómicos construidos por CONAPO #########
###############################################################
###############################################################
CONAPOindicadores %>%  names
XX <- CONAPOindicadores[,c(4:11)]
VA <- princomp(XX, cor = T)
VI <- VA$scores
VI %>%  dim
VI <- VI %>%  as.data.frame
# gráfico IMG
VI %>% ggplot() + geom_point(aes(x=localidad3, y =sort(VI$Comp.1))) +
  labs(title="Índice de Marginalización", 
       x="",
       y="Localidad") +
  geom_hline(yintercept=0, 
             color = "black", size=.3)
# escalando a %
minimo <-min(VI$Comp.1)
maximo <-max(VI$Comp.1)
vi <- ((VI$Comp.1-minimo)/(maximo-minimo))*100

# tabla estratificación
fqconapoCompia1 <- sum(vi >= 0 & vi<= 12)
fqconapoCompia2 <- sum(vi > 12 & vi<= 20)
fqconapoCompia3 <- sum(vi> 20 & vi <= 31)
fqconapoCompia4 <- sum(vi > 31 & vi<= 48)
fqconapoCompia5 <- sum(vi> 48 & vi<= 100)

fqconapoCompia <- rbind(fqconapoCompia1,fqconapoCompia2,
                  fqconapoCompia3,fqconapoCompia4,
                  fqconapoCompia5) %>%  as.data.frame
fqconapoCompia <- rownames_to_column(fqconapoCompia)
fqconapoCompia <- fqconapoCompia[-1]

TablaTodos <- cbind(TablaTodos,fqconapoCompia) 
TablaTodos <- TablaTodos[c(1,2,3,6,4,5)]
colnames(TablaTodos) <- c("Grado de marginación ",
                          "Frecuencia Original",
                          "Frecuencia Propia",
                          "Frecuencia Replica",
                          "Límite inferior",
                          "Límite superior")

TablaTodos %>% kable %>% kable_styling

# gráfico IMG %
to <- cbind(localidad2, vi)
to %>% head
to <- to %>% as.data.frame
to %>% ggplot() + geom_point(aes(x=localidad2,  y =sort(vi))) +
  labs(title="Índice de Marginalización", 
       x="%",
       y="Localidad") 

# gráfico IMG %, original (conapo), replica y propio
coni + geom_point(aes(x=to$localidad2,  y =sort(to$vi),colour="Replica")) +
  geom_point(aes(x=Tabla3$localidad2, 
                 y =sort(Tabla3[,4]),colour="Propio"))

# resumen replica indicadores

resumenReplica <- XX %>% summary %>% as.data.frame 
resumenReplica <- resumenReplica %>% separate(Freq, into = c("label","freq"), sep = ":")
resumenReplica <- resumenReplica %>% spread(label, freq)
resumenReplica <- resumenReplica[,2:8]
colnames(resumenReplica) <- c("Indicador","1er Qtl","3er Qtl",
                       "Max","Media","Mediana","Min")
resumenReplica <- resumen[c(1,2,3,7,5,6,4)]
resumenReplica %>% kable %>% kable_styling

###############################################################
###############################################################
########################      FIN     #########################
###############################################################