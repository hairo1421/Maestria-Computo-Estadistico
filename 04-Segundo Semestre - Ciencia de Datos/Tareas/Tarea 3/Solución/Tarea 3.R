###############################
#
# Ciencia de Datos
# Miranda Belmonte Hairo Ulises
# Tarea 2 Ejercicio 2
# Ejercicio Extra
# Versión 1
#
###############################

######################
## Librerias
######################
library("tidyverse") # manipulación de datos 
library("magrittr") # pipes
library("readxl") # lectura xlsx
library("DataExplorer") # Reporte base de datos
library("knitr") # tablas data frame
library("kableExtra") # formato kable tablas
library("gridExtra") # ggplot join plots
library("cluster") # fuzzy k means
library("ggdendro") # dendrogramas
library("dendextend") # dendrogramas
library("factoextra") # dendrogramas
library("corrplot") # correlación dendrogramas

#######################
## PARTE I Ejercicio A
#######################

###################
# exportar datos
###################
getwd()
setwd("C:/Users/h_air/Desktop/CIMAT MCE/Semestre_2/Ciencia de Datos/Tareas/Tarea 3")
base <- read.csv("gene_expression_2classes.csv")
# Descripción de la base 
base %>%  create_report
base %>% plot_intro
base %>% introduce %>% kable %>%  kable_styling

#############################
# Paso 1. escalar los datos
#############################
X <- base %>%  t %>% scale 

##########################################################
# Paso 2. cluste jerarquico
 # Distancia Euclidiana
   # no se consideran X como matriz de disimilaridad.
##########################################################

aglomerativo_E_AL <- X %>% agnes(metric = "euclidean", diss=F, stand = F, method = "average")
aglomerativo_E_SL <- X %>% agnes(metric = "euclidean", diss=F, stand = F, method = "single")
aglomerativo_E_CL <- X %>% agnes(metric = "euclidean", diss=F, stand = F, method = "complete")
divisible_E <- X %>% diana(diss=FALSE,metric="euclidean")
dend1 <- as.dendrogram(aglomerativo_E_AL,hang=-1)
dend2 <- as.dendrogram(aglomerativo_E_SL,hang=-1)
dend3 <- as.dendrogram(aglomerativo_E_CL,hang=-1)
dend4 <- as.dendrogram(divisible_E,hang=-1)

par(mfrow=c(1,2))

# Asignar color a etiquetas
uno <- rep(1,20) %>% as.data.frame
dos <- rep(2,20) %>% as.data.frame
indicador <- rbind(uno,dos)
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.

# Enlace promedio
cc <- dend1
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  
ggd1 <- as.ggdend(cc)
# gráfico
a <- ggplot(ggd1, horiz = TRUE, theme = NULL) + ggtitle("Average") +
  ylab("Altura") + xlab("muestras tejidos")

# Enace singular
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_E_SL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  
ggd1 <- as.ggdend(cc)
# gráfico
b <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Single") +
  ylab("Altura") + xlab("muestras tejidos")

# join gráfico
grid.arrange(a, b, nrow = 1)

# Enlace complete
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_E_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))   
ggd1 <- as.ggdend(cc)
c2 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")+
ylab("Altura") + xlab("muestras tejidos")

# enlace divisible
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_E,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))   
ggd1 <- as.ggdend(cc)
# gráfico
d <- ggplot(ggd1, horiz = TRUE, theme = NULL) + ggtitle("Divisible") +
ylab("Altura") + xlab("muestras tejidos")

# join gráfico
grid.arrange(c2, d, nrow = 1)



##########################################################
# Paso 3. cluste jerarquico
# Correlación a distancia.
##########################################################
temp <- sqrt(1-cor(t(X))) # matriz  similaridad  correlación sobre variables

aglomerativo_C_AL <- temp %>% agnes(diss=T, stand = F, method = "average")
aglomerativo_C_SL <- temp %>% agnes(diss=T, stand = F, method = "single")
aglomerativo_C_CL <- temp %>% agnes(diss=T, stand = F, method = "complete")
divisible_C <- temp %>% diana(diss=T)

dend1c <- as.dendrogram(aglomerativo_C_AL,hang=-1)
dend2c <- as.dendrogram(aglomerativo_C_SL,hang=-1)
dend3c <- as.dendrogram(aglomerativo_C_CL,hang=-1)
dend4c <- as.dendrogram(divisible_C,hang=-1)

par(mfrow=c(1,2))

# Asignando colores a labels
uno <- rep(1,20) %>% as.data.frame
dos <- rep(2,20) %>% as.data.frame
indicador <- rbind(uno,dos)
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.

# enlace con promedio
cols <- c$.[order.dendrogram(dend1c)]
labels_colors(dend1c) <- as.numeric(cols)
dend1c  <- cc %>% set("labels_cex", c(.7,.7))  
# gráfico
ggd1 <- as.ggdend(dend1c)
a_c <- ggplot(ggd1, horiz = TRUE, theme = NULL) + ggtitle("Average")+
  ylab("Altura") + xlab("muestras tejidos")

# enlace single
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_C_SL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  
ggd1 <- as.ggdend(cc)
# gráfico
b_c <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Single")+
  ylab("Altura") + xlab("muestras tejidos")
grid.arrange(a_c, b_c, nrow = 1)

# enlace complete
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_C_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))   
ggd1 <- as.ggdend(cc)

# gráfico
c2_c <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")+
  ylab("Altura") + xlab("muestras tejidos")

# enlace divisible
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_C,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))   
ggd1 <- as.ggdend(cc)

# gráfico
d_c <- ggplot(ggd1, horiz = TRUE, theme = NULL) + ggtitle("Divisible")+
  ylab("Altura") + xlab("muestras tejidos")

# join gráfico 
grid.arrange(c2_c, d_c, nrow = 1)



##########################################################
# Paso 4. me quedo con dos dendrogramas completo y
# divisible con distancia euclidiana y correlación
#
##########################################################


# completo euclidiano
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_E_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
c2 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3))["2"]
# gráfico
C2 <- c2 + geom_hline(yintercept = 48.0983, size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")

# divisible euclidiano
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_E,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
d <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Divisible")
heights_per_k.dendrogram(as.dendrogram(dend4))["2"]
# gráfico
d2 <- d + geom_hline(yintercept = 48.09794 , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")
# joint gráfico
grid.arrange(C2, d2, nrow = 1)

# completo correlación 
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_C_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
c2_c <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3c))["2"]
# gráfico
C2_c <- c2_c + geom_hline(yintercept = 1.083052 , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")

# divisible correlación
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_C,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
dc <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend4c))["2"]
# gráfico
d2c <- dc + geom_hline(yintercept = 1.083044  , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")
# joint gráfico
grid.arrange(C2_c , d2c, nrow = 1)


############################################################
# Paso 4. reduciendo disimilaridad con dendrogramas previos
#
############################################################

# Se asumen 3 grupos 

# completo euclidiano
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_E_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 3, value = c("purple", "green", "orange")) 
ggd1 <- as.ggdend(cc)
#  gráfico
c2_k3 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3))["3"]
C2_k3 <- c2_k3 + geom_hline(yintercept = 46.58713 , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")

# divisible euclidiano
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_E,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 3, value = c("purple", "green", "orange")) 
ggd1 <- as.ggdend(cc)
# gráfico
d_k3 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Divisible")
heights_per_k.dendrogram(as.dendrogram(dend4))["3"]
d2_k3 <- d_k3 + geom_hline(yintercept = 46.58677 , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")
# joint gráfica
grid.arrange(C2_k3, d2_k3, nrow = 1)

# completo correlación 
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_C_CL,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 3, value = c("purple", "green", "orange")) 
ggd1 <- as.ggdend(cc)
c2_ck3 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3c))["3"]
C2_ck3 <- c2_ck3 + geom_hline(yintercept = 1.040954  , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")

# divisible correlación
class(indicador)
c <- cbind(indicador,X)
colors.dend <- c$.
cc <- as.dendrogram(divisible_C,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 3, value = c("purple", "green","orange")) 
ggd1 <- as.ggdend(cc)
dc_k3 <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Divisible")
heights_per_k.dendrogram(as.dendrogram(dend4c))["3"]
d2c_k3 <- dc_k3 + geom_hline(yintercept = 1.040946   , size=1, linetype = 2 , color="blue")+
  ylab("Altura") + xlab("muestras tejidos")
# joint gráfica
grid.arrange(C2_ck3 , d2c_k3, nrow = 1)





#################################################################
# Paso 5. Tablas de frecuencia en asignación de obs. a clusters
#
#################################################################

fit1_e <- cutree(dend3, k = 3) # complete
fit2_e <- cutree(dend4, k = 3) # divisible
fit3_e <- cutree(dend1, k = 3) # average
fit4_e <- cutree(dend2, k = 3) # single
fit1_e %>% table
fit2_e %>% table
fit3_e %>% table
fit4_e %>% table


completo <- fit1_e %>% table
divisible <- fit2_e %>% table
promedio <- fit3_e %>% table
single <- fit4_e %>% table
tabla2 <- cbind(completo, divisible, promedio, single) %>% as.data.frame
row.names(tabla2) <- c("k=1", "k=2", "k=3")
tabla2 <- rownames_to_column(tabla2)
colnames(tabla2) <- c("K", "Complete", "Divisible", "Average", "Single")
tabla2 %>% kable %>% kable_styling



fit1 <- cutree(dend3c, k = 3) # complete
fit2 <- cutree(dend4c, k = 3) # divisible
fit3 <- cutree(dend1c, k = 3) # average
fit4 <- cutree(dend2c, k = 3) # single
fit1 %>% table
fit2 %>% table
fit3 %>% table
fit4 %>% table

Completo <- fit1 %>% table
Divisible <- fit2 %>% table
Promedio <- fit3 %>% table
Single <- fit4 %>% table
tabla1 <- cbind(Completo, Divisible, Promedio, Single) %>% as.data.frame
row.names(tabla1) <- c("k=1", "k=2","k=3")
tabla1 <- rownames_to_column(tabla1)
colnames(tabla1) <- c("K", "Complete", "Divisible", "Average", "Single")
tabla1 %>% kable %>% kable_styling


# podemos ver que con los datos escalados y utilizando 
# distancia en correlación, la matriz de similaridad
# es la misma que la euclidiana


############################################################
# Paso 6. Correlación dendrogramas
############################################################

# correlación entre los modelos jerarquicos
dend_list_e <- dendextend::dendlist("Complete" = as.dendrogram(dend3), "Single" = as.dendrogram(dend2),
                      "Average" = as.dendrogram(dend1), "Divisible" = as.dendrogram(dend4))

cors_e <- cor.dendlist(dend_list_e)
round(cors_e, 2)


dend_list_c <- dendextend::dendlist("Complete" = as.dendrogram(dend3c), "Single" = as.dendrogram(dend2c),
                                    "Average" = as.dendrogram(dend1c), "Divisible" = as.dendrogram(dend4c))

cors_c <- cor.dendlist(dend_list_c)
round(cors_c, 2)



par(mfrow=c(1,2))
corrplot(cors_e, "pie", "lower")
corrplot(cors_c, "pie", "lower")

# conclusión, no se pueden identificar con cluster jerarquico



############################################################
# Paso 7. Métodos no-jerárquicos
#
############################################################

# Elección del número de clusters

# k-mean
set.seed(123) # semilla

fviz_nbclust(X, kmeans, method = "wss") # suma de errores entre grupos
fviz_nbclust(X, kmeans, method = "silhouette") # promedio de valores silhouette

# k-medoides
set.seed(123) # semilla
fviz_nbclust(X, pam, method = "wss")
fviz_nbclust(X, pam, method = "silhouette")

# fuzzy k-means

set.seed(123) # semilla
fviz_nbclust(X, fanny, method = "wss")
fviz_nbclust(X, fanny, method = "silhouette")

# Gráficos PCA
componentes <- prcomp(X)
valoresPropios <- componentes$sdev^2
porpAcum <- valoresPropios[1:30]/sum(valoresPropios)
PVEplot <- qplot(c(1:30), porpAcum) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, .1)

# PCA kmeans
set.seed(123)
final <- kmeans(X, 2, nstart = 20)
fviz_cluster(final, data = X, stand = F)
# PCA k-medoides
set.seed(123)
final2 <- pam(X, 2)
fviz_cluster(final2, data = X)
# PCA y fuzzy k-means
set.seed(123)
final3 <- fanny(X, 2)
fviz_cluster(final3, data = X)


############################################################
# Paso 7. Métodos PCA y cluesters jerárquicos
#
############################################################

# completo euclidiano
componentes <- prcomp(X)
scores <- componentes$x[,1:2]
aglomerativo_E_AL_PCA <- scores %>% agnes(metric = "euclidean", diss=F, stand = F, method = "average")
aglomerativo_E_SL_PCA <- scores %>% agnes(metric = "euclidean", diss=F, stand = F, method = "single")
aglomerativo_E_CL_PCA <- scores %>% agnes(metric = "euclidean", diss=F, stand = F, method = "complete")
divisible_E_PCA <- scores %>% diana(diss=FALSE,metric="euclidean")
dend1 <- as.dendrogram(aglomerativo_E_AL_PCA,hang=-1)
dend2 <- as.dendrogram(aglomerativo_E_SL_PCA,hang=-1)
dend3 <- as.dendrogram(aglomerativo_E_CL_PCA,hang=-1)
dend4 <- as.dendrogram(divisible_E_PCA,hang=-1)

class(indicador)
c <- cbind(indicador,scores)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_E_CL_PCA,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
PCA_E_CL <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3))["2"]
PCA_E_CL_2 <- PCA_E_CL + geom_hline(yintercept = 28.49178, size=1, linetype = 2 , color="blue")

# divisible euclidiano

class(indicador)
c <- cbind(indicador,scores)
colors.dend <- c$.
cc <- as.dendrogram(divisible_E_PCA,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
d_PCA <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Divisible")
heights_per_k.dendrogram(as.dendrogram(dend4))["2"]
d2_PCA <- d_PCA + geom_hline(yintercept = 28.49178 , size=1, linetype = 2 , color="blue")

grid.arrange(PCA_E_CL_2, d2_PCA, nrow = 1)

# completo correlación 
temp <- sqrt(1-cor(t(scores))) # matriz  similaridad  correlación sobre variables

aglomerativo_C_AL_PCA <- temp %>% agnes(diss=T, stand = F, method = "average")
aglomerativo_C_SL_PCA <- temp %>% agnes(diss=T, stand = F, method = "single")
aglomerativo_C_CL_PCA <- temp %>% agnes(diss=T, stand = F, method = "complete")
divisible_C_PCA <- temp %>% diana(diss=T)

dend1c <- as.dendrogram(aglomerativo_C_AL_PCA,hang=-1)
dend2c <- as.dendrogram(aglomerativo_C_SL_PCA,hang=-1)
dend3c <- as.dendrogram(aglomerativo_C_CL_PCA,hang=-1)
dend4c <- as.dendrogram(divisible_C_PCA,hang=-1)

class(indicador)
c <- cbind(indicador,scores)
colors.dend <- c$.
cc <- as.dendrogram(aglomerativo_C_CL_PCA,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
c2_c_PCA <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Complete")
heights_per_k.dendrogram(as.dendrogram(dend3c))["2"]
C2_c_PCA <- c2_c_PCA + geom_hline(yintercept =1.414214  , size=1, linetype = 2 , color="blue")

# divisible correlación

class(indicador)
c <- cbind(indicador,scores)
colors.dend <- c$.
cc <- as.dendrogram(divisible_C,hang=-1)
cols <- c$.[order.dendrogram(cc)]
labels_colors(cc) <- as.numeric(cols)
cc  <- cc %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green")) 
ggd1 <- as.ggdend(cc)
dc_PCA <- ggplot(ggd1, horiz = T, theme = NULL) + ggtitle("Divisible")
heights_per_k.dendrogram(as.dendrogram(dend4c))["2"]
d2c_PCA <- dc_PCA + geom_hline(yintercept = 1.414214   , size=1, linetype = 2 , color="blue")

grid.arrange(C2_c_PCA , d2c_PCA, nrow = 1)

############################################################
# Fin Ejercicio 1
#
############################################################

############################################################
# Ejercicio 2
#
############################################################


# primera alternativa es hacer un cluster en variables
# pero son muchos genes

# hacer un heat map para microarreglos
# es doble en filas y columnas

# tercera bioconductor que nos diga cosas al respecto

# heatmap de prueba
options(repr.plot.width=6, repr.plot.height=6)
heatmap(as.matrix(base))
heatmap(as.matrix(t(X)))

# utilizar bioconductor
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
library("ComplexHeatmap")

# realizar hetmap
Heatmap(t(X))
Heatmap(base)
# utilizando mis clusters distancia euclidiana y enlace completo
dend3  <- dend3 %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green"))
Heatmap(t(scores), cluster_columns = dend3, heatmap_legend_param = list(title = ""))
Heatmap(t(X), cluster_columns = dend3, name = "", km = 2)
 
# utilizando mis clusters distancia euclidiana y divisible    
dend4  <- dend4 %>% set("labels_cex", c(.7,.7))  %>% 
  set("branches_k_color", k = 2, value = c("purple", "green"))
Heatmap(t(scores), cluster_columns = dend4, heatmap_legend_param = list(title = ""))
Heatmap(t(X), cluster_columns = dend4, name = "", km = 2)

############################################################
# Fin Ejercicio 2
#
############################################################