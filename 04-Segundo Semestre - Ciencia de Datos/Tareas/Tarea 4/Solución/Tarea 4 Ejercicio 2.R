############################################
############################################
############################################
#####   Tarea 4. Ciencia de datos
#####   Ejercicio 2. Imagene
#####   Miranda Belmonte Hairo Ulise
#####   22 de Marzo del 2019
############################################
############################################
############################################

# Librerias
library("imager") # manipular imagenes
library("plyr") # visualización 
library("raster") # manipular imagenes
library("gridExtra") # visualizar imagenes
library("grid") # visualizar imagnes
library("rgl") # manipular imagenes
library("ggplot2") # visualizar imagenes
library("plot3D") # visualizar 3d imagenes
library("plot3Drgl") # visualizar 3d imagenes
library("kernlab") # Kernel PCA y Kernel K-means

# Importar carpeta de imagene; introduccir dirección
files <- load.dir("C:/Users/h_air/Desktop/TAREA 4/Tarea 4-20190316/data_fruits_tarea/data_fruits_tarea")

##############################################
# Matrices que almacenan medianas para el caso RGB y HSV
Medianas <- matrix(0L, 1300,3)
MedianasHSV <- matrix(0L, 1300,3)

##############################################



##################
# PARTE 1 RGB
##################

# Se importa imagen por imagen y se separa en tres canales 
 # a cada canal de una imagen le tomamos la mediana
for(i in 1:1300) {
Medianas[i,] <- imsplit(files[[i]],"c") %>% laply(median) #medianpixel 
}

# Realizamos la visualiación del espacio RGB 
open3d()
plot3d(Medianas[,1], Medianas[,2], Medianas[,3], 
       col=rgb(Medianas[,1], Medianas[,2], Medianas[,3]), # colores de los clustesr
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE)
play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )

scatter3D(Medianas[,1], Medianas[,2], Medianas[,3], bty = "g", pch = 18,
          col = rgb(Medianas[,1], Medianas[,2], Medianas[,3]) ,
          colkey = F, main ="RGB",
          theta = 50, phi =0, xlab = "R",
          ylab ="G", zlab = "B") # direcciones


plotrgl()

 

##################
# PARTE 2 HSV
##################

# Se importa imagen por imagen y se separa en tres canales 
# a cada canal de una imagen le tomamos la mediana
for(i in 1:1300) {
MedianasHSV[i,] <- RGBtoHSV(files[[i]]) %>% channels %>%  laply(median) #medianpixel 
}

# Se realiza visualización del espacio HSV
open3d()
plot3d((MedianasHSV[,1]/360), MedianasHSV[,2], MedianasHSV[,3], 
       col=hsv((MedianasHSV[,1]/360), MedianasHSV[,2], MedianasHSV[,3]), # colores de los clustesr
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE) 
play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 ) 

scatter3D((MedianasHSV[,1]/360), MedianasHSV[,2], MedianasHSV[,3], bty = "g", pch = 18,
          col =hsv((MedianasHSV[,1]/360), MedianasHSV[,2], MedianasHSV[,3]) ,
          colkey = F, main ="HSV",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()


# Representación de una imagen
grid.raster(files[[1]])

# Representación de imagen en RGB
s.R = files[[1]]
s.G = files[[1]]
s.B = files[[1]]
s.R[,,2:3] = 0
s.G[,,1]=0
s.G[,,3]=0
s.B[,,1:2]=0
img1 = rasterGrob(s.R)
img2 = rasterGrob(s.G)
img3 = rasterGrob(s.B)
grid.arrange(img1, img2, img3, nrow=1)

#  Representación de imagen en HSV
imageHsv <- RGBtoHSV(files[[1]])
img1_hsv = rasterGrob(imageHsv[,,,1]/360) # escalado
img2_hsv = rasterGrob(imageHsv[,,,2])
img3_hsv = rasterGrob(imageHsv[,,,3])
grid.arrange(img1_hsv, img2_hsv, img3_hsv, nrow=1)



####################
# RGB 
####################

# A continuación se trabaja el inciso "a" , "b" y "c" de la tarea para las medianas
# de los canales RGB para las 1300 imagenes

#####################
# PCA RGB medianas
#####################

# Se crea dataframe com las medianas de cada canal
# df es un data frame con 1300 filas y 3 columnas (R,G,B)
df = data.frame(
  red =Medianas[,1],
  green = Medianas[,2],
  blue = Medianas[,3]
)


# PCA valores de la mediana a los canales R,G,B
PCA <- prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
# representación en dos componentes
df$u <- PCA$x[,1]
df$v <- PCA$x[,2]

# Observamos que estos dos componentes explican 80% de la varianza
summary(PCA)

# PCA medianas RGB visualización (2 componentes)
ggplot(df, aes(x=u, y=v, col=rgb(red,green,blue))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="PCA medianas RGB",
                                                     x= "PC 1",
                                                     y= "PC 2")
# Se visualiza con 3 componentes
scatter3D(PCA$x[,1],PCA$x[,2],PCA$x[,3], bty = "g", pch = 18,
          col =rgb(Medianas[,1], Medianas[,2], Medianas[,3]) ,
          colkey = F, main ="PCA medianas RGB",
          theta = 10, phi =-5, xlab = "PC 1",
          ylab ="PC 2", zlab = "PC 3") # direcciones


plotrgl() # utilizar para obtener rotaciones en la visualización

##########################
#  kmeans en medianas RGB
##########################

df = data.frame(
  red =Medianas[,1],
  green = Medianas[,2],
  blue = Medianas[,3]
)


# Aplicamos k medias con k=10 debido a que detectamos visualmente
# en las imagenes 10 tonos de colores para todas las frutas en la muestra
set.seed(10)
K <- kmeans(df,10)
df$label <- K$cluster # asignación de clusters
Tabla1 <- K$cluster %>% table


# Se guardan centroides de cada canal para cada cluster
# esto se utiliza para colorear los grupos

colors <- data.frame(
  label = 1:nrow(K$centers), 
  R <- K$centers[,"red"],
  G <- K$centers[,"green"],
  B <- K$centers[,"blue"]
)

# Etiquetando valores de la mediana con cluster correspondiente
df$order <- 1:nrow(df)
df <- merge(df, colors)
df <- df[order(df$order),] # ordena los datos
df$order <- NULL # elimina variable order


# RGB Mediana  DE CLUSTERS KMEANS K=10 Visualización

scatter3D(df$red, df$green, df$blue, bty = "g", pch = 18,
          col =rgb(df$R, df$G, df$B),
          colkey = F, main ="kmeans medianas RGB k=10",
          theta = 50, phi =0, xlab = "R",
          ylab ="G", zlab = "B") # direcciones

plotrgl()


##################################
#  kmeans en PCA de medianas  RGB
##################################
# Se aplica kmeans en los pca; esto no se reporta por no tener
# resultados interesantes

# Aplicamos k medias con k=10 debido a que detectamos visualmente
# en las imagenes 10 tonos de colores para todas las frutas en la muestra
dfPCA <- PCA$x %>% as.data.frame
set.seed(0)
K_PCA_RGB <- kmeans(dfPCA,10)
dfPCA$label <- K_PCA_RGB$cluster # asignación de clusters
Tabla2 <- K_PCA_RGB$cluster %>% table


colors <- data.frame(
  label = 1:10, 
  R <- K$centers[,"red"],
  G <- K$centers[,"green"],
  B <- K$centers[,"blue"]
)

# Etiquetando valores de la mediana con cluster correspondiente
dfPCA$order <- 1:nrow(dfPCA)
dfPCA <- merge(dfPCA, colors)
dfPCA <- dfPCA %>% as.matrix
dfPCA <-dfPCA[order(dfPCA[,5]),] # ordena los datos
dfPCA <- dfPCA %>% as.data.frame
colnames(dfPCA) <- c("label","PC1","PC2","PC3","order","R","G","B")
dfPCA %>% names


# RGB Mediana  DE CLUSTERS KMEANS K=10 Visualización
# revisar esto está raro; usa los colores de kamans  o rgb
scatter3D(dfPCA$PC1, dfPCA$PC2, dfPCA$PC3, bty = "g", pch = 18,
          col =rgb(dfPCA$R,dfPCA$G,dfPCA$B),
          colkey = F, main ="kmeans k=10 a PCA de medianas RGB",
          theta = 50, phi =0, xlab = "PC1",
          ylab ="PC2", zlab = "PC3") # direcciones

plotrgl()

ggplot(dfPCA, aes(x=PC1, y=PC2, col=rgb(dfPCA$R,dfPCA$G,dfPCA$B))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kmeans al PCA de medianas RGB k=10",
                                                     x= "PC 1",
                                                     y= "PC 2")



#####################
# Kernel PCA RGB
#####################
df = data.frame(
  red =Medianas[,1],
  green = Medianas[,2],
  blue = Medianas[,3]
)

# Realizamos kernel PCA con un kernel gaussiano y una sigma de 10
dfPCA <- df %>% as.matrix 
set.seed(0)
k <- kpca(dfPCA, kernel = "rbfdot", kpar = list(sigma = 10))
dfPCA <- dfPCA %>% as.data.frame 
dfPCA$u <- k@rotated[,1]
dfPCA$v <- k@rotated[,2]
dfPCA$w <- k@rotated[,3]
# Realizamos la visualización con dos componentes
ggplot(dfPCA, aes(x=u, y=v, col=rgb(red,green,blue))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kernel PCA medianas RGB",
                                                     x= "PC 1",
                                                     y= "PC 2",
                                                     caption="Kernel Gaussiano")


# No presenta en el reporte los tres primeros componentes
# dado a que no se visualiza algo de forma clara
scatter3D(dfPCA$u, dfPCA$v, dfPCA$w, bty = "g", pch = 18,
          col =rgb(df$red,df$green,df$blue),
          colkey = F, main ="Kernel PCA medianas RGB",
          theta = 50, phi =0, xlab = "PC1",
          ylab ="PC2", zlab = "PC3") # direcciones


plotrgl()

#####################
# Kernel k-means RGB
#####################
# Ahora se realiza kernel kmeans a las medianas de los canales R,G,B

df = data.frame(
  red =Medianas[,1],
  green = Medianas[,2],
  blue = Medianas[,3]
)


# Aplicamoskernel kmeans con k=10 y sigma de 30
df <- df %>% as.matrix
set.seed(0)
K <- kkmeans(df, 10, kernel = "rbfdot",
                        kpar = list(sigma=30),
                        alg ="kkmeans") 
df <- df %>% as.data.frame

df$label <- K@.Data
Tabla5 <- K@.Data %>% table

# guardamos centroides de cada canal para dar color
colors = data.frame(
  label = 1:nrow(K@centers), 
  R <- K@centers[,1],
  G <- K@centers[,2],
  B <- K@centers[,3]
)

colnames(colors) <- c("label","R","G","B")
# Etiquetando mediana con cluster correspondiente
df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),] # ordena los datos
# elimina variable order
df$order = NULL


# Visualización

scatter3D(df$red, df$green, df$blue, bty = "g", pch = 18,
          col =rgb(df$R, df$G, df$B),
          colkey = F, main ="kernel kmeans medianas k=10 RGB",
          theta = 50, phi =0, xlab = "R",
          ylab ="G", zlab = "B") # direcciones


plotrgl()

###############################################################
# Aplicando Kernel k-means a KernelPCA de medianas de RGB
###############################################################

# NOTA: SE EQUIVOCA DEMASIADO ES POR ESO QUE ESTA PARTE NO SE PONE
# EN EL ANÁLISIS PERO QUEDA PARA PODER COMPROBAR LO MENCIONADO.

# Ahora se realiza kernel kmeans  a los componentes de Kernel PCA de las medianas RGB

# guardamos proyecciones de componentes
dfPCA2 <- dfPCA[,4:6] %>% as.data.frame
dfPCA2 <- dfPCA2 %>% as.matrix
set.seed(0)
K <- kkmeans(dfPCA2, 10, kernel = "rbfdot",
             kpar = list(sigma=30),
             alg ="kkmeans") 

dfPCA2 <- dfPCA2 %>% as.data.frame

dfPCA2$label <- K@.Data
Tabla4 <- K@.Data %>% table

Tabla5 # kernel kmeans a mediana RGB
Tabla4 # kernel kmean a los pca realizados por kernel pca



# Etiquetando valores de la mediana con cluster correspondiente
dfPCA2$order <- 1:nrow(dfPCA2)
dfPCA2 <- merge(dfPCA2, colors)
dfPCA2 <- dfPCA2 %>% as.matrix
dfPCA2 <-dfPCA2[order(dfPCA2[,5]),] # ordena los datos
dfPCA2 <- dfPCA2 %>% as.data.frame
colnames(dfPCA2) <- c("label","PC1","PC2","PC3","order","R","G","B")
dfPCA2 %>% names


# RGB Mediana  DE CLUSTERS KMEANS K=10 Visualización
# revisar esto está raro; usa los colores de kamans  o rgb
scatter3D(dfPCA2$PC1, dfPCA2$PC2, dfPCA2$PC3, bty = "g", pch = 18,
          col =rgb(dfPCA2$R,dfPCA2$G,dfPCA2$B),
          colkey = F, main ="kmeans k=10 a PCA de medianas RGB",
          theta = 50, phi =0, xlab = "PC1",
          ylab ="PC2", zlab = "PC3") # direcciones

plotrgl()

ggplot(dfPCA2, aes(x=PC1, y=PC2, col=label)) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kmeans al PCA de medianas RGB k=10",
                                                     x= "PC 1",
                                                     y= "PC 2")


###########################################################################
###########################################################################
# Con HSV 
###########################################################################
###########################################################################
####################

# A continuación se trabaja el inciso "d"  de la tarea para las medianas
# de los canales HSV para las 1300 imagenes

#####################
# separamos por canales
# Nota, canal H se esala a 0 y 1
df_hsv = data.frame(
  H = MedianasHSV[,1]/360,
  S = MedianasHSV[,2],
  V = MedianasHSV[,3]
)

#################
# PCA hsv
#################
# se realiza PCA a los canales H,S,V
PCA_hsv = prcomp(df_hsv[,c("H","S","V")], center=TRUE, scale=TRUE)
df_hsv$u_hsv = PCA_hsv$x[,1]
df_hsv$v_hsv = PCA_hsv$x[,2]
df_hsv$w_hsv = PCA_hsv$x[,3]
# Se observa que los dos primeros componentes describan en gran 
# porporción la varianza
summary(PCA_hsv)

# visualizació 2D
ggplot(df_hsv, aes(x=u_hsv, y=v_hsv, col=hsv(H,S,V))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="PCA mediana HSV",
                                                     x="PC 1",
                                                     y="PC 2")
# Visualización 3D

scatter3D(df_hsv$u_hsv,df_hsv$v_hsv,df_hsv$w_hsv, bty = "g", pch = 18,
          col =hsv(df_hsv$H,df_hsv$S,df_hsv$V),
          colkey = F, main ="PCA mediana HSV",
          theta = 50, phi =0, xlab = "PC 1",
          ylab ="PC 2", zlab = "PC 3") # direcciones


plotrgl()



#################
# kmeans hsv
#################
df_hsv = data.frame(
  H = MedianasHSV[,1]/360,
  S = MedianasHSV[,2],
  V = MedianasHSV[,3]
)


set.seed(1)
K_hsv  = kmeans(df_hsv,10)
df_hsv$label = K_hsv$cluster
tabla1_hsv <- K_hsv$cluster %>% table

# laber igual al número de clusters
# guardamos centroides de cada canal
colors_hsv = data.frame(
  label = 1:nrow(K_hsv$centers), 
  h = K_hsv$centers[,"H"],
  s = K_hsv$centers[,"S"],
  v = K_hsv$centers[,"V"]
)

# Etiquetando mediana con cluster correspondiente
df_hsv$order = 1:nrow(df_hsv)
df_hsv = merge(df_hsv, colors_hsv)
df_hsv = df_hsv[order(df_hsv$order),] # ordena los datos
# elimina variable order
df_hsv$order = NULL

# Visualizacióm
scatter3D(df_hsv$H, df_hsv$S, df_hsv$V, bty = "g", pch = 18,
          col =hsv(df_hsv$h, df_hsv$s, df_hsv$v),
          colkey = F, main ="kmeans mediana HSV k=10",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()

############################
# Kernel PCA HSV medianas
############################
df_hsv = data.frame(
  H = MedianasHSV[,1]/360,
  S = MedianasHSV[,2],
  V = MedianasHSV[,3]
)

# Realizamos kernel PCA con un kernel gaussiano y una sigma de 10
dfPCA <- df_hsv %>% as.matrix 
set.seed(1)
k <- kpca(dfPCA, kernel = "rbfdot", kpar = list(sigma = 10))
dfPCA <- dfPCA %>% as.data.frame 
dfPCA$u <- k@rotated[,1]
dfPCA$v <- k@rotated[,2]
dfPCA$w <- k@rotated[,3]
# Realizamos la visualización con dos componentes
ggplot(dfPCA, aes(x=u, y=v, col=hsv(H,S,V))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kernel PCA medianas HSV",
                                                     x= "PC 1",
                                                     y= "PC 2",
                                                     caption="Kernel Gaussiano")


##############################
# Kernel k-means HSV medianas
##############################

# Ahora se realiza kernel kmeans a las medianas de los canales R,G,B

df_hsv = data.frame(
  H = MedianasHSV[,1]/360,
  S = MedianasHSV[,2],
  V = MedianasHSV[,3]
)

# Aplicamoskernel kmeans con k=10 y sigma de 30

df_hsv <- df_hsv %>% as.matrix
set.seed(1)
KK <- kkmeans(df_hsv, 10, kernel = "rbfdot",
             kpar = list(sigma=25),
             alg ="kkmeans") 
df_hsv <- df_hsv %>% as.data.frame

df_hsv$label <- KK@.Data
tabla2_hsv <- KK@.Data %>% table

# guardamos centroides de cada canal para dar color
colors = data.frame(
  label = 1:nrow(KK@centers), 
  h <- KK@centers[,1],
  s <- KK@centers[,2],
  v <- KK@centers[,3]
)

colnames(colors) <- c("label","h","s","v")
# Etiquetando mediana con cluster correspondiente
df_hsv$order = 1:nrow(df_hsv)
df_hsv = merge(df_hsv, colors)
df_hsv = df_hsv[order(df_hsv$order),] # ordena los datos
# elimina variable order
df_hsv$order = NULL


# Visualización

scatter3D(df_hsv$H, df_hsv$S, df_hsv$V, bty = "g", pch = 18,
          col =hsv(df_hsv$h, df_hsv$s,df_hsv$v),
          colkey = F, main ="kernel kmeans medianas k=10 HSV",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()






###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

#######################################
# RGB utilizando 3 cuartiles centrales
#######################################
# NOTA: ESTO NO SE REPORTA EN EL DOCUMENTO YA QUE SOLO SE APLICA PARA EL
# INCISO D QUE SOLO TRABAJA CON EL CANA HSV


#####################
# PCA RGB medianas
#####################

# Se crea dataframe com las medianas de cada canal
# df es un data frame con 1300 filas y 3 columnas (R,G,B)
df = data.frame(
  red1 =Medianas[(1:433),1],
  red2 =Medianas[(434:866),1],
  red3 =Medianas[(867:1299),1],
  green1 = Medianas[(1:433),2],
  green2 = Medianas[(434:866),2],
  green3 = Medianas[(867:1299),2],
  blue1 = Medianas[(1:433),3],
  blue2 = Medianas[(434:866),3],
  blue3 = Medianas[(867:1299),3]
)


# PCA valores de la mediana a los canales R,G,B
PCA <- prcomp(df, center=TRUE, scale=TRUE)
# representación en dos componentes
df$u <- PCA$x[,1]
df$v <- PCA$x[,2]

# Observamos que estos dos componentes explican 80% de la varianza
summary(PCA)

# PCA medianas RGB visualización (2 componentes)
ggplot(df, aes(x=u, y=v, col=rgb(red1,green2,blue3))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="PCA medianas RGB",
                                                     x= "PC 1",
                                                     y= "PC 2")

##########################
#  kmeans en medianas RGB
##########################

df = data.frame(
  red1 =Medianas[(1:433),1],
  red2 =Medianas[(434:866),1],
  red3 =Medianas[(867:1299),1],
  green1 = Medianas[(1:433),2],
  green2 = Medianas[(434:866),2],
  green3 = Medianas[(867:1299),2],
  blue1 = Medianas[(1:433),3],
  blue2 = Medianas[(434:866),3],
  blue3 = Medianas[(867:1299),3]
)


# Aplicamos k medias con k=10 debido a que detectamos visualmente
# en las imagenes 10 tonos de colores para todas las frutas en la muestra
set.seed(10)
K <- kmeans(df,10)
df$label <- K$cluster # asignación de clusters
table1 <- K$cluster %>% table



#####################
# Kernel PCA RGB
#####################

df = data.frame(
  red1 =Medianas[(1:433),1],
  red2 =Medianas[(434:866),1],
  red3 =Medianas[(867:1299),1],
  green1 = Medianas[(1:433),2],
  green2 = Medianas[(434:866),2],
  green3 = Medianas[(867:1299),2],
  blue1 = Medianas[(1:433),3],
  blue2 = Medianas[(434:866),3],
  blue3 = Medianas[(867:1299),3]
)

# Realizamos kernel PCA con un kernel gaussiano y una sigma de 10
dfPCA <- df %>% as.matrix 
set.seed(0)
k <- kpca(dfPCA, kernel = "rbfdot", kpar = list(sigma = 10))
dfPCA <- dfPCA %>% as.data.frame 
dfPCA$u <- k@rotated[,1]
dfPCA$v <- k@rotated[,2]
dfPCA$w <- k@rotated[,3]
# Realizamos la visualización con dos componentes
ggplot(dfPCA, aes(x=u, y=v, col=rgb(red2,green2,blue2))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kernel PCA medianas RGB",
                                                     x= "PC 1",
                                                     y= "PC 2",
                                                     caption="Kernel Gaussiano")

#####################
# Kernel k-means RGB
#####################
# Ahora se realiza kernel kmeans a las medianas de los canales R,G,B

df = data.frame(
  red1 =Medianas[(1:433),1],
  red2 =Medianas[(434:866),1],
  red3 =Medianas[(867:1299),1],
  green1 = Medianas[(1:433),2],
  green2 = Medianas[(434:866),2],
  green3 = Medianas[(867:1299),2],
  blue1 = Medianas[(1:433),3],
  blue2 = Medianas[(434:866),3],
  blue3 = Medianas[(867:1299),3]
)


# Aplicamoskernel kmeans con k=10 y sigma de 30
df <- df %>% as.matrix
set.seed(0)
K <- kkmeans(df, 10, kernel = "rbfdot",
             kpar = list(sigma=30),
             alg ="kkmeans") 
df <- df %>% as.data.frame

df$label <- K@.Data
table2 <- K@.Data %>% table



###########################################################################
###########################################################################
# Con HSV en 3 cuartiles centrales
###########################################################################
###########################################################################

# ESTO SI SE PRESENTA EN EL DOCUMENTO

# separamos por canales
# Nota, canal H se esala a 0 y 1
df_hsv = data.frame( # NUEVE DIMENSIONES
  H1 =MedianasHSV[(1:433),1]/360,
  H2 =MedianasHSV[(434:866),1]/360,
  H3 =MedianasHSV[(867:1299),1]/360,
  S1 = MedianasHSV[(1:433),2],
  S2 = MedianasHSV[(434:866),2],
  S3 = MedianasHSV[(867:1299),2],
  V1 = MedianasHSV[(1:433),3],
  V2 = MedianasHSV[(434:866),3],
  V3 = MedianasHSV[(867:1299),3]
)


########################
# PCA hsv 9 DIMENSIONES
########################
# se realiza PCA a los canales H,S,V
PCA_hsv = prcomp(df_hsv, center=TRUE, scale=TRUE)
df_hsv$u_hsv = PCA_hsv$x[,1]
df_hsv$v_hsv = PCA_hsv$x[,2]
df_hsv$w_hsv = PCA_hsv$x[,3]
# Se observa que los dos primeros componentes describan en gran 
# porporción la varianza
summary(PCA_hsv)

# visualizació 2D
ggplot(df_hsv, aes(x=u_hsv, y=v_hsv, col=hsv(H1,S1,V1))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="PCA mediana HSV",
                                                     x="PC 1",
                                                     y="PC 2")




###########################
# kmeans hsv 9 DIMENSIONES
###########################
df_hsv = data.frame(
  H1 =MedianasHSV[(1:433),1]/360,
  H2 =MedianasHSV[(434:866),1]/360,
  H3 =MedianasHSV[(867:1299),1]/360,
  S1 = MedianasHSV[(1:433),2],
  S2 = MedianasHSV[(434:866),2],
  S3 = MedianasHSV[(867:1299),2],
  V1 = MedianasHSV[(1:433),3],
  V2 = MedianasHSV[(434:866),3],
  V3 = MedianasHSV[(867:1299),3]
)


set.seed(1)
K_hsv  = kmeans(df_hsv,10)
df_hsv$label = K_hsv$cluster
table1_hsv <- K_hsv$cluster %>% table
# laber igual al número de clusters
# guardamos centroides de cada canal
colors_hsv = data.frame(
  label = 1:nrow(K_hsv$centers), 
  h1 = K_hsv$centers[,"H1"],
  s1 = K_hsv$centers[,"S1"],
  v1 = K_hsv$centers[,"V1"],
  h2 = K_hsv$centers[,"H2"],
  s2 = K_hsv$centers[,"S2"],
  v2 = K_hsv$centers[,"V2"],
  h3 = K_hsv$centers[,"H3"],
  s3 = K_hsv$centers[,"S3"],
  v3 = K_hsv$centers[,"V3"]
)

# Etiquetando mediana con cluster correspondiente
df_hsv$order = 1:nrow(df_hsv)
df_hsv = merge(df_hsv, colors_hsv)
df_hsv = df_hsv[order(df_hsv$order),] # ordena los datos
# elimina variable order
df_hsv$order = NULL

# Visualizacióm
scatter3D(df_hsv$H1, df_hsv$S1, df_hsv$V1, bty = "g", pch = 18,
          col =hsv(df_hsv$h1, df_hsv$s1, df_hsv$v1),
          colkey = F, main ="kmeans mediana cuantil 1 HSV k=10",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()

scatter3D(df_hsv$H2, df_hsv$S2, df_hsv$V2, bty = "g", pch = 18,
          col =hsv(df_hsv$h2, df_hsv$s2, df_hsv$v2),
          colkey = F, main ="kmeans mediana cuantil 2 HSV k=10",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()

scatter3D(df_hsv$H3, df_hsv$S3, df_hsv$V3, bty = "g", pch = 18,
          col =hsv(df_hsv$h3, df_hsv$s3, df_hsv$v3),
          colkey = F, main ="kmeans mediana cuantil 3 HSV k=10",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()





#########################################
# Kernel PCA HSV medianas 9 DIMENSIONES
#########################################
df_hsv = data.frame(
  H1 =MedianasHSV[(1:433),1]/360,
  H2 =MedianasHSV[(434:866),1]/360,
  H3 =MedianasHSV[(867:1299),1]/360,
  S1 = MedianasHSV[(1:433),2],
  S2 = MedianasHSV[(434:866),2],
  S3 = MedianasHSV[(867:1299),2],
  V1 = MedianasHSV[(1:433),3],
  V2 = MedianasHSV[(434:866),3],
  V3 = MedianasHSV[(867:1299),3]
)

# Realizamos kernel PCA con un kernel gaussiano y una sigma de 1
dfPCA <- df_hsv %>% as.matrix 
set.seed(1)
k <- kpca(dfPCA, kernel = "rbfdot", kpar = list(sigma = 1))
dfPCA <- dfPCA %>% as.data.frame 
dfPCA$u <- k@rotated[,1]
dfPCA$v <- k@rotated[,2]

# Realizamos la visualización con dos componentes
ggplot(dfPCA, aes(x=u, y=v, col=hsv(H2,S2,V2))) + 
  geom_point(size=2) + scale_color_identity() + labs(title="Kernel PCA medianas HSV",
                                                     x= "PC 1",
                                                     y= "PC 2",
                                                     caption="Kernel Gaussiano")


#############################################
# Kernel k-means HSV medianas 9 DIMENSIONES
#############################################

# Ahora se realiza kernel kmeans a las medianas de los canales R,G,B

df_hsv = data.frame(
  H1 =MedianasHSV[(1:433),1]/360,
  H2 =MedianasHSV[(434:866),1]/360,
  H3 =MedianasHSV[(867:1299),1]/360,
  S1 = MedianasHSV[(1:433),2],
  S2 = MedianasHSV[(434:866),2],
  S3 = MedianasHSV[(867:1299),2],
  V1 = MedianasHSV[(1:433),3],
  V2 = MedianasHSV[(434:866),3],
  V3 = MedianasHSV[(867:1299),3]
)

# Aplicamoskernel kmeans con k=10 y sigma de 30

df_hsv <- df_hsv %>% as.matrix
set.seed(1)
KK <- kkmeans(df_hsv, 10, kernel = "rbfdot",
              kpar = list(sigma=25),
              alg ="kkmeans") 
df_hsv <- df_hsv %>% as.data.frame

df_hsv$label <- KK@.Data
table2_hsv <- KK@.Data %>% table

# guardamos centroides de cada canal para dar color
colors_hsv = data.frame(
  label = 1:nrow(KK@centers), 
  h1 = K_hsv$centers[,1],
  h2 = K_hsv$centers[,2],
  h3 = K_hsv$centers[,3],
  s1 = K_hsv$centers[,4],
  s2 = K_hsv$centers[,5],
  s3 = K_hsv$centers[,6],
  v1 = K_hsv$centers[,7],
  v2 = K_hsv$centers[,8],
  v3 = K_hsv$centers[,9]
)


# Etiquetando mediana con cluster correspondiente
df_hsv$order = 1:nrow(df_hsv)
df_hsv = merge(df_hsv, colors_hsv)
df_hsv = df_hsv[order(df_hsv$order),] # ordena los datos
# elimina variable order
df_hsv$order = NULL


# Visualización

scatter3D(df_hsv$H1, df_hsv$S1, df_hsv$V1, bty = "g", pch = 18,
          col =hsv(df_hsv$h1, df_hsv$s1,df_hsv$v1),
          colkey = F, main ="kernel kmeans medianas cuantil 1 k=10 HSV",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()


scatter3D(df_hsv$H2, df_hsv$S2, df_hsv$V2, bty = "g", pch = 18,
          col =hsv(df_hsv$h2, df_hsv$s2,df_hsv$v2),
          colkey = F, main ="kernel kmeans medianas cuantil 2 k=10 HSV",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()



scatter3D(df_hsv$H3, df_hsv$S3, df_hsv$V3, bty = "g", pch = 18,
          col =hsv(df_hsv$h3, df_hsv$s3,df_hsv$v3),
          colkey = F, main ="kernel kmeans medianas cuantil 3 k=10 HSV",
          theta = 50, phi =0, xlab = "H",
          ylab ="S", zlab = "V") # direcciones


plotrgl()


################################################################
################################################################
##################### FIN DEJ EJERCICIO 2 ######################
################################################################
################################################################
################################################################
