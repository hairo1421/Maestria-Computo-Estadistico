# Descriptivos
N <- function(X) return(X[,1] %>% length)

ones <- function(X){
  unos <- X[,1] %>% length  %>% 
    diag %>% diag %>% as.matrix
  return(unos)
}

media <- function(X) return((1/N(X))*(t(X)%*%ones(X)))

matrizCentrado <- function(X) return(X-ones(X)%*%t(media(X)))

covarianza <- function(X) return((1/(N(X)-1))*t(matrizCentrado(X))%*%matrizCentrado(X))

X <- numeric()
Xmean <-  matrix(c(5.26,-3.09)) 
S <- matrix(c(7.12,-0.67,-0.67,12.43),2,2)
Sinv <-  solve(S)
X1 <- c(1.43, 1.62, 2.46, 2.48, 2.97, 4.03, 4.47, 5.76, 6.61, 6.68, 6.79, 7.46, 7.88, 8.92, 9.42)
X2 <- c(-0.69, -5, -1.13, -5.20, -6.39 ,2.87 ,-7.88 ,-3.97, 2.32, -3.24, -3.56, 1.61,-1.87,-6.60,-7.64)
X <- cbind(X1, X2)

# Distncia mahallanobis
mahalanobis(X,mean(X),covarianza(X))

dmahallanobis<- function(X){
  S <- covarianza(X)
  Sinv<- S %>% solve
  j <- 1:length(X[,1]) 
  d <- sapply(j, function(j) t(X[j,]-media(X))%*%Sinv%*%(X[j,]-media(X)))
  return(as.matrix(d))
  }

# t-student2 p=1
tstudent2 <- function(X,mu) N(X)*((media(X)-mu)^2)/var(X)

#T2 hotelling
t2hotelling <- function(X,mu) N(X)*(t(media(X)-mu)%*%solve(covarianza(X))%*%(media(X)-mu))

# F critico T2 hotellinf

Fcritico <- function(X,alpha) ((dim(X)[2]*(N(X)-1))/(N(X)-dim(X)[2]))*qf(alpha, dim(X)[2],N(X) -dim(X)[2], lower.tail = F) 

# EVALUANDO NORMALIDAD UNIVARIADA

sapply(i, function(i) shapiro.test(X[,i]))

# QQplot Solo para una serie
library("ggpubr")
ggqqplot(datos$X) + theme_gray() +  labs(title="QQ-plot",
                                         subtitle = "residentes ")

qqplot<- function(X){
  X <- sort(X)
  j <- 1:length(X)
  Adj<- (j -.5)/length(X) 
  Q <-qnorm(Adj)
  datos <- cbind(X,Adj,Q) %>%  as.data.frame
  coeficientes <- lm(X~Q)$coefficient
 
  p <-  datos %>% ggplot() + geom_point(aes( x=Q ,y=X), color="black", size=2) + 
    geom_abline(intercept = coeficientes[1], slope = coeficientes[2], linetype = 2, colour = "Red", size=1.5) +
    labs(title = " QQ-plot", 
         subtitle = "", 
         x="Quantiles",
         y="Obs.",
         caption = "") +
    theme( panel.background = element_rect(fill = "gray"),
           plot.margin = margin(.3, .3, .3, .3, "cm"),
           plot.background = element_rect(
             fill = "grey90",
             colour = "black",
             size = 1
           ))
  r <- sum(((X-mean(X)))*((Q-mean(Q))))/ (sqrt(sum((X-mean(X))^2))*sqrt(sum((Q-mean(Q))^2)))
  return(list(r=r,plot=p))
  
}
# pendiente programar
# contrastar contra tabla 
# rechazas normalidad a un alpha si
# r<criticos alpha
X <- matrix(c(-3.83, -3.65, -2.80, -2.79, -2.30, -1.23,-.80,.49,1.35,1.41,1.52,2.19,2.62,3.66,4.15 ))
cor(sort(X),sort(Q))  
qqplot(X)

dev.off()
# EVALUANDO NORMALIDAD BIVARIADA
X1 <- c(1.43, 1.62, 2.46, 2.48, 2.97, 4.03, 4.47, 5.76, 6.61, 6.68, 6.79, 7.46, 7.88, 8.92, 9.42)
X2 <- c(-0.69, -5, -1.13, -5.20, -6.39 ,2.87 ,-7.88 ,-3.97, 2.32, -3.24, -3.56, 1.61,-1.87,-6.60,-7.64)
X <- cbind(X1, X2)

# diagramas de dispersión
plot(X,pch=19, col='darkgray', xlim=c(0,12), ylim=c(-20,20))
car::ellipse(colMeans(X),
             covarianza(X),
          radius= sqrt(qchisq(0.5,ncol(X),lower.tail = FALSE)),
          col = "blue",
         fill = TRUE, 
        fill.alpha = 0.05 )
abline(h=0, lty=2, lwd=1)

X %>% as.data.frame %>% ggplot() + aes(x = X[,1],y = X[,2]) +
  geom_point() + labs(title="a",
                        x = "b",
                        y = "c",
                        caption = "d",
                        subtitle ="e") +
  stat_ellipse(level=0.95, color="blue",linetype = 2) 


# Porcentaje de observaciones dentro de la ellipse

porcentajeEllipse <- function(X,nivel){
  dentro <- dmahallanobis(X) <= qchisq(nivel,2)
  return(mean(dentro)*100)
}

porcentajeEllipse(X,.5)


# diagramas chi-cuadrado

library("ggpubr")
d <- dmahallanobis(X) %>% as.data.frame
ggqqplot(d$V1) + theme_gray() +  labs(title="QQ-plot",
                                         subtitle = "residentes ")
DiagramaChi2 <- function(X){
  d <- dmahallanobis(X) %>% sort
  j <- 1:length(d)
  Adj <- (j-.5)/length(d)
  Q <- qchisq(Adj, ncol(X))
  datos <- cbind(d,Adj,Q) %>%  as.data.frame
  coeficientes <- lm(d~Q)$coefficient
  
  p <-  datos %>% ggplot() + geom_point(aes( x=Q ,y=d), color="black", size=2) + 
    geom_abline(intercept = coeficientes[1], slope = coeficientes[2], linetype = 2, colour = "Red", size=1.5) +
    labs(title = " QQ-plot", 
         subtitle = "", 
         x="Quantiles",
         y="Obs.",
         caption = "") +
    theme( panel.background = element_rect(fill = "gray"),
           plot.margin = margin(.3, .3, .3, .3, "cm"),
           plot.background = element_rect(
             fill = "grey90",
             colour = "black",
             size = 1
           ))
  return(list(r=cor(d,Q),plot=p))
}

# correlación contrastar contra tabla
# se rechaza la Ho de normalidad se valores críticos
# son mayores a r
DiagramaChi2(X)


# TRANSFORMACIONES PARA APROXIMAR NORMALIDAD 

conteos<- function(X) sqrt(X)
porporciones <- function(X) (1/2)*log(X/(1-X))
  # transformación Z de Fisher
correlaciones <- function(X) (1/2)*log((1+X)/(1-X)) 


boxCox <- function(x,lam){
# box cox transformación
Boxcox_transformacion <- function(i){
  if(lam[i] == 0){
    x <- log(x)
  }else{
    x <- ((x^lam[i])-1)/lam[i]
  }
  return(x)
}
# Encontrando parametro maximo
maxval <- function(i){
  xj <- Boxcox_transformacion(i)
  n <- length(xj)
  xmeanj <- (1/n)*sum(xj)
  Llam[i] <- (-n/2)*log((1/n)*sum(((xj-xmeanj)^2)))+
    (lam[i]-1)*sum(log(x))
}

Llam <- numeric(0)
i <- 1:length(lam)
L <- numeric(0)
L <-sapply(i,  maxval) %>% as.data.frame
L <- cbind(L,lam)

colnames(L) <-c("LVal","Lambda")

L <- L %>% arrange(desc(LVal)) 
lamHat1 <- L[1,2]
plotL <- na.omit(L) %>% ggplot() + geom_line(aes(x=Lambda,y=LVal), size=1,col="black") +
  geom_vline(xintercept = L[1,2],  linetype="dashed", color = "red") +
  geom_hline(yintercept = L[1,1],  linetype="dashed", color = "red") +
  labs(x=expression(lambda),
       y=expression(gamma(lambda)))

return(list(plotL = plotL, lambdaOptima = lamHat1))
}

library("tidyverse")
# univariada
x1 <- c(12.5, 14.5,8,9,19.5,8,9,7,7,9,6.5,10.5,10,4.5,
        7,8.5,6.5,8,3.5,8,17.5,10.5,12,6,13)
x2 <- c(13.7,16.5,17.4,11,23.6,13.2,32.1,12.3,11.8,24.4,
        18.2,22,32.5,18.7,15.8,15.6,12,12.8,26.1,14.5,42.3,
        17.5,21.8,10.4,25.6)

Obs <- cbind(x1,x2) %>% as.data.frame
x <- Obs$x1
lam <- seq(-10,10,.01)
boxCox(x, lam)
# NOTA:
#Se le puede sumar una constante m a cada observacion en el conjunto de
#datos, de tal forma que si algunos de los valores son negativos, entonces
#x + m > 0

# Transformación potencia multivariada
  # puede ser una por una pero no suficiente que marginales
  # sean normales
boxCox(Obs[,1],lam)
boxCox(Obs[,2],lam)

# Podemos iniciar con transformaciones marginales


library("MASS")
library("geoR")
library("AID")
library("car")
library("mvtnorm")

# multivariate
lambdas <- c(lamHat1,lamHat2)

LamdasMv <- powerTransform(as.matrix(Obs), start=lambdas)


LamdasMv <- LamdasMv$lambda 
LamdasMv <-LamdasMv %>% as.data.frame
colnames(LamdasMv) <- c("Parámetro de Transformación") 
LamdasMv%>%
  kable %>% kable_styling



# lambda sin 2/n
lamdadeWilks_sin_2n <- function(X,mu){
  n <- length(X[,1])
  A <- t(matrizCentrado(X))%*%matrizCentrado(X)/N(X)
  B <- t(X-ones(X)%*%t(mu))%*%(X-ones(X)%*%t(mu))/N(X)
  return(det(A)/(det(B)^(n/2)))
}


# lambda 2/n
lamdadeWilks<- function(X,mu){
A <- t(matrizCentrado(X))%*%matrizCentrado(X)/N(X)
B <- t(X-ones(X)%*%t(mu))%*%(X-ones(X)%*%t(mu))/N(X)
return(det(A)/det(B))
}

# lambda 2/n Aproximación T de hotelling
# B matriz de covarianzas estimada sobre la nula
# A matriz de covarianza completa
# n tamaño
# alpha significancia
lamdadeWilksT2 <- function(A,B,n,alpha){
  numerador <- (n-1)*det(B)
  t2 <- (numerador/det(A))-(n-1) # estadístico
  lambdaW <- (1+(t2/(n-1)))^(-1) # estadistico
  
  critico <- Fcritico(X,alpha) # valor critico T2
  lambdaW_critico <- (1+(critico/(n-1)))^(-1) # valor crpitico razón verosimilitud
  test <- lambdaW >= lambdaW_critico # No rechazo
  return(list(T2Prueba=t2,LWPrueba=lambdaW, 
              T2CriticoF=critico, LWCritico=lambdaW_critico,NoRechaza=test))
}


matrizcentrado_mu<- function(X,mu){
  return(X-ones(X)%*%t(mu))
}

# Ejemplo lambda de wilks
mua <- matrix(c(4,-1.5))
S <- covarianza(X)
Salterno <- t(matrizcentrado_mu(X,mua))%*%matrizcentrado_mu(X,mua)/(length(X[,1])-1)

# Esta función da como resultado
# las funciones lamdadeWilks y t2hotelling
lamdadeWilksT2(S,Salterno,length(X[,1]),.1) # es el mismo que el de t2hotelling
#lamdadeWilks(X,mua)
#t2hotelling(X,mua) # valor estadístico


# OJO CON VALOR CRÍTICO 
lambdaWilks_muestra_grande <- function(X,mu,alpha){
  lambdaW <- lamdadeWilks_sin_2n(X,mu)
  Lam <- -2*log(lambdaW)
  v <- dim(X)[2] + (dim(X)[2]*(dim(X)[2] + 1)/2)
  vo <-  (dim(X)[2]*(dim(X)[2] + 1)/2)
  ChiCritico <- qchisq(alpha,v-vo,lower.tail = FALSE)
  test <- Lam >= ChiCritico # No rechazo
  return(list(LambdaW=Lam, chi=ChiCritico,NoRechazo=test ))
}

lambdaWilks_muestra_grande(X,mua,.1)




#REGIONES DE CONFIANZA PARA EL VECTOR DE MEDIAS
RegionConfianza <- function(X,mu,alpha){
  
T2 <-t2hotelling(X,mu)
Fest <- Fcritico(X,alpha) 
test <- T2 <=Fest # No se rechaza Ho si es menor
# equivale a decir que la media nula está dentro de la region
return(list(T2=T2,Fcritico=Fest,CaeEnRegion=test))
}

# Ejemplo
mu2<-matrix(c(-1.5,4))
RegionConfianza(X,mu2,.05)

EjesElipsoide<-function(X,alpha){
 
n <- length(X[,1]) 
S <- cov(X)
p <- dim(X)[2]
Fe <- Fcritico(X,alpha)
valores <- S %>% eigen 
valoresPropios <- valores$values #valor propios
vectoresPropios <- valores$vectors #vectores propios
ejeMayor <- sqrt(valoresPropios[1])*sqrt(((p*(n-1))/(n*(n-p)))*Fe) # eje mayor de elipse
ejeMenor <-  sqrt(valoresPropios[2])*sqrt(((p*(n-1))/(n*(n-p)))*Fe) # eje menor de elipse
return(list(ejeMayor=ejeMayor,ejeMenor=ejeMenor,VectoresPropios=vectoresPropios))
}

#Ejemplo
ejes <- EjesElipsoide(X,.1)
Xmean <- colMeans(X)
mua<-matrix(c(4,-1.5))
#######TRAZAR EJES Y ELIPSE DE COFIANZA
X %>% as.data.frame %>% ggplot()  + stat_ellipse(aes(x=X[,1], y=X[,2]), level=0.50) +
    geom_point(x=mua[1], y=mua[2], colour="red") + 
  geom_point(x=Xmean[1], y=Xmean[2], colour="blue") +
  #geom_segment(aes(x=Xmean[1], y=Xmean[2], xend=mu0[1], yend=mu0[2]), arrow = arrow()) +
  labs(title="Elipse ",
       x="X1",
       y="X1",
       caption = "Punto azul centroide de la media; Punto rojo centroide de Ho.") +
  geom_point(x=Xmean[1]+ejes$ejeMayor, y=Xmean[2]+ejes$ejeMayor, colour="green") +
  geom_point(x=Xmean[1]-ejes$ejeMenor, y=Xmean[2]+ejes$ejeMenor, colour="black") +
  geom_segment(aes(x=Xmean[1], y=Xmean[2], xend=Xmean[1]-ejes$ejeMenor, yend=Xmean[2]+ejes$ejeMenor), arrow = arrow()) +
  geom_segment(aes(x=Xmean[1], y=Xmean[2], xend=Xmean[1]+ejes$ejeMayor, yend=Xmean[2]+ejes$ejeMayor), arrow = arrow()) 

# haz ellipse con función ellipse



###INTERVALOS DE CONFIANZA SIMULTANEOS

# aproximación de a que maximiza t strudent al cudrado
S <- X %>% cov
Sinv <- X %>% cov %>% solve
Xmean <- X %>% colMeans %>% as.matrix
n <- length(X[,1])
a <- Sinv%*%(Xmean - mu2) #aproxmación de "a"
#t de student al cuadrado, con la aproximación de "a"" 
t2 <- n*(t(a)%*%(Xmean - mu2))^2/t(a)%*%S%*%a
t2hotelling(X,mu2)


# t de student al cuadrado simultaneo univariado
# estos intevalos no garantizan mantenerse simultaneamente

tstudent_intervalo_simultaneo <- function(X,a,alpha){
n <- length(X[,1])  
S <- X %>% cov
Sinv <- S %>% cov
Xmean <- X %>% colMeans %>% as.matrix
t<- qt(alpha/2,(n-1))
lower <- t(a)%*%Xmean - t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
upper <- t(a)%*%Xmean + t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
return(list(Upper=upper,Lower=lower))
}

a <-matrix(c(0,1)) 
tstudent_intervalo_simultaneo(X,a,.1)



# T2 Hotelling al cuadrado simultaneo 
# se mantienen simultaneamente pero no son ajustados
# t de student al cuadrado simultaneo univariado
T2_intervalo_simultaneo <- function(X,a,alpha){
  n <- length(X[,1]) 
  p <- dim(X)[2]
  S <- X %>% cov
  Xmean <- X %>% colMeans %>% as.matrix
  Fdistribucion <- qf(alpha,p, (n-p), lower.tail = F )
  critico <- (((n-1)*p)/(n*(n-p)))*Fdistribucion
  lower <- t(a)%*%Xmean - sqrt(critico*(t(a)%*%S%*%a))
  upper <- t(a)%*%Xmean + sqrt(critico*(t(a)%*%S%*%a))
  return(list(Upper=upper,Lower=lower))
}

5.26+(-3.09)-sqrt(5.95)*sqrt((7.12-2*(0.72)+12.43)/15)


a <-matrix(c(1,1)) 
T2_intervalo_simultaneo(X,a,.1)
  

# Método de Bonferroni de comparaciones múltiples
# estos intevalos no garantizan mantenerse simultaneamente
Bonferroni <- function(X,a,alpha){
  n <- length(X[,1]) 
  p <- dim(X)[2]
  S <- X %>% cov
  Xmean <- X %>% colMeans %>% as.matrix
  t<- qt(alpha/(2*p),(n-1))
  lower <- t(a)%*%Xmean - t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  upper <- t(a)%*%Xmean + t*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  return(list(Upper=upper,Lower=lower))
}


a <-matrix(c(0,1)) 
Bonferroni(X,a,.1)


# Inferencia sobre el vector de medias de la población para muestras grandes
# n-p es grande
# Ho:mu=mu0
# rechazas a favor de H1:mu diff mu0
# T2>chi 


Chi_intervalo <- function(X,a,alpha){
  n <- length(X[,1]) 
  p <- dim(X)[2]
  S <- X %>% cov
  Xmean <- X %>% colMeans %>% as.matrix
  chi<- qchisq(.1,p, lower.tail = FALSE)
  lower <- t(a)%*%Xmean - sqrt(chi)*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  upper <- t(a)%*%Xmean + sqrt(chi)*(sqrt(t(a)%*%S%*%a)/sqrt(n))
  return(list(Upper=upper,Lower=lower))
}

n <- 107
p <- 5
Xmean <- matrix(c(58.6,19.3,101.5,67,42.5))
S <- c(4.44,2.39,8.87,3.39,4.69)^2
S <- diag(S)
  
a <-matrix(c(0,1,0,0,0)) 
Chi_intervalo(X,a,.1)
19.3-sqrt(9.236)*(2.39/sqrt(107))

# Modelos de regresón lineal multivariada

# E(epsilon_j)=0
# Var(epsilon_j)= sigma^2(kte)
# cov(epsilon_j,epsilon_k)=0
# j diferente k

# Nota, si es a mano la regresión utiliza
# un vectoe de unos para la constante.

# Ejemplo

z1 <- matrix(c(0,1,2,3,4))
unos <- ones(z1)
Z <- cbind(unos,z1)
y <- matrix(c(1,4,3,8,9))

b_multiple <- function(Z,y){
  return(solve(t(Z)%*%Z)%*%t(Z)%*%y)
}

beta <- b_multiple(Z,y)

# prueba de supuestos
matrizSombrero<- function(Z){
  return(Z%*%solve(t(Z)%*%Z)%*%t(Z))
}

matrizSombrero(Z) 

yest <-  Z%*%b_multiple(Z,y)
yest <- matrizSombrero(Z)%*%y



identidades <- function(Z){
  return(diag(length(Z[,1])))
}
  

# cuidado con problemas de redondeo 
residuales <- y - yest
residuales <- (identidades(Z)-matrizSombrero(Z))%*%y




RSS <-   t(y)%*%y-t(y)%*%Z%*%b_multiple(Z,y)
RSS <- t(y)%*%(identidades(Z) - matrizSombrero(Z))%*%y
RSS <-t(residuales)%*%residuales


# evaluar supuestos
  # debe de dar cero
t(yest)%*%residuales
  # ortogonal con residuos
t(ones(Z))%*%residuales
  
# suma total de residuales
 # y'y
resExtra <- t(yest)%*%yest + RSS

Rcuadrado <- function(Z,y){
  RSS <-   t(y)%*%y-t(y)%*%Z%*%b_multiple(Z,y)
  sumaTotalCuadrado <-  t(matrizCentrado(y))%*%matrizCentrado(y)
  return(1-RSS/sumaTotalCuadrado)
}

# covarianza beta
s2 <- RSS/(length(Z[,1]-(dim(Z)[2]+1)))
as.numeric(s2)*solve(t(Z)%*%Z)



# MV
beta <- b_multiple(Z,y)
#  formas de calcular sigma ML
sigma2 <- RSS
length(Z[,1])*as.numeric(s2)

# Región de confianza
beta_region_confianza <- function(Z,y,b,alpha){
  
  r <- dim(Z)[2]
  n <- length(Z[,1])
  RSS <-   t(y)%*%y-t(y)%*%Z%*%b_multiple(Z,y)
  sigma2 <- RSS
  Beta <- t(b_multiple(Z,y)-b)%*%t(Z)%*%Z%*%(b_multiple(Z,y)-b)
  Fcrit <-(r+1)*as.numeric(sigma2)*qf(alpha,(r+1),(n-r-1),lower.tail = F)
  cae <- Beta <= Fcrit # cae dentro
  return(list(regionBeta=Beta,Fcrit=Fcrit,CaeDentro=cae))
}

# b es la hipotesis o la original
b <- matrix(c(7,8))
alpha <- .05
beta_region_confianza(Z,y,b,.1)

# Intervalo de confianza Beta
  #CHECAR QUE LA COVARIANZA LA CALCULAS BIEN

# intervalos de confianza beta simultaneo
beta_ICsimultaneo_multivariado <- function(beta,covarianzaBeta,a,alpha){
  
  r <- dim(Z)[2]
  n <- length(Z[,1])
  Upper <- t(a)%*%beta + sqrt(t(a)%*%covarianzaBeta%*%a)*sqrt((r+1)*qf(alpha,(r+1),(n-r-1)))
  Lower <- t(a)%*%beta - sqrt(t(a)%*%covarianzaBeta%*%a)*sqrt((r+1)*qf(alpha,(r+1),(n-r-1)))
  return(list(Upper=Upper,Lower=Lower))
}

s2 <- RSS/(length(Z[,1]-(dim(Z)[2]+1)))
covarianzaBeta <- as.numeric(s2)*solve(t(Z)%*%Z)
beta <- b_multiple(Z,y)
a <- matrix(c(1,0))
alpha <- .05

beta_ICsimultaneo_multivariado(beta,covarianzaBeta,a,alpha)

# intervalos univariados beta
  # CHECAR NO SE VE BIEN
beta_ICunivariado_multivariado <- function(beta,covarianzaBeta,a,alpha){
  
  r <- dim(Z)[2]
  n <- length(Z[,1])
  Upper <- t(a)%*%beta + sqrt(t(a)%*%covarianzaBeta%*%a)*qt(alpha/2,(n-r-1))
  Lower <- t(a)%*%beta - sqrt(t(a)%*%covarianzaBeta%*%a)*qt(alpha/2,(n-r-1))
  return(list(Upper=Upper,Lower=Lower))
}

s2 <- RSS/(length(Z[,1]-(dim(Z)[2]+1)))
covarianzaBeta <- as.numeric(s2)*solve(t(Z)%*%Z)
beta <- b_multiple(Z,y)
a <- matrix(c(1,0))
alpha <- .05

beta_ICunivariado_multivariado(beta,covarianzaBeta,a,alpha)


# Prueba de razón de verosimilitod en los parámetros de regresión
beta_original <- b_multiple(Z,y)
beta_incompleta <- b_multiple(Z[,1],y)

res_original  <- y -Z%*%beta_original
res_incompleta  <- y -Z%*%beta_original

s2 <- RSS/(length(Z[,1]-(dim(Z)[2]+1)))
r <- dim(Z)[2]
# CHECAR Q puede cambiar seguó la hipótesis
q <- length(Z[,1])

RSS_original <- t(res_original)%*%res_original
RSS_incompleta <- t(res_incompleta)%*%res_incompleta

alpha <- .05
# Prueba
EstadisticoRverosimilitud <- ((RSS_incompleta-RSS_original)/(r-q))/sigma2
ValorCritico <- qt(alpha, (r-q), (n-r-1))

# rechaza la Ho si 

EstadisticoRverosimilitud > ValorCritico

# Regresión multivariada multiple

  # datos sinteticos
y1 <- matrix(c(63,70,72,75,89,76))
y2 <- matrix(c(67,70,70,72,88,77))
Y <- cbind(y1,y2)
z1 <- matrix(c(65,72,77,68,81,73))
z2 <- matrix(c(71,77,73,78,76,87))
unos <- ones(z1)
Z <-  cbind(unos,z1)
Z <-  cbind(Z,z2)

beta_multivariada <- function(Z,Y){
  return(solve(t(Z)%*%Z)%*%t(Z)%*%Y)
}

beta_multivariada(Z,Y)

# Y estimada
yest <- Z%*%beta_multivariada(Z,Y)

# residuales
residuales_multivariados <- Y - yest # utiliza este
residuales_multivariados <- (identidades(Z)-matrizSombrero(Z))%*%Y

# supuestos multivariados
  # debe dar cero
t(Z)%*%residuales_multivariados
  # debe dar cero
t(yest)%*%residuales_multivariados

# suma de cuadrados de predichos y produtos cruzados
t(yest)%*%yest
# suma decuadrados de residuales y productos cruzados
Rss_multivariado <- t(residuales_multivariados)%*%residuales_multivariados
# suma total de cuadrados y productos cruazados
t(yest)%*%yest + Rss_multivariado
t(Y)%*%Y
# NOTA LA Z ES LA MATRIZ DE DISEÑO

# covarianza entre los coeficientes
  # checa como sacar sigma_ik
    # es la covarianza entre ellos dos
sigma_ik*solve(t(Z)%*%Z)
cov(beta_multivariada(Z,Y)) # revisar como 

# Estimadores de máxima verosimilitud y sus distribuciones
# se pueden obtener cuando los errores epsilon tienen 
# una distribución normal multivariada

  # la esperanza de los betas es la misma 
  # las covarianza entre los betas es la misma 
  # ela varianza de MV cambia

n <- length(Z[,1])
Rss_multivariado/n

# Prueba de razon de verosimilitud 

Z1 <- Z[,c(1,3)]
beta1 <- beta_multivariada(Z1,Y)
beta <-  beta_multivariada(Z,Y)
yest1 <- Z1%*%beta1
yest <-Z%*%beta
resid1 <- Y - yest1
resid <- Y - yest
RSS1 <- t(resid1)%*%resid1
RSS <- t(resid)%*%resid
Sigma1 <- RSS1/length(Z[,1])
Sigma <- RSS/length(Z[,1])
n <- length(Z[,1])
E <- n*Sigma
H <- n*(Sigma1-Sigma)
# cociente de razón de verosimilitud
  Lambda_multivariada <- (det(Sigma)/det(Sigma1))^(n/2)
# Lambda 2/n
  # Forma 1
LambdaWilks_multivariada <- det(Sigma)/det(Sigma1)
  # Forma 2
LambdaWilks_multivariada <- det(E)/det(E+H)
  # Forma 3
m <- dim(Y)[2]
r <- dim(Z)[2]
q <- dim(Z1)[2]
# chose the minimum
m
r-q


# in this example is the number one
values <- eigen(H%*%solve(E))$values
1/(1+values[1])


# NOTA: EN LIKELIHOOD RATIO TEST SI EL VALOR ES PEQUEÑO SE RECHAZA LA H0
# RECHAZA SI Lambda < c

# NOTA: LA LAMBDA DE WILKS SE RECHAZA A VALORES PEQUEÑOS DE LAMBDA^2/N O EQUIVALENTE 
# A VALORES GRANDES DE T2

# Equivalente, la prueba de razón de verosimilitud
# de H0 es equivalente a rechazar Ho para valores grandes de:
-2*log10(Lambda_multivariada)
# esto es igual a:
-n*log10(det(Sigma)/det(Sigma1))
# esto es igual a:
-n*log10(det(n*Sigma)/det(n*Sigma+n*(Sigma1-Sigma)))

# CHECA QUE SEA PARA LOG DE 10
n <- length(Z[,1])
r <- dim(Z)[2]
# si n-r u n-m son ambos grandes
# le quite el menos uno, porque el r ya lo lleva
Estadistico <- -(n-r-(1/2)*(m-r+q+1))*log10(det(Sigma)/det(Sigma1))
# se distribute
Critico <-qchisq(.01,m*(r-q), lower.tail = F)
# No se rechaza Ho
Estadistico <= Critico
pvalor <- 1-pchisq(Estadistico, 2)


# INTERVALOS DE CONFIANZA REGRESIÓN MULTIVARIADA

# Predecir la respuesta media
# z0 valor fijo de las vvariables predictoras, una fila
t(beta)%*%z0

a <- t((t(beta_est)%*%z0-t(beta)%*%z0)/(sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0)))
#b <- solve((n/(n-r-1))*Sigma)
b <- solve((n/(n-r))*Sigma) # le quite el uno porque ya lo contabilizo en r
c <- (t(beta_est)%*%z0-t(beta)%*%z0)/(sqrt(t(z0)%*%solve(t(Z)%*%Z)%*%z0))
T2 <- a%*%b%*%c

# Ellipsoide de confianza para la función de regresión bera'z0
# asoiciado con z0
d <- t(t(beta_est)%*%z0-t(beta)%*%z0)%*%b%*%(t(beta_est)%*%z0-t(beta)%*%z0)
e <- (t(z0)%*%solve(t(Z)%*%Z)%*%z0)*((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F) # le quite el 1 porque r ya lo incluye
# Si es cierto lo de abaja, entonces cae dentro de la ellipse
d <= e

# INTERVALOS DE CONFIANZA PARA LA FUNCIÓN DE REGRECIÓN ASOCIADAS CON beta'z0

# n-r-1 no se lo pongo porque r ya lo incluye
t(z0)%*%beta_est[i]+sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt((t(z0)%*%solve(t(Z)%*%Z)%*%z0))*sqrt((n/(n-r))*Sigma[i,i])
t(z0)%*%beta_est[i]-sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt((t(z0)%*%solve(t(Z)%*%Z)%*%z0))*sqrt((n/(n-r))*Sigma[i,i])

# ELIPSOIDE E INTERVALOS DE CONFIANZA PARA PREDICCIONES Yi

f <- t(Y0-t(beta_est)%*%z0)%*%solve((n/(n-r))*Sigma)%*%(Y0-t(beta_est)%*%z0)
g <- (1+t(z0)%*%solve(t(Z%*%Z))%*%z0)*((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F)
# entonces si se cumple lo de abajo, Yo cae dentro de la ellipse de confianza
f <= g

# Intervalos de confianza para la predicción
t(z0)%*%beta_est[i]+sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt(1+t(z0%*%solve(t(Z)%*%Z)%*%z0))*sqrt((n/(n-r))*Sigma[i,i])
t(z0)%*%beta_est[i]-sqrt(((m*(n-r))/(n-r-m))*qf(.05,m, n-r-m,lower.tail = F))*sqrt(1+t(z0%*%solve(t(Z)%*%Z)%*%z0))*sqrt((n/(n-r))*Sigma[i,i])
