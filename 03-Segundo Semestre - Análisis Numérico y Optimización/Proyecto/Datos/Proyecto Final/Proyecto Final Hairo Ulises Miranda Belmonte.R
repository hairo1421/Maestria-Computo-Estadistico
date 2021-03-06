# # # # # # # # # # # # # # # # # # # # # # # #  # #
# 
#  Optimizaci�n
#
# PROYECTO FINAL: An�lisis Envolvente de Datos (DEA);
# aplicaci�n en la medici�n de la eficiencia
# bancaria en M�xico.
#
# HAIRO ULISES MIRANDA BELMONTE
# VERSION 1.0
# 09 DE MAYO DEL 2019
#
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #        PARTE I: Datos     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# Utilice direcci�n donde se encuntran sus archivos xlsx
setwd("C:/Users/h_air/Desktop/Proyecto N�merioc/Datos")
getwd()

# Librer�as
library("openxlsx") # abrir archivos
library("tidyverse") # manipulaci�n de la base
library("rDEA") # dea
library("knitr") # presentaci�n de tablas
library("kableExtra") # estilo de tablas
library("htmlwidgets") # imagen a html
library("plotly") # gr�fica
library("Benchmarking") # dea
library("microbenchmark") # an�lisis microbenc

# Funci�n para extraer los datos para los meses MARZO, JUNIO,SEPTIEMBRE DICIEMBRE
# los cuales tienen un formato distinto a los meses restantes.

# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : MesesRaro(x)
#        Toma las variables que se necesitan de la base
#        
# Input: data frame
#        
# Outpu: variables que se necesitan 
#       
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
MesesRaro<-function(X){
# genera matrix
Datos <-  matrix(0L, 22,9) %>% as.data.frame
# son 22 bancos inicialmente
for(i in 1:22){
# limpieza y depuraci�n de la base
df1 <- read.xlsx(xlsxFile = X,sheet = 29+i)
df1 <- df1[ ,1:4]
names(df1) <- c("Valores", "EF del Banco", "Tarjetas Banamex", "Soriana")
banco <- df1[1,1]
names(banco) <- "Banco"
A <- df1 %>% filter(Valores=="Inversiones en valores") 
C <- df1 %>% filter(Valores=="Derivados")
C <- C[1,]
D <- df1 %>% filter(Valores=="Cartera de cr�dito vigente")
E <- df1 %>% filter(Valores=="    Dep�sitos de exigibilidad inmediata")
FF <- df1 %>% filter(Valores=="Capital Contable")
G <- df1 %>% filter(Valores=="    Dep�sitos a plazo")
H <- df1 %>% filter(Valores=="Gastos por intereses")
I <- df1 %>% filter(Valores=="Gastos de administraci�n y promoci�n")
Variables <- rbind(A,C,D,E,FF,G,H,I) 
obs <- Variables[,1:2]
obs <-column_to_rownames(obs, var="Valores")
obs <- obs %>% t
Obs <- cbind(banco,obs)
Obs %>% dim
names(Datos) <- colnames(Obs)
Datos[i,] <- Obs  
} # end for

# filtra bancos que no registren informaci�n completa
row_sub <- apply(Datos, 1, function(row) all(row !=0 ))
Datos <- Datos[row_sub,]

return(Datos)
} # end MesesRaros

# Aplica la funci�n para los meses mencionados
X<-"estado12.xlsx"
DatosDic<-MesesRaro(X)
DatosDic <- DatosDic %>% filter(banco!="Investa Bank")
X<-"estado6.xlsx"
DatosJun <- MesesRaro(X)
DatosJun <- DatosJun %>% filter(banco!="Investa Bank")
X<-"estado9.xlsx"
DatosSep <- MesesRaro(X)
DatosSep <- DatosSep %>% filter(banco!="Investa Bank")
X<-"estado3.xlsx"
DatosMar <- MesesRaro(X) 
DatosMar <- DatosMar %>% filter(banco!="Investa Bank")


# # # # # # # # # # # # # # # # # # # # # # # #  # #
# Funci�n : Meses(x)
#        Toma las variables que se necesitan de la base
#        
# Input: data frame
#        
# Outpu: variables que se necesitan 
#       
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

Meses <- function(df){
# limpieza y depuraci�n de la base de datos
names(df)[1] <- "Valores"
Nombre <- df %>% filter(Valores=="Balance General") 
A <- df %>% filter(Valores=="Inversiones en valores") 
C <- df %>% filter(Valores=="Derivados")
C <- C[1,]
D <- df %>% filter(Valores=="Cartera de cr�dito vigente")
E <- df %>% filter(Valores=="    Dep�sitos de exigibilidad inmediata")
FF <- df %>% filter(Valores=="Capital Contable")
G <- df %>% filter(Valores=="    Dep�sitos a plazo")
H <- df %>% filter(Valores=="Gastos por intereses")
I <- df %>% filter(Valores=="Gastos de administraci�n y promoci�n")
Variables <- rbind(Nombre, A,C,D,E,FF,G,H,I) 
colnames(Variables) <- Variables[1,]
Variables <- Variables[-1,]
Variables <- Variables %>% t 
colnames(Variables) <- Variables[1,]
Variables <- Variables[-1,] 
Variables <- Variables[c("Banamex","BBVA Bancomer","Santander","HSBC","Banco del Baj�o","Inbursa","Banca Mifel",
 "Scotiabank","Banregio","Invex","Afirme" ,"Banorte", "Bank of America" ,            
 "Bank of Tokyo-Mitsubishi UFJ","Monex"),]
Variables <- rownames_to_column(as.data.frame(Variables), var="banco")
return(Variables)
} # end meses

# Carga los datos
Enero <- read.xlsx(xlsxFile = "estado1.xlsx",sheet = 30)
Febrero <- read.xlsx(xlsxFile = "estado2.xlsx",sheet = 30)
Abril <- read.xlsx(xlsxFile = "estado4.xlsx",sheet = 30)
Mayo <- read.xlsx(xlsxFile = "estado5.xlsx",sheet = 30)
Julio <- read.xlsx(xlsxFile = "estado7.xlsx",sheet = 30)
Agosto <- read.xlsx(xlsxFile = "estado8.xlsx",sheet = 30)
Octubre <- read.xlsx(xlsxFile = "estado10.xlsx",sheet = 30)
Noviembre <- read.xlsx(xlsxFile = "estado11.xlsx",sheet = 30)
# Aplica la funci�n para los meses restantes
DatosEne <- Meses(Enero)
DatosFeb <- Meses(Febrero)
DatosAbr <- Meses(Abril)
DatosMay <- Meses(Mayo)
DatosJul <- Meses(Julio)
DatosAgo <- Meses(Agosto)
DatosOct <- Meses(Octubre)
DatosNov <- Meses(Noviembre)

# Retira algunos elementos
DatosMar<- DatosMar[-((dim(DatosMar)[1]-1):dim(DatosMar)[1]),] 
DatosMar %>% dim
DatosDic<- DatosDic[-((dim(DatosDic)[1]-1):dim(DatosDic)[1]),] 
DatosDic %>% dim
DatosJun<- DatosJun[-(dim(DatosJun)[1]),] 
DatosSep<- DatosSep[-(dim(DatosSep)[1]),] 
DatosSep%>% dim
DatosSep<- DatosSep[-(dim(DatosSep)[1]),] 
DatosSep%>% dim
# revisa estructra balanceada en los datos
DatosEne %>% dim
DatosFeb %>% dim
DatosAbr %>% dim
DatosMay %>% dim
DatosJul %>% dim
DatosAgo %>% dim
DatosOct %>% dim
DatosNov %>% dim

# genera variable que indique el mes
DatosEne <-  DatosEne %>% mutate(mes=1)
DatosFeb <- DatosFeb %>% mutate(mes=2)
DatosMar<- DatosMar %>% mutate(mes=3)
DatosAbr <- DatosAbr %>% mutate(mes=4)
DatosMay <- DatosMay %>% mutate(mes=5)
DatosJun <- DatosJun %>% mutate(mes=6)
DatosJul <- DatosJul %>% mutate(mes=7)
DatosAgo <- DatosAgo %>% mutate(mes=8)
DatosSep <- DatosSep %>% mutate(mes=9)
DatosOct <- DatosOct %>% mutate(mes=10)
DatosNov <- DatosNov %>% mutate(mes=11)
DatosDic<- DatosDic %>% mutate(mes=12)

# genera variable que indique el banco
DatosEne <-  DatosEne %>% mutate(id=1:15)
DatosFeb <- DatosFeb %>% mutate(id=1:15)
DatosMar<- DatosMar %>% mutate(id=1:15)
DatosAbr <- DatosAbr %>% mutate(id=1:15)
DatosMay <- DatosMay %>% mutate(id=1:15)
DatosJun <- DatosJun %>% mutate(id=1:15)
DatosJul <- DatosJul %>% mutate(id=1:15)
DatosAgo <- DatosAgo %>% mutate(id=1:15)
DatosSep <- DatosSep %>% mutate(id=1:15)
DatosOct <- DatosOct %>% mutate(id=1:15)
DatosNov <- DatosNov %>% mutate(id=1:15)
DatosDic<- DatosDic %>% mutate(id=1:15)

# Suma de los indicadores
DatosEne[,2:11] <-  apply(DatosEne[,2:11] , 2, as.numeric)
DatosFeb[,2:11] <-  apply(DatosFeb[,2:11] , 2, as.numeric)
DatosMar[,2:11] <-  apply(DatosMar[,2:11] , 2, as.numeric)
DatosAbr[,2:11] <-  apply(DatosAbr[,2:11] , 2, as.numeric)
DatosMay[,2:11] <-  apply(DatosMay[,2:11] , 2, as.numeric)
DatosJun[,2:11] <-  apply(DatosJun[,2:11] , 2, as.numeric)
DatosJul[,2:11] <-  apply(DatosJul[,2:11] , 2, as.numeric)
DatosAgo[,2:11] <-  apply(DatosAgo[,2:11] , 2, as.numeric)
DatosSep[,2:11] <-  apply(DatosSep[,2:11] , 2, as.numeric)
DatosOct[,2:11] <-  apply(DatosOct[,2:11] , 2, as.numeric)
DatosNov[,2:11] <-  apply(DatosNov[,2:11] , 2, as.numeric)
DatosDic[,2:11] <-  apply(DatosDic[,2:11] , 2, as.numeric)

# calcula variables que se necesitan
DatosEne <-  DatosEne %>% mutate(depTot=DatosEne$`    Dep�sitos de exigibilidad inmediata`+DatosEne$`    Dep�sitos a plazo`)
DatosEne <-  DatosEne %>% mutate(cTot=DatosEne$`Gastos por intereses`+DatosEne$`Gastos de administraci�n y promoci�n`)
DatosEne <-  DatosEne %>% mutate(cartVigeOtros=DatosEne$`Inversiones en valores`+DatosEne$Derivados+DatosEne$`Cartera de cr�dito vigente`)


DatosFeb <-  DatosFeb %>% mutate(depTot=DatosFeb$`    Dep�sitos de exigibilidad inmediata`+DatosFeb$`    Dep�sitos a plazo`)
DatosFeb <-  DatosFeb %>% mutate(cTot=DatosFeb$`Gastos por intereses`+DatosFeb$`Gastos de administraci�n y promoci�n`)
DatosFeb <-  DatosFeb %>% mutate(cartVigeOtros=DatosFeb$`Inversiones en valores`+DatosFeb$Derivados+DatosFeb$`Cartera de cr�dito vigente`)

DatosMar <-  DatosMar %>% mutate(depTot=DatosMar$`    Dep�sitos de exigibilidad inmediata`+DatosMar$`    Dep�sitos a plazo`)
DatosMar <-  DatosMar %>% mutate(cTot=DatosMar$`Gastos por intereses`+DatosMar$`Gastos de administraci�n y promoci�n`)
DatosMar <-  DatosMar %>% mutate(cartVigeOtros=DatosMar$`Inversiones en valores`+DatosMar$Derivados+DatosMar$`Cartera de cr�dito vigente`)
DatosMar[13,1] <- "Bank of America"
DatosMar[14,1] <- "Bank of Tokyo-Mitsubishi UFJ"
DatosMar[5,1] <- "Banco del Baj�o"

DatosAbr <-  DatosAbr %>% mutate(depTot=DatosAbr$`    Dep�sitos de exigibilidad inmediata`+DatosAbr$`    Dep�sitos a plazo`)
DatosAbr <-  DatosAbr %>% mutate(cTot=DatosAbr$`Gastos por intereses`+DatosAbr$`Gastos de administraci�n y promoci�n`)
DatosAbr <-  DatosAbr %>% mutate(cartVigeOtros=DatosAbr$`Inversiones en valores`+DatosAbr$Derivados+DatosAbr$`Cartera de cr�dito vigente`)

DatosMay <-  DatosMay %>% mutate(depTot=DatosMay$`    Dep�sitos de exigibilidad inmediata`+DatosMay$`    Dep�sitos a plazo`)
DatosMay <-  DatosMay %>% mutate(cTot=DatosMay$`Gastos por intereses`+DatosMay$`Gastos de administraci�n y promoci�n`)
DatosMay <-  DatosMay %>% mutate(cartVigeOtros=DatosMay$`Inversiones en valores`+DatosMay$Derivados+DatosMay$`Cartera de cr�dito vigente`)

DatosJun <-  DatosJun %>% mutate(depTot=DatosJun$`    Dep�sitos de exigibilidad inmediata`+DatosJun$`    Dep�sitos a plazo`)
DatosJun <-  DatosJun %>% mutate(cTot=DatosJun$`Gastos por intereses`+DatosJun$`Gastos de administraci�n y promoci�n`)
DatosJun <-  DatosJun %>% mutate(cartVigeOtros=DatosJun$`Inversiones en valores`+DatosJun$Derivados+DatosJun$`Cartera de cr�dito vigente`)
DatosJun[13,1] <- "Bank of America"
DatosJun[14,1] <-"Bank of Tokyo-Mitsubishi UFJ"
DatosJun[5,1] <- "Banco del Baj�o"

DatosJul <-  DatosJul %>% mutate(depTot=DatosJul$`    Dep�sitos de exigibilidad inmediata`+DatosJul$`    Dep�sitos a plazo`)
DatosJul <-  DatosJul %>% mutate(cTot=DatosJul$`Gastos por intereses`+DatosJul$`Gastos de administraci�n y promoci�n`)
DatosJul <-  DatosJul %>% mutate(cartVigeOtros=DatosJul$`Inversiones en valores`+DatosJul$Derivados+DatosJul$`Cartera de cr�dito vigente`)


DatosAgo <-  DatosAgo %>% mutate(depTot=DatosAgo$`    Dep�sitos de exigibilidad inmediata`+DatosAgo$`    Dep�sitos a plazo`)
DatosAgo <-  DatosAgo %>% mutate(cTot=DatosAgo$`Gastos por intereses`+DatosAgo$`Gastos de administraci�n y promoci�n`)
DatosAgo <-  DatosAgo %>% mutate(cartVigeOtros=DatosAgo$`Inversiones en valores`+DatosAgo$Derivados+DatosAgo$`Cartera de cr�dito vigente`)

DatosSep <-  DatosSep %>% mutate(depTot=DatosSep$`    Dep�sitos de exigibilidad inmediata`+DatosSep$`    Dep�sitos a plazo`)
DatosSep <-  DatosSep %>% mutate(cTot=DatosSep$`Gastos por intereses`+DatosSep$`Gastos de administraci�n y promoci�n`)
DatosSep <-  DatosSep %>% mutate(cartVigeOtros=DatosSep$`Inversiones en valores`+DatosSep$Derivados+DatosSep$`Cartera de cr�dito vigente`)
DatosSep[13,1] <- "Bank of America"
DatosSep[14,1] <- "Bank of Tokyo-Mitsubishi UFJ"
DatosSep[5,1] <- "Banco del Baj�o"

DatosOct <-  DatosOct %>% mutate(depTot=DatosOct$`    Dep�sitos de exigibilidad inmediata`+DatosOct$`    Dep�sitos a plazo`)
DatosOct <-  DatosOct %>% mutate(cTot=DatosOct$`Gastos por intereses`+DatosOct$`Gastos de administraci�n y promoci�n`)
DatosOct <-  DatosOct %>% mutate(cartVigeOtros=DatosOct$`Inversiones en valores`+DatosOct$Derivados+DatosOct$`Cartera de cr�dito vigente`)

DatosNov <-  DatosNov %>% mutate(depTot=DatosNov$`    Dep�sitos de exigibilidad inmediata`+DatosNov$`    Dep�sitos a plazo`)
DatosNov <-  DatosNov %>% mutate(cTot=DatosNov$`Gastos por intereses`+DatosNov$`Gastos de administraci�n y promoci�n`)
DatosNov <-  DatosNov %>% mutate(cartVigeOtros=DatosNov$`Inversiones en valores`+DatosNov$Derivados+DatosNov$`Cartera de cr�dito vigente`)

DatosDic <-  DatosDic %>% mutate(depTot=DatosDic$`    Dep�sitos de exigibilidad inmediata`+DatosDic$`    Dep�sitos a plazo`)
DatosDic <-  DatosDic %>% mutate(cTot=DatosDic$`Gastos por intereses`+DatosDic$`Gastos de administraci�n y promoci�n`)
DatosDic <-  DatosDic %>% mutate(cartVigeOtros=DatosDic$`Inversiones en valores`+DatosDic$Derivados+DatosDic$`Cartera de cr�dito vigente`)
DatosDic[13,1] <- "Bank of America"
DatosDic[14,1] <- "Bank of Tokyo-Mitsubishi UFJ"
DatosDic[5,1] <- "Banco del Baj�o"

# Guarda las bases por mes en xlsx
write.xlsx(DatosEne, "DatosEne.xlsx")
write.xlsx(DatosFeb, "DatosFeb.xlsx")
write.xlsx(DatosMar, "DatosMar.xlsx")
write.xlsx(DatosAbr, "DatosAbr.xlsx")
write.xlsx(DatosMay, "DatosMay.xlsx")
write.xlsx(DatosJun, "DatosJun.xlsx")
write.xlsx(DatosJul, "DatosJul.xlsx")
write.xlsx(DatosAgo, "DatosAgo.xlsx")
write.xlsx(DatosSep, "DatosSep.xlsx")
write.xlsx(DatosOct, "DatosOct.xlsx")
write.xlsx(DatosNov, "DatosNov.xlsx")
write.xlsx(DatosDic, "DatosDic.xlsx")

# junta todos los meses
OBSERVACIONES <- rbind(DatosEne,DatosFeb,DatosMar,DatosAbr,DatosMay,
                       DatosJun,DatosJul,DatosAgo,DatosSep,DatosOct,
                       DatosNov,DatosDic)

# guarda en xlsx la base de los meses
write.xlsx(OBSERVACIONES, "OBSERVACIONES.xlsx")


# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #        PARTE III: microbench   ## # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# Abra el archivo de la base completa
OBSERVACIONES <- openxlsx::read.xlsx("OBSERVACIONES.xlsx")
# Utilice estas etiquetas para el nombre de las variables
names(OBSERVACIONES) <- c("banco", "Inversiones en valores","Derivados",                           
                          "Cartera de cr�dito vigente", "Dep�sitos de exigibilidad inmediata", 
                          "Capital Contable","Dep�sitos a plazo","Gastos por intereses" ,               
                          "Gastos de administraci�n y promoci�n","mes","id","depTot",                              
                          "cTot" ,"cartVigeOtros")

OBSERVACIONES %>% names
# Genere un ajuste en una de las variables 
OBSERVACIONES <- OBSERVACIONES %>% mutate(otrosAct = cartVigeOtros-`Cartera de cr�dito vigente`)

# determine las variables entrada y salida para el modelo  
inpvar <- OBSERVACIONES %>% select(depTot, `Capital Contable`,cTot)
outvar <-OBSERVACIONES %>% select(`Cartera de cr�dito vigente`,otrosAct)

# microbench
set.seed(1)
Contrastar <- microbenchmark(
  rDea = rDEA::dea(XREF = inpvar, YREF = outvar, X = inpvar[,], Y=outvar[,],model = "input",RTS = "variable"),
  benchmarking = Benchmarking::dea(X=inpvar[,], Y=outvar[,], RTS="vrs", ORIENTATION="in", XREF=inpvar, YREF=outvar,
                                   FRONT.IDX=NULL, SLACK=FALSE, DUAL=FALSE, DIRECT=NULL, param=NULL,
                                   TRANSPOSE=FALSE, FAST=FALSE, LP=FALSE, CONTROL=NULL, LPK=NULL),
  times = 100
)

# Resultados 

#Unit: milliseconds
#expr      min       lq     mean   median
#rDea 31.58484 32.25023 33.99213 32.89023
#benchmarking 34.66964 35.38068 38.27882 36.22953
#uq      max neval cld
#34.09919 58.25235   100  a 
#37.55476 75.72477   100   b



# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #        PARTE III: DEA     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 

# Abra el archivo de la base completa
OBSERVACIONES <- openxlsx::read.xlsx("OBSERVACIONES.xlsx")
# Utilice estas etiquetas para el nombre de las variables
names(OBSERVACIONES) <- c("banco", "Inversiones en valores","Derivados",                           
                          "Cartera de cr�dito vigente", "Dep�sitos de exigibilidad inmediata", 
                          "Capital Contable","Dep�sitos a plazo","Gastos por intereses" ,               
                          "Gastos de administraci�n y promoci�n","mes","id","depTot",                              
                          "cTot" ,"cartVigeOtros")

OBSERVACIONES %>% names
# Genere un ajuste en una de las variables 
OBSERVACIONES <- OBSERVACIONES %>% mutate(otrosAct = cartVigeOtros-`Cartera de cr�dito vigente`)

# determine las variables entrada y salida para el modelo  
inpvar <- OBSERVACIONES %>% select(depTot, `Capital Contable`,cTot)
outvar <-OBSERVACIONES %>% select(`Cartera de cr�dito vigente`,otrosAct)

# modelo DEA CCR con retornos a escala constantes
model_constante <- dea(XREF = inpvar, YREF = outvar, X = inpvar[,], Y=outvar[,],model = "input",RTS = "constant")

# vusualiza tabla de resultados
result <- cbind(round(model_constante$thetaOpt,4), round(model_constante$lambda,4))
rownames(result) <- OBSERVACIONES$banco
colnames(result) <- c("Eficiencia", rownames(result))
kable(result[,]) %>% kable_styling()
Res <- result %>% as.data.frame
Res2 <- cbind(OBSERVACIONES$banco,Res$Eficiencia,OBSERVACIONES$id, OBSERVACIONES$mes)
Res2 <- Res2 %>% as.data.frame()
names(Res2) <- c("Banco","Eficiencia", "id", "mes")
Res2$Banco %>% levels

# Guarde resultados en xlsx
write.xlsx(as.data.frame(result), "result.xlsx")
write.xlsx(Res2, "Res2.xlsx")

# visualizaci�n resultados
Res2$mes <- Res2$mes %>% as.character %>% as.numeric()
Res2$id <- Res2$id %>% as.character %>% as.numeric()
Res2$Banco %>% class
Res2$Eficiencia <- Res2$Eficiencia %>% as.character %>% as.numeric()
X11()
p1 <-  ggplot(Res2, aes(x = mes, y = Eficiencia, color = Banco, group = id,  shape = Banco))+
  geom_point() +  
  scale_shape_manual(values=1:nlevels(Res2$Banco)) + geom_line() + scale_x_discrete(limits = c(1, 12)) +
  labs(x="Mes", title = "Eficiencia t�cnica pura; Asumiendo rendimientos constantes", subtitle = "Asumiendo rendimientos constantes")
plotly::ggplotly(p1)  
# Guarde gr�fico en html
htmlwidgets::saveWidget(as_widget(ggplotly(p1)), "Eficiencia_t�cnica_pura_RST_CONSTANTE.html")



# modelo DEA CCR con retornos a escala variable
model_variable <- dea(XREF = inpvar, YREF = outvar, X = inpvar[,], Y=outvar[,],model = "input",RTS = "variable")

# vusualiza tabla de resultados
result2 <- cbind(round(model_variable$thetaOpt,4), round(model_variable$lambda,4))
rownames(result2) <- OBSERVACIONES$banco
colnames(result2) <- c("Eficiencia", rownames(result2))
kable(result2[,]) %>% kable_styling()
Res_variable <- result2 %>% as.data.frame
Res2_variable <- cbind(OBSERVACIONES$banco,Res_variable$Eficiencia,OBSERVACIONES$id, OBSERVACIONES$mes)
Res2_variable <- Res2_variable %>% as.data.frame()
names(Res2_variable) <- c("Banco","Eficiencia", "id", "mes")
Res2_variable$Banco %>% levels

# Guarde resultados en xlsx
write.xlsx(as.data.frame(result2), "result2.xlsx")
write.xlsx(Res2_variable, "Res2_variable.xlsx")

# visualizaci�n resultados
Res2_variable$mes <- Res2_variable$mes %>% as.character %>% as.numeric()
Res2_variable$id <- Res2_variable$id %>% as.character %>% as.numeric()
Res2_variable$Eficiencia <- Res2_variable$Eficiencia %>% as.character %>% as.numeric()
X11()
p2 <-  ggplot(Res2_variable, aes(x = mes, y = Eficiencia, color = Banco, group = id,  shape = Banco))+
  geom_point() +  
  scale_shape_manual(values=1:nlevels(Res2_variable$Banco)) + geom_line() + scale_x_discrete(limits = c(1, 12)) +
  labs(x="Mes", title = "Eficiencia t�cnica pura; Asumiendo rendimientos variables", subtitle = "Asumiendo rendimientos variables")
plotly::ggplotly(p2)  
# Guarde resultados en html
htmlwidgets::saveWidget(as_widget(ggplotly(p2)), "Eficiencia_t�cnica_pura_RST_VARIABLE.html")

# Calculaar la eficiencia a escala
ETP_crs <- Res2$Eficiencia %>% as.character %>% as.numeric
ETP_vrs <- Res2_variable$Eficiencia %>% as.character %>% as.numeric
EES <- ETP_crs/ ETP_vrs %>% as.data.frame
RES3 <- cbind(Res2$Banco,EES,Res2$id,Res2$mes)
names(RES3) <- c("Banco","Eficiencia","id","mes")

# vusualiza tabla de resultados
X11()
p3 <-  ggplot(RES3, aes(x = mes, y = Eficiencia, color = Banco, group = id,  shape = Banco))+
  geom_point() +  
  scale_shape_manual(values=1:nlevels(RES3$Banco)) + geom_line() + scale_x_discrete(limits = c(1, 12)) +
  labs(x="Mes", title = "Eficiencia a escala")
plotly::ggplotly(p3)  
# Guarde resultados en html
htmlwidgets::saveWidget(as_widget(ggplotly(p3)), "Eficiencia_escala.html")

# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # #     FIN DEL DOCUMENTO     ## # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 
# # # # # # # # # # # # # #  # # # # # # # # # # # # 