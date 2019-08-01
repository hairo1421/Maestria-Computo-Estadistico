# librerias
library(tidyverse)

setwd("C:/Users/angel/Desktop/MCE_CIMAT/Segundo_Semestre/Estadistica_Multivariada/ProyectoFinal/baseballreference")
## 2018
dat81<-read.csv("2018bateost.csv")
dat82<-read.csv("2018bateov.csv")
dat83<-read.csv("2018bateow.csv")
dat84<-read.csv("2018picheost.csv")
dat85<-read.csv("2018picheov.csv")
dat86<-read.csv("2018picheow.csv")
dat87<-read.csv("2018campost.csv")
dat88<-read.csv("2018campoa.csv")
## 2017
dat71<-read.csv("2017bateost.csv")
dat72<-read.csv("2017bateov.csv")
dat73<-read.csv("2017bateow.csv")
dat74<-read.csv("2017picheost.csv")
dat75<-read.csv("2017picheov.csv")
dat76<-read.csv("2017picheow.csv")
dat77<-read.csv("2017campost.csv")
dat78<-read.csv("2017campoa.csv")
## 2016
dat61<-read.csv("2016bateost.csv")
dat62<-read.csv("2016bateov.csv")
dat63<-read.csv("2016bateow.csv")
dat64<-read.csv("2016picheost.csv")
dat65<-read.csv("2016picheov.csv")
dat66<-read.csv("2016picheow.csv")
dat67<-read.csv("2016campost.csv")
dat68<-read.csv("2016campoa.csv")

## Agregar anio
dat81<- mutate(dat81,year="2018")
dat82<- mutate(dat82,year="2018")
dat83<- mutate(dat83,year="2018")
dat84<- mutate(dat84,year="2018")
dat85<- mutate(dat85,year="2018")
dat86<- mutate(dat86,year="2018")
dat87<- mutate(dat87,year="2018")
dat88<- mutate(dat88,year="2018")

dat71<- mutate(dat71,year="2017")
dat72<- mutate(dat72,year="2017")
dat73<- mutate(dat73,year="2017")
dat74<- mutate(dat74,year="2017")
dat75<- mutate(dat75,year="2017")
dat76<- mutate(dat76,year="2017")
dat77<- mutate(dat77,year="2017")
dat78<- mutate(dat78,year="2017")

dat61<- mutate(dat61,year="2016")
dat62<- mutate(dat62,year="2016")
dat63<- mutate(dat63,year="2016")
dat64<- mutate(dat64,year="2016")
dat65<- mutate(dat65,year="2016")
dat66<- mutate(dat66,year="2016")
dat67<- mutate(dat67,year="2016")
dat68<- mutate(dat68,year="2016")

### Agregar ID


dat81<- separate(dat81,Name,sep="\\\\" , into=c("Name","Id"))
dat82<- separate(dat82,Name,sep="\\\\" , into=c("Name","Id"))
dat83<- separate(dat83,Name,sep="\\\\" , into=c("Name","Id"))
dat84<- separate(dat84,Name,sep="\\\\" , into=c("Name","Id"))
dat85<- separate(dat85,Name,sep="\\\\" , into=c("Name","Id"))
dat86<- separate(dat86,Name,sep="\\\\" , into=c("Name","Id"))
dat87<- separate(dat87,Name,sep="\\\\" , into=c("Name","Id"))
dat88<- separate(dat88,Name,sep="\\\\" , into=c("Name","Id"))

dat71<- separate(dat71,Name,sep="\\\\" , into=c("Name","Id"))
dat72<- separate(dat72,Name,sep="\\\\" , into=c("Name","Id"))
dat73<- separate(dat73,Name,sep="\\\\" , into=c("Name","Id"))
dat74<- separate(dat74,Name,sep="\\\\" , into=c("Name","Id"))
dat75<- separate(dat75,Name,sep="\\\\" , into=c("Name","Id"))
dat76<- separate(dat76,Name,sep="\\\\" , into=c("Name","Id"))
dat77<- separate(dat77,Name,sep="\\\\" , into=c("Name","Id"))
dat78<- separate(dat78,Name,sep="\\\\" , into=c("Name","Id"))


dat61<- separate(dat61,Name,sep="\\\\" , into=c("Name","Id"))
dat62<- separate(dat62,Name,sep="\\\\" , into=c("Name","Id"))
dat63<- separate(dat63,Name,sep="\\\\" , into=c("Name","Id"))
dat64<- separate(dat64,Name,sep="\\\\" , into=c("Name","Id"))
dat65<- separate(dat65,Name,sep="\\\\" , into=c("Name","Id"))
dat66<- separate(dat66,Name,sep="\\\\" , into=c("Name","Id"))
dat67<- separate(dat67,Name,sep="\\\\" , into=c("Name","Id"))
dat68<- separate(dat68,Name,sep="\\\\" , into=c("Name","Id"))

#### Compactacion
# bateo
dat_bateo_st<-rbind(dat81,dat71,dat61)
dat_bateo_val<-rbind(dat82,dat72,dat62)
dat_bateo_win<-rbind(dat83,dat73,dat63)

# picheo
dat_picheo_st<-rbind(dat84,dat74,dat64)
dat_picheo_val<-rbind(dat85,dat75,dat65)
dat_picheo_win<-rbind(dat86,dat76,dat66)

# campo
dat_campo_st<-rbind(dat87,dat77,dat67)
dat_campo_a<-rbind(dat88,dat78,dat68)

### Imporatacion de tablas
write.csv(dat_bateo_st,file="dat_bateo_st.csv",row.names = FALSE)
write.csv(dat_bateo_val,file="dat_bateo_val.csv",row.names = FALSE)
write.csv(dat_bateo_win,file="dat_bateo_win.csv",row.names = FALSE)

write.csv(dat_picheo_st,file="dat_picheo_st.csv",row.names = FALSE)
write.csv(dat_picheo_val,file="dat_picheo_val.csv",row.names = FALSE)
write.csv(dat_picheo_win,file="dat_picheo_win.csv",row.names = FALSE)

write.csv(dat_campo_st,file="dat_campo_st.csv",row.names = FALSE)
write.csv(dat_campo_a,file="dat_campo_a.csv",row.names = FALSE)

########## Relectura de tablas
dat_bateo_st<- read.csv("dat_bateo_st.csv")
dat_bateo_val<- read.csv("dat_bateo_val.csv")
dat_bateo_win<- read.csv("dat_bateo_win.csv")

dat_picheo_st<- read.csv("dat_picheo_st.csv")
dat_picheo_val<- read.csv("dat_picheo_val.csv")
dat_picheo_win<- read.csv("dat_picheo_win.csv")

dat_campo_a<- read.csv("dat_campo_a.csv")
dat_campo_st<- read.csv("dat_campo_st.csv")


## Creacion tabla unica de bateo
exp<-left_join(x=dat_bateo_st,y=dat_bateo_val,by=c("Id","year","Name","Age"),suffix=c(".st",".val"))
dat_bateo1<-exp
exp<- left_join(x=dat_bateo1,y=dat_bateo_win,by=c("Id","year","Name","Age"),suffix=c(".st_val",".win"))
dat_bateo<-exp
write.csv(dat_bateo,file="dat_bateo.csv",row.names = FALSE)
## Creacion tabla unica de picheo
exp<-left_join(x=dat_picheo_st,y=dat_picheo_val,by=c("Id","year","Name","Age"),suffix=c(".st",".val"))
dat_picheo1<-exp
exp<- left_join(x=dat_picheo1,y=dat_picheo_win,by=c("Id","year","Name","Age"),suffix=c(".st_val",".win"))
dat_picheo<-exp
write.csv(dat_picheo,file="dat_picheo.csv",row.names = FALSE)

## Creacion tabla unica de campo
exp<-left_join(x=dat_campo_st,y=dat_campo_a,by=c("Id","year","Name","Age"),suffix=c(".st",".a"))
dat_campo<-exp
write.csv(dat_campo,file="dat_campo.csv",row.names = FALSE)

### Releectura de tablas finales
dat_bateo<-read.csv("dat_bateo.csv")
dat_picheo<-read.csv("dat_picheo.csv")
dat_campo<-read.csv("dat_campo.csv")


# Buscar un jugador
idJ<- "reedco01"
jug_bateo<- dat_bateo_Roster[which(dat_bateo$Id==idJ),]
jug_bateo2<- dat_bateo_RL[which(dat_bateo$Id==idJ),]
jug_campo<- dat_campo[which(dat_campo$Id==idJ),]
jug_picheo<- dat_picheo[which(dat_picheo$Id==idJ),]

### Añadir si el jugador es derecho o izquierdo
setwd("C:/Users/angel/Desktop/MCE_CIMAT/Segundo_Semestre/Estadistica_Multivariada/ProyectoFinal/")

playerid<- read.csv("PLAYERIDMAP.csv")

indicesPl<- match(dat_bateo$Id,playerid$IDPLAYER)
indicesRost<- playerid$RETROID[indicesPl]
mrost<- match(as.character(indicesRost),as.character(roster$Player.ID))
varRost<- roster$Bats[mrost]
var<- playerid[indicesPl,c("BATS")]
dat_bateo_RL<- cbind(dat_bateo,BATS=var)
dat_bateo_Roster<- cbind(dat_bateo,BATS=varRost)
indicesPl2<- match(dat_picheo$Id,playerid$IDPLAYER)
var2<- playerid[indicesPl2,c("THROWS")]
dat_picheo_RL<- cbind(dat_picheo,THROWS=var2)

write.csv(dat_picheo_RL,file="dat_picheo_RL.csv",row.names = FALSE)
write.csv(dat_bateo_RL,file="dat_bateo_RL.csv",row.names = FALSE)



