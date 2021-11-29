###############################################################
###############################################################
###############################################################
###############################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE-master/datos/")

load("datos.curso1.RData")

# Version Celmira

#setwd("C://Users//Celmira Laza//Dropbox//CURSO EPIDEMIOLOGIA//Curso 3//ANALISIS_ESTADISTICO_CON_R_ONLINE-master//datos//")

#load("datos.curso1.RData")

###################################################
# ANALISIS DESCRIPTIVO DE VARIABLE CUANTITATIVA
###################################################

# Estadisticos de tendencia central

mean(datos$"peso") # 

median(datos$"peso") # 

exp(mean(log(datos$"peso"))) # 

# Medidas de posicion

quantile(datos$"peso")

quantile(datos$"peso",prob=seq(0,1,1/4))

quantile(datos$"peso",prob=seq(0,1,1/3))

# Categorizacion de variables cuantitativas (funcion cut)

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso",prob=seq(0,1,1/3)),right=TRUE, include.lowest=TRUE)
table(datos$"peso.gr",exclude=NULL)

# Medidas de dispersion

sd(datos$"peso")

var(datos$"peso")

IQR(datos$"peso")

range(datos$"peso")
min(datos$"peso")
max(datos$"peso")

# Graficos

hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

boxplot(datos$"peso")

qqnorm(datos$"peso")


# Summary

summary(datos)


#################################################################
# ANALISIS DESCRIPTIVO DE VARIABLE CUANTITATIVA por grupo
#################################################################

# Una manera

datos.hombre<-datos[datos$"sexo"%in%"Hombre",]

mean(datos.hombre$"peso") # 79.991

datos.mujer<-datos[datos$"sexo"%in%"Mujer",]

mean(datos.mujer$"peso") # 59.90394


# Otra manera


tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",mean)[1]

tapply(datos$"peso",datos$"sexo",mean)[2]

# Graficos


par(mfrow=c(3,2))

hist(datos$"peso"[datos$"sexo"%in%"Hombre"],main="Peso Hombres",xlab="Peso (Kg)")

hist(datos$"peso"[datos$"sexo"%in%"Mujer"],main="Peso Mujeres",xlab="Peso (Kg)")

hist(datos$"peso",main="Peso ambos",xlab="Peso (Kg)")

boxplot(datos$"peso" ~ datos$"sexo",main="Peso",xlab="Sexo",ylab="Peso (Kg)")

qqnorm(datos$"peso"[datos$"sexo"%in%"Hombre"],main="QQ plot (Hombres)")

qqnorm(datos$"peso"[datos$"sexo"%in%"Mujer"],main="QQ plot (Mujeres)")



###################################################
# ANALISIS DESCRIPTIVO DE VARIABLE CUALITATIVA
###################################################

# Estado civil

table(datos$"estado.civil",exclude=NULL)

as.data.frame(table(datos$"estado.civil",exclude=NULL))

tapply(datos$"peso",datos$"estado.civil",mean)

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100


# Graficos

barplot(table(datos$"estado.civil"))

barplot(prop.table(table(datos$"estado.civil"))*100,ylim=c(0,50))

pie(table(datos$"estado.civil"))


#############################################################
# ANALISIS DESCRIPTIVO DE VARIABLE CUALITATIVA agrupada
#############################################################

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)
margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100
prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100


# Graficos

par(mfrow=c(2,1))

barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Hombre"]))

barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Mujer"]))














