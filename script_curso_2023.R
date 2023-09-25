########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

###################################################
###################################################
###################################################
# CURSO DE ANALISIS ESTADISTICO CON R
###################################################
###################################################
###################################################

# R version 4.1.0 (2021-05-18)

rm(list=ls())
gc()

ls()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_FI_2023/datos/")

load("datos.curso1.RData")

ls()

class(datos)

dim(datos)
str(datos)

###################################################
# ESTADISTICA DESCRIPTIVA
###################################################


# Variable cuantitativa

datos$"peso"

datos[   , c(6) ]

datos[,6]

class(datos$"peso")

datos$"peso" <- as.numeric(datos$"peso")

class(datos$"peso")


# datos$"edad"<-as.character(datos$"edad")

mean(datos$"peso")
median(datos$"peso")
exp(mean(log(datos$"peso")))

quantile(datos$"peso",prob=seq(0,1,1/4))

quantile(datos$"peso",prob=seq(0,1,1/4),type=2)


datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso",prob=seq(0,1,1/3)),
right=TRUE, include.lowest=TRUE)

table(datos$"peso.gr",exclude=NULL)


sd(datos$"peso")

var(datos$"peso")

IQR(datos$"peso")


range(datos$"peso")

min(datos$"peso")

max(datos$"peso")


hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

boxplot(datos$"peso")

qqnorm(datos$"peso")


summary(datos)


# install.packages("crosstable")
# install.packages("knitr")

library(crosstable)
library(knitr)

res<-crosstable(datos, c(peso,altura))
kable(res)



tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
"coeficiente de variacion","desviación típica","minimo","maximo"),
Edad=NA,Peso=NA,Altura=NA,
stringsAsFactors=FALSE)


tabla[,"Edad"]<-c(mean(datos$"edad"), median(datos$"edad"),round(exp(mean(log(datos$"edad"))),digits=2),
paste(round(quantile(datos$"edad"),digits=2),collapse=";"),
c(sd(datos$"edad")/mean(datos$"edad"))*100,
sd(datos$"edad"),
range(datos$"edad")[1],range(datos$"edad")[2])



tabla[,"Peso"]<-c(mean(datos$"peso"), median(datos$"peso"),exp(mean(log(datos$"peso"))),
paste(round(quantile(datos$"peso"),digits=2),collapse=";"),
c(sd(datos$"peso")/mean(datos$"peso"))*100,
sd(datos$"peso"),
range(datos$"peso")[1],range(datos$"peso")[2])



tabla[,"Altura"]<-c(mean(datos$"altura"), median(datos$"altura"),
exp(mean(log(datos$"altura"))),
paste(round(quantile(datos$"altura"),digits=2),collapse=";"),
c(sd(datos$"altura")/mean(datos$altura))*100,
sd(datos$altura),
range(datos$altura)[1],range(datos$altura)[2])


library("openxlsx")
write.xlsx(tabla,file="/Users/pfernandezn/Desktop/tabla_descriptiva_curso.xlsx")



tapply(datos$"peso",datos$"estado.civil",mean)

library(crosstable)
library(knitr)
res<-crosstable(datos, c(peso,edad), by=sexo, num_digits = 3,total="both")
kable(res)


par(mfrow=c(1,2))
hist(datos$"edad"[datos$"sexo"%in%"Hombre"],
main="Edad Hombres",xlab="Peso (Kg)")
hist(datos$"edad"[datos$"sexo"%in%"Mujer"],
main="Edad Mujeres",xlab="Peso (Kg)")


par(mfrow=c(1,2))
qqnorm(datos$"edad"[datos$"sexo"%in%"Hombre"],main="QQ plot (Hombres)")
qqnorm(datos$"edad"[datos$"sexo"%in%"Mujer"],main="QQ plot (Mujeres)")



# Variables cualitativas

table(datos$"estado.civil",exclude=NULL)

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100



table(datos$"sexo",exclude=NULL)

margin.table(table(datos$"sexo",exclude=NULL))

prop.table(table(datos$"sexo",exclude=NULL))*100



barplot(table(datos$"estado.civil"),ylim=c(0,90),xlab="Estado Civil",ylab="Frecuencia Absoluta (N)")


pie(table(datos$"estado.civil"),labels=paste(c("Casado","Divorciado","Soltero"), 
sep = " ", table(datos$"estado.civil")))

library("plotrix")
slices <- table(datos$"estado.civil")
lbls <- c("Casado","Divorciado","Soltero")
pie3D(slices,labels=lbls,explode=0.1,
   main="Pie Chart Estado civil")







