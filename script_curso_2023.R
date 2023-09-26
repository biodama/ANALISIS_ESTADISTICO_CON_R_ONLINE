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


# Variable cualitativa agrupada

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100

# library("gmodels")

# install.packages("gmodels")
gmodels::CrossTable(datos$"estado.civil",datos$"sexo",
prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)



# Sonido
library("beepr")
beep(sound = 3, expr = NULL)

#library("music")
#playProgression(buildProgression("G4", "minor"))


tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
"coeficiente de variacion","desviación típica","maximo","minimo"),
Edad=NA,Peso=NA,Altura=NA,
stringsAsFactors=FALSE)

tabla[,"Edad"]<-c(mean(datos$edad), median(datos$edad),exp(mean(log(datos$edad))),
paste(quantile(datos$edad),collapse=";"),(sd(datos$edad)/mean(datos$edad))*100,
sd(datos$edad),range(datos$edad)[1],
range(datos$edad)[2])

tabla[,"Peso"]<-c(mean(datos$peso), median(datos$peso),exp(mean(log(datos$peso))),
paste(quantile(datos$peso),collapse=";"),(sd(datos$peso)/mean(datos$peso))*100,
sd(datos$peso),range(datos$peso)[1],
range(datos$peso)[2])

tabla[,"Altura"]<-c(mean(datos$altura), median(datos$altura),
exp(mean(log(datos$altura))),paste(quantile(datos$altura),collapse=";"),
c(sd(datos$altura)/mean(datos$altura))*100,sd(datos$altura),range(datos$altura)[1],
range(datos$altura)[2])


round(mean(datos$"peso"),digits=2)
floor(mean(datos$"peso"))
ceiling(mean(datos$"peso"))





descript<-function(x,nombre){
	
	tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
	"coeficiente de variacion","desviación típica","maximo","minimo"),
	Variable=NA,
	stringsAsFactors=FALSE)
	
	tabla[,"Variable"]<-c(mean(x), median(x),exp(mean(log(x))),paste(quantile(x),
	collapse=";"),(sd(x)/mean(x))*100,sd(x),range(x)[1],
	range(x)[2])

	names(tabla)[2]<-nombre
	
	beepr::beep(sound = 3, expr = NULL)
	
	tabla

}

descript(x=datos$"edad",nombre="edad")

mean(datos$"edad")

tapply(datos$"edad",datos$"sexo",mean)

tapply(datos$"edad",datos$"sexo",descript,nombre="edad")


#################################
#  INFERENCIA
#################################

mujeres <- datos[datos$"sexo"%in%"Mujer", ]

hombres <- datos[datos$"sexo"%in%"Hombre" , ]



res<-t.test(hombres$"peso",conf.level = 0.95)

res

names(res)

res$"conf.int"
res$"estimate"

mean_ic95<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
round(res$"conf.int"[2],2),")",sep="")
mean_ic95


HOMBRES
79.99 (79.74-80.24)

AMBOS SEXOS
69.95 (68.54-71.36)



#################################

t.test(mujeres$"peso",hombres$"peso",var.equal = TRUE)

t.test(mujeres$"peso",hombres$"peso",var.equal = FALSE)


res<-t.test(hombres$"peso",mu = 90,sd=1, conf.level = 0.95)
res


anova(lm(hombres$"peso"~hombres$"nivel.estudios"))


res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95,var.equal = TRUE)


res_alternativo<-t.test(datos$"peso" ~ datos$"sexo")






pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),

p.adj="BH")

tapply(hombres$"peso",hombres$"nivel.estudios",mean)
tapply(hombres$"peso",hombres$"nivel.estudios",shapiro.test)

hist(hombres$peso[hombres$nivel.estudios%in%"Bajo"])
length(hombres$peso[hombres$nivel.estudios%in%"Bajo"])

length(hombres$peso[hombres$nivel.estudios%in%"Alto"])

hist(hombres$peso[hombres$nivel.estudios%in%"Alto"])
hist(hombres$peso[hombres$nivel.estudios%in%"Alto"],n=100)


kruskal.test(hombres$"peso"~hombres$"nivel.estudios")


suc<-table(datos$"fumador",datos$"sexo")[2,]
tot<-margin.table(table(datos$"fumador",datos$"sexo"),2)

prop.test(suc,tot)


fumadores <- c( 83, 90, 129, 70 )
pacientes <- c( 86, 93, 136, 82 )
prop.trend.test(fumadores,pacientes)

fumadores <- c( 83, 70,129, 90)
pacientes <- c( 86, 82,136, 93)
prop.trend.test(fumadores,pacientes)
prop.test(fumadores,pacientes)


fumadores <- c( 83, 90, 129, 75 )
pacientes <- c( 86, 93, 136, 82 )
prop.trend.test(fumadores,pacientes)
prop.test(fumadores,pacientes)




# VARIABLES CUANTITATIVAS (t.test wilcox.test anova kruskal.test
	
t.test(,paired=F)

datos$"bmi" <- datos$"peso"/c(c(datos$"altura"/100)^2)

set.seed(10)
datos$"bmi.post"<- datos$"bmi" + sample(c(1:5),dim(datos)[1],replace=T)

mean(datos$"bmi")
mean(datos$"bmi.post")

t.test(datos$"bmi",datos$"bmi.post",paired=T)

t.test(datos$"bmi.post",datos$"bmi",paired=T)




# VARIABLES CUALITATIVAS (prop.test prop.trend.test)


# Comparar con un valor 0.40

prop.test(as.numeric(table(datos$"fumador"))[2] , dim(datos)[1] ,0.40,alternative="greater")


# Comparar la proporcion de fumadores entre hombres y mujeres
table(datos$sexo,datos$fumador)[,c(2,1)]
prop.table(table(datos$sexo,datos$fumador)[,c(2,1)],1)

suc<-table(datos$"fumador",datos$"sexo")[2,]
tot<-margin.table(table(datos$"fumador",datos$"sexo"),2)
prop.test(suc,tot)

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])


# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])

prop.trend.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2)],
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1))


modelo_lineal<-lm(datos$peso~datos$sexo)

summary(modelo_lineal)

# Comparacion de dos proporciones pareadas

Performance <-matrix(c(794, 86, 150, 570),
       nrow = 2,
       dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)

# Comparacion de mas de dos proporciones pareadas





# Comparacion de conteos pareados numero de brotes de un paciente antes

wilcox.test(nbantes,nbdespues,paired=T)

id  status    brotes  sexo
1    antes      0     m
1    despues    3     m
2    antes      10    h
2    despues    2     h

numero de brotes ~ efecto aletorio (id) + (antes/post) + sexo + estado_inmunoa (lineal mixto)
numero de brotes ~ efecto aletorio (id) + antes/post + sexo + estado_inmunoa (Poisson mixto)











