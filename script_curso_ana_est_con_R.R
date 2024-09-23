##########################################
##########################################
##    CURSO ANALISIS ESTADISTICO CON R    ##
##########################################
##########################################

# primera posibilidad para cargar un workspace

setwd("C:/Users/sara.munoz/Desktop/datos/datos/")

load("datos.curso1.RData")
  
  
# segunda posibilidad para cargar un workspace
 
load("C:/Users/sara.munoz/Desktop/datos/datos/datos.curso1.RData") 
  
  

rm(list=ls()) # borra los objetos del workspace
gc()

ls()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE-master/datos/")

load("datos.curso1.RData")

str(datos)


# Variable cuantititiva (Estadisticos de tendencia central)

mean(c(1,2,NA,4,5),na.rm=TRUE)
?mean
sum(is.na(datos$"peso"))

class(datos$"peso")
sum(is.na(datos$"peso"))

mean(datos$"peso") # 69.94747median(datos$"peso")exp(mean(log(datos$"peso")))

# Variable cuantititiva (Medidas de posición)

quantile(datos$"peso") # por defecto calcula cuartiles

#.          0%         25%        50%         75%     100% 
#56.56025 59.98709 69.75524 80.00352 82.28081 

quantile(datos$"peso",prob=seq(from=0, to=1, by=1/3)) # terciles

quantile(datos$"peso",prob=seq(0,1,1/3)) # # terciles



quantile(datos$"peso",prob=seq(0,1,1/3))



datos$"peso.gr" <- cut(datos$"peso",
breaks=quantile(datos$"peso",prob=seq(0,1,1/3)),
right=TRUE, 
include.lowest=TRUE)

table(datos$"peso.gr",exclude=NULL)

# Variable cuantititiva (Medidas de dispersion)

sd(datos$"peso")var(datos$"peso")IQR(datos$"peso")range(datos$"peso") # mínimo y máximomin(datos$"peso")max(datos$"peso")


# Variable cuantititiva (graficos)

hist(datos$"peso")

hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

boxplot(datos$"peso") # outliers

qqnorm(datos$"peso")

set.seed(10)
hist(rnorm(1000))
set.seed(10)
qqnorm(rnorm(1000))


# Tablas de resultados técnicas descriptivas (NO HACER)

tabla <- data.frame(Estadistico=c("media","mediana","media.geometrica","coeficiente de variacion","minimo","maximo"),Edad=NA,
Peso=NA,
Altura=NA,stringsAsFactors=FALSE)
tabla[,"Edad"]<-c(mean(datos$"edad"), median(datos$"edad"),exp(mean(log(datos$"edad"))),
c(sd(datos$"edad")/mean(datos$"edad"))*100,
range(datos$"edad")[1],range(datos$"edad")[2])

tabla[,"Peso"]<-c(mean(datos$peso), median(datos$peso),exp(mean(log(datos$peso))),
c(sd(datos$peso)/mean(datos$peso))*100,
range(datos$peso)[1],range(datos$peso)[2])

tabla[,"Altura"]<-c(mean(datos$altura), median(datos$altura),exp(mean(log(datos$altura))),c(sd(datos$altura)/mean(datos$altura))*100,range(datos$altura)[1],range(datos$altura)[2])

library("openxlsx")
write.xlsx(tabla,file="tabla_descriptiva_manual.xlsx")

# Tablas de resultados técnicas descriptivas

library(crosstable)library(knitr)
res<-crosstable(datos, c(peso,edad))kable(res)



