
######################
# COSAS DEL CURSO
######################


rm(list=ls())

setwd("/Users/pfernandezn/Desktop/datos")

load("datos.curso1.RData")

stripchart(peso ~ sexo,data=datos,method="jitter",jitter=0.1)

stripchart(peso ~ sexo,data=datos,method="jitter",jitter=0.5)

dotchart(t(table(datos$"sexo",datos$"estado.civil")))


######################################################################################

# Estadistica descritiva (data.table)

# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

######################################################################################

library("data.table")

datos.tabla<-setDT(datos)

datos.tabla[sexo=="Hombre", .(m.hombre=mean(peso)), by=.(fumador)]




par(mfrow=c(2,1))
tapply(datos$"edad",datos$"sexo",boxplot,ylab=c("edad"))

