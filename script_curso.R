

# COSAS DEL CURSO


rm(list=ls())
setwd("/Users/pfernandezn/Desktop/datos")
load("datos.curso1.RData")

stripchart(peso ~ sexo,data=datos,method="jitter",jitter=0.1)
stripchart(peso ~ sexo,data=datos,method="jitter",jitter=0.5)