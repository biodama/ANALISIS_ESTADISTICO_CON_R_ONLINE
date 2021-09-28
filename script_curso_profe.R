#################################################
#################################################
#        ANALISIS DESCRIPTIVO MUESTRA
#################################################
#################################################

rm(list=ls()) # borrar todos los objetos del workspace 
gc()	   # liberar memoria RAM	

ls() # ver que objetos tengo en workspace

###############################
# Cargar los datos del curso
###############################

# Windows

datos <-  read.table("C:\\Users\\biodama\\datos\\datos.curso1.txt",header=T) # una manera

load("C:\\Users\\biodama\\datos\\datos.curso1.RData") # cargo un workspace. OTRA manera

ls()

# Mac o Linux

datos <-  read.table("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE/datos/datos.curso1.txt",
header=T)

load("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE/datos/datos.curso1.RData")


###############################
# Checks basicos
###############################

dim(datos) # numero de filas y columnas

head(datos,10) # vemos los 10 primeros registros

str(datos) # formato de variables

########################################################

hombres <- datos[datos$"sexo"%in%"Hombre",]
dim(hombres)
str(hombres)

row.names(hombres)<- NULL

mujeres <- datos[datos$"sexo"%in%"Mujer",]
dim(mujeres)
str(mujeres)

row.names(mujeres)<- NULL

########################################################



################################################################
# Estadistica descriptiva variable cuantitativa (edad)
################################################################

# Si tiene pocos registros visualizo la variable asi

datos$"edad"

datos$edad

datos[  , c(2) ]

# Si tiene muchos registros visualizo la variable asi

datos$"edad"[c(1:20)]

# Estadisticos muestrales

mean(datos$"edad") # media
median(datos$"edad")# mediana
quantile(datos$"edad")# cuartiles
sd(datos$"edad")# desviacion tipica
range(datos$"edad")# minimo y maximo

hist(datos$"edad") # histograma
# install.packages("ggplot2")
library("ggplot2")
g1<-ggplot(data=datos, aes(edad)) + 
  	geom_histogram(color="black", fill="white")
ggsave(g1,file="/Users/pfernandezn/Desktop/hist.png")


boxplot(datos$"edad")# boxplot

# Descriptivo de una variable cuantitativa segun los valores de otra variable (cualitativa)

tapply(datos$"edad",datos$"sexo",mean)
tapply(datos$"edad",datos$"sexo",median)

tapply(datos$"edad",datos$"estado.civil",mean)

mean(hombres$"edad")
mean(mujeres$"edad")

par(mfrow=c(2,1))
hist(hombres$"edad")
hist(mujeres$"edad")


################################################################
# Estadistica descriptiva variable cualitativa (sexo)
################################################################

# Frecuencias absolutas (N)
table(datos$"sexo") 
unique(datos$"sexo")


# Frecuencias absolutas tablas complejas
margin.table(table(datos$"sexo",datos$"estado.civil"),1)
margin.table(table(datos$"sexo",datos$"estado.civil"),2)


prop.table(table(datos$"sexo"))*100
prop.table(table(datos$"sexo",datos$"estado.civil"))*100

prop.table(table(datos$"sexo",datos$"estado.civil"),1)*100
prop.table(table(datos$"sexo",datos$"estado.civil"),2)*100


SNP<-c("AA","AT","AA","TT","AT","AA")


pie(table(datos$sexo))

barplot(table(datos$sexo))


# EJEMPLO DE ANALISIS

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
tabla[,"Altura"]<-c(mean(datos$"altura"), median(datos$"altura"),
exp(mean(log(datos$"altura"))),paste(quantile(datos$"altura"),collapse=";"),
c(sd(datos$"altura")/mean(datos$"altura"))*100,sd(datos$"altura"),range(datos$"altura")[1],
range(datos$altura)[2])

# install.packages("openxlsx")
library("openxlsx")

write.xlsx(tabla,file="/Users/pfernandezn/Desktop/tabla_1.xlsx")



prop.table(table(datos$cancer.mama,datos$sexo,exclude=NULL),2)*100

#################
# COMPARACION
#################

# One sample t.test

ingesta.calorias<-c(5263,5472,5400,6180,6300,6545,6805,7525,7520,8100,8700)

result1<-t.test(ingesta.calorias,mu=7725) # a dos colas

names(result1)

result1$"p.value"

mean(ingesta.calorias)
t.test(ingesta.calorias)

result2<-t.test(ingesta.calorias,mu=7725,alternative = "greater") # a una cola

result3<-t.test(ingesta.calorias,mu=7725,alternative = "less") # a una cola


resultado_no_parametrico <- wilcox.test(ingesta.calorias,mu=7725)


# Two sample t.test

calorias.hombres<-c(100,330,340,1000,323,453,532,332)

calorias.mujeres<-c(40,41,55,77,37,94,12,34)

resultado_two_sample<-t.test(x=calorias.hombres,y=calorias.mujeres)

resultado_two_sample_no_para<-wilcox.test(x=calorias.hombres,y=calorias.mujeres)

##########################
# Check de asunciones
##########################

# qqnorm plot
par(mfrow=c(1,2))
qqnorm(calorias.hombres)
qqnorm(calorias.mujeres)

# Histograma
par(mfrow=c(1,2))
hist(calorias.hombres)
hist(calorias.mujeres)

# Shapiro-Wilk Normality Test

shapiro.test(calorias.hombres)

shapiro.test(calorias.mujeres)

# Test de homogeneidad: var.test()

var.test(calorias.hombres,calorias.mujeres)




