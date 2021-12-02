######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################


rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE-master/datos/")

load("datos.curso1.RData")


############################################################################
############################################################################
# 1. Preparacion de datos
############################################################################
############################################################################

dim(datos)

head(datos)

head(datos[,c(1:10)])

str(datos)

str(datos[,c(1:5)])

#####################################################
# Outcome o variable dependiente (cancer protata)
#####################################################

class(datos$"cancer.prostata") # "character"

table(datos$"cancer.prostata",exclude=NULL)
#  No   Si <NA> 
#  24   76  100 

head(datos[is.na(datos$"cancer.prostata"),])

table(datos$"sexo",datos$"cancer.prostata",exclude=NULL)

class(datos$"sexo")
table(datos$"sexo",exclude=NULL)
unique(datos$"sexo")

#####################################
# Estratificacion de los datos
#####################################

dim(datos)# 200  11
datos<-datos[datos$"sexo"%in%"Hombre",]
dim(datos)# 100  11
table(datos$"sexo",exclude=NULL)

table(datos$"cancer.prostata",exclude=NULL)
# No Si 
# 24 76


######################################
# Variable de interes
######################################

class(datos$"diabetes")
table(datos$"diabetes",exclude=NULL)
# No Si 
# 55 45
tabla<-table(datos$"diabetes",datos$"cancer.prostata",exclude=NULL)
colnames(tabla)<-paste("Prostata-",colnames(tabla),sep="")
rownames(tabla)<-paste("Diabetes-",rownames(tabla),sep="")
tabla

tabla<-table(Diabetes=datos$"diabetes",Prostata=datos$"cancer.prostata",exclude=NULL)

##########################################################################
# Posibles variables de confusion (Edad, nivel de estudios, bmi, fumador)
##########################################################################


class(datos$"peso") # numeric
sum(is.na(datos$"peso")) # 0
range(datos$"peso")# 77.20059 82.28081
mean(datos$"peso")# 79.991
quantile(datos$"peso")
#       0%      25%      50%      75%     100% 
# 77.20059 79.37832 80.02680 80.60192 82.28081 
#par(mfrow=c(2,1))
hist(datos$"peso")
qqnorm(datos$"peso")
shapiro.test(datos$"peso") # p-value = 0.5985


class(datos$"altura") # numeric
sum(is.na(datos$"altura")) # 0
range(datos$"altura")# 167.6929 172.8742 (cm)
mean(datos$"altura")# 169.7716
quantile(datos$"altura")
#       0%      25%      50%      75%     100% 
# 167.6929 168.9830 169.7328 170.4842 172.8742
#par(mfrow=c(2,1))
hist(datos$"altura")
qqnorm(datos$"altura")
shapiro.test(datos$"altura") # p-value = 0.4546

# Creamos BMI
datos$"bmi"<-c(datos$"peso")/c(c(datos$"altura"/100)^2)

class(datos$"bmi") # numeric
sum(is.na(datos$"bmi")) # 0
range(datos$"bmi")# 26.56333 28.70381
mean(datos$"bmi")# 27.75562
quantile(datos$"bmi")
#        0%      25%      50%      75%     100% 
# 26.56333 27.49305 27.75624 28.10115 28.70381
#par(mfrow=c(2,1))
hist(datos$"bmi")
qqnorm(datos$"bmi")
shapiro.test(datos$"bmi") # p-value = 0.4341


class(datos$"edad")# numeric
sum(is.na(datos$"edad")) # 0
range(datos$"edad")# 1 85
mean(datos$"edad")# 39.64
quantile(datos$"edad")
#        0%      25%      50%      75%     100% 
# 1.00 17.75 38.00 60.00 85.00
#par(mfrow=c(2,1))
hist(datos$"edad")
qqnorm(datos$"edad")
shapiro.test(datos$"edad") # p-value = 0.0006421

datos_mirar<-datos[datos$edad<=17.75,]
# datos_mirar<-datos[datos$edad<=quantile(datos$"edad")[2],]
dim(datos_mirar)# 25 12

#library("openxlsx")
#write.xlsx(datos_mirar,file="/Users/pfernandezn/Desktop/datos_mirar.xlsx")


# Importamos los datos revisados

library("openxlsx")
datos_check<-read.xlsx("/Users/pfernandezn/Desktop/datos_mirar_revisado.xlsx",sheet=1)

datos_check<-datos_check[,c("ID","edad")]

length(unique(datos_check$ID))# 25
length(datos_check$ID)# 25

length(intersect(datos_mirar$"ID",datos_check$"ID"))# 25
length(intersect(datos$"ID",datos_check$"ID"))# 25

class(datos_check$"edad")# numeric
sum(is.na(datos_check$"edad")) # 0
range(datos_check$"edad")# 23 85
quantile(datos_check$"edad")
# 0%  25%  50%  75% 100% 
# 23   44   58   67   85 

for(i in 1:length(datos_check$"ID")){
	
	print(i)
	print(datos[datos$"ID"%in%datos_check$"ID"[i],c("ID","edad")])
	print(datos_check[i,])
	
	datos$"edad"[datos$"ID"%in%datos_check$"ID"[i]]<-datos_check$"edad"[i]
	
	
}

datos[datos$"ID"%in%c(23,90),c("ID","edad")]


class(datos$"edad")# numeric
sum(is.na(datos$"edad")) # 0
range(datos$"edad")# 18 85
mean(datos$"edad")# 51.19
quantile(datos$"edad")
#   0%  25%  50%  75% 100% 
# 18.0 34.0 51.5 66.0 85.0 
#par(mfrow=c(2,1))
hist(datos$"edad")
qqnorm(datos$"edad")
shapiro.test(datos$"edad") # p-value = 0.006701



class(datos$"fumador") # character
sum(is.na(datos$"fumador"))# 0
table(datos$"fumador",exclude=NULL)
# No Si 
# 52 48
prop.table(table(datos$"fumador",exclude=NULL))*100
table(FUMADOR=datos$"fumador",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL)
table(FUMADOR=datos$"fumador",CANCER_PROSTATA=datos$"cancer.prostata",
DIABETES=datos$"diabetes",exclude=NULL)
prop.table(table(FUMADOR=datos$"fumador",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL),1)
prop.table(table(FUMADOR=datos$"fumador",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL),2)


class(datos$"nivel.estudios") # character
sum(is.na(datos$"nivel.estudios"))# 0
table(datos$"nivel.estudios",exclude=NULL)
#Alto  Bajo Medio 
#   44    33    23 
prop.table(table(datos$"nivel.estudios",exclude=NULL))*100
table(NIVEL.ESTUDIOS=datos$"nivel.estudios",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL)
table(NIVEL.ESTUDIOS=datos$"nivel.estudios",CANCER_PROSTATA=datos$"cancer.prostata",
DIABETES=datos$"diabetes",exclude=NULL)
prop.table(table(NIVEL.ESTUDIOS=datos$"nivel.estudios",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL),1)
prop.table(table(NIVEL.ESTUDIOS=datos$"nivel.estudios",CANCER_PROSTATA=datos$"cancer.prostata",exclude=NULL),2)

barplot(table(datos$"nivel.estudios"))



