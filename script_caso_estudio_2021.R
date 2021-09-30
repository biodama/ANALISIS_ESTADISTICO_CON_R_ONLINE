##################################################################
##################################################################
#                        SCRIPT CASO ESTUDIO
##################################################################
##################################################################

rm(list=ls())
gc()

ls()


#1.Analisis descriptivo de las variables categoricas:
#	1.1. Estado civil
#	1.2. Diabetes

load("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE/datos/datos.curso1.RData")

ls()

dim(datos)
str(datos)
head(datos)
head(datos,10)

head(datos[,c(1:8)])

summary(datos)

# Registros check
class(datos$"ID") # "numeric"
length(datos$"ID")# 200
length(unique(datos$"ID"))# 200
sum(is.na(datos$"ID"))# 0


################
# Estado civil
################

class(datos$"estado.civil") # "character"
table(datos$"estado.civil")
sum(is.na(datos$"estado.civil")) # 0

datos$"estado.civil"[c(4,9,33)]<-c("casado","div","Sóltero")
table(datos$"estado.civil")
# casado     Casado        div Divorciado    Soltero    Sóltero 

datos$"estado.civil"[datos$"estado.civil"%in%"Sóltero"]<-"Soltero"
datos$"estado.civil"[datos$"estado.civil"%in%"casado"]<-"Casado"
datos$"estado.civil"[datos$"estado.civil"%in%"div"]<-"Divorciado"
table(datos$"estado.civil")

table(datos$"estado.civil")
prop.table(table(datos$"estado.civil"))
barplot(table(datos$"estado.civil"),ylim=c(0,150))

tabla.pie<-table(datos$"estado.civil")
names(tabla.pie)<-paste(names(tabla.pie),"\n (",prop.table(table(datos$"estado.civil"))*100,"%)",
sep="")
pie(tabla.pie)


################
# Diabetes
################

class(datos$"diabetes") # "character"
table(datos$"diabetes")
sum(is.na(datos$"diabetes")) # 0


prop.table(table(datos$"diabetes"))
barplot(table(datos$"diabetes"))


######################
# Diabetes vs Sexo
######################

class(datos$"sexo") # "character"
table(datos$"sexo")
sum(is.na(datos$"sexo")) # 0

table(datos$"diabetes",datos$"sexo")
margin.table(table(datos$"diabetes",datos$"sexo"),1)
margin.table(table(datos$"diabetes",datos$"sexo"),2)
prop.table(table(datos$"diabetes",datos$"sexo"))*100
prop.table(table(datos$"diabetes",datos$"sexo"),1)*100
prop.table(table(datos$"diabetes",datos$"sexo"),2)*100

counts <- table(datos$"diabetes", datos$"sexo")
barplot(counts, main="Diabetes vs Sexo",
  xlab="Number", col=c("darkblue","red"),
  legend = rownames(counts), beside=TRUE)
  



# 2.Análisis de asociación entre cáncer de mama y las variables:

table(datos_mujer$"cancer.mama",datos_mujer$"sexo",exclude=NULL)
  
class(datos$"sexo")
sum(is.na(datos$"sexo"))
table(datos$"sexo")

datos_mujer<-datos[datos$"sexo"%in%"Mujer",]

row.names(datos_mujer)<-NULL

dim(datos_mujer)
table(datos_mujer$"sexo")

# Borro de el workspace la base de datos completa

rm(datos)

# Outcome

class(datos_mujer$"cancer.mama") # character
table(datos_mujer$"cancer.mama")
sum(is.na(datos_mujer$"cancer.mama")) # 0

prop.table(table(datos_mujer$"cancer.mama"))
barplot(table(datos_mujer$"cancer.mama")) 


# Para la modelizacion hacemos una nueva variable de cancer de mama

datos_mujer$"cm"<-datos_mujer$"cancer.mama"
datos_mujer$"cm"[datos_mujer$"cancer.mama"%in%"No"]<-0
datos_mujer$"cm"[datos_mujer$"cancer.mama"%in%"Si"]<-1
table(datos_mujer$"cm")
datos_mujer$cm<-as.factor(datos_mujer$"cm")

table(datos_mujer$"cm",datos_mujer$"cancer.mama")

# 2.1. Edad
  
class(datos_mujer$"edad") # "numeric"
sum(is.na(datos_mujer$"sexo")) # 0

mean(datos_mujer$"edad")
median(datos_mujer$"edad")
quantile(datos_mujer$"edad")
sd(datos_mujer$"edad")
var(datos_mujer$"edad")
hist(datos_mujer$"edad")

# Modifico las edades con informacion que dan

datos_mujer[datos_mujer$"ID"%in%c(194,188),]
datos_mujer$"edad"[datos_mujer$"ID"%in%194]<-40
datos_mujer$"edad"[datos_mujer$"ID"%in%188]<-40
datos_mujer[datos_mujer$"ID"%in%c(194,188),] 

# Modelo de asociacion

modelo1 <- glm(cm ~ edad,data=datos_mujer,family="binomial")

exp(cbind(OR=coef(modelo1),confint(modelo1))) 
 
#  2. Altura
#  3. Peso
#  4. bmi
#  5. Diabetes
#  6. Fumador
#  7. Estado civil


# 8. Nivel de estudios

class(datos_mujer$"nivel.estudios") # character
table(datos_mujer$"nivel.estudios")
sum(is.na(datos_mujer$"nivel.estudios")) # 0

prop.table(table(datos_mujer$"nivel.estudios"))


table(as.factor(datos_mujer$"nivel.estudios"))
datos_mujer$"ne"<-datos_mujer$"nivel.estudios"
datos_mujer$"ne"[datos_mujer$"nivel.estudios"%in%"Bajo"]<-2
datos_mujer$"ne"[datos_mujer$"nivel.estudios"%in%"Medio"]<-1
datos_mujer$"ne"[datos_mujer$"nivel.estudios"%in%"Alto"]<-0
table(datos_mujer$"ne")
datos_mujer$"ne"<-as.factor(datos_mujer$"ne")
table(datos_mujer$"ne",datos_mujer$"nivel.estudios")


modelo2 <- glm(cm ~ ne,data=datos_mujer,family="binomial")
exp(cbind(OR=coef(modelo2),confint(modelo2))) 
summary(modelo2)


modelo3 <- glm(cm ~ as.numeric(ne),data=datos_mujer,family="binomial")
exp(cbind(OR=coef(modelo3),confint(modelo3))) 
summary(modelo3)

table(datos_mujer$"cm",datos_mujer$"ne")


# 9. Fumador teniendo en cuenta el bmi

class(datos_mujer$"fumador") # character
table(datos_mujer$"fumador")
sum(is.na(datos_mujer$"fumador")) # 0

prop.table(table(datos_mujer$"fumador"))

table(as.factor(datos_mujer$"fumador"))
datos_mujer$"fu"<-datos_mujer$"fumador"
datos_mujer$"fu"[datos_mujer$"fumador"%in%"Si"]<-1
datos_mujer$"fu"[datos_mujer$"fumador"%in%"No"]<-0
table(datos_mujer$"fu")
datos_mujer$"fu"<-as.factor(datos_mujer$"fu")
table(datos_mujer$"fu",datos_mujer$"fumador")


# BMI

datos_mujer$"bmi" <- datos_mujer$"peso"/c(c(datos_mujer$"altura"/100)^2)

mean(datos_mujer$"bmi")
median(datos_mujer$"bmi")
quantile(datos_mujer$"bmi")
sd(datos_mujer$"bmi")
var(datos_mujer$"bmi")
hist(datos_mujer$"bmi")
shapiro.test(datos_mujer$"bmi")
qqnorm(datos_mujer$"bmi")


modelo4 <- glm(cm ~ fu + bmi,data=datos_mujer,family="binomial")
round(exp(cbind(OR=coef(modelo4),round(confint(modelo4),digits=3))),digits=3) 
summary(modelo4)

modelo5 <- glm(cm ~ fu,data=datos_mujer,family="binomial")
round(exp(cbind(OR=coef(modelo5),round(confint(modelo5),digits=3))),digits=3) 
summary(modelo5)


library("forestplot")
library("dplyr")

table_data <- structure(list(mean  = c(NA,1,exp(coef(modelo4)[2]),1,exp(coef(modelo4)[3])), 
                        lower = c(NA,1,exp(confint(modelo4)[2,1]),1,exp(confint(modelo4)[3,1])),
                        upper = c(NA,1,exp(confint(modelo4)[2,2]),1,exp(confint(modelo4)[3,2]))),
                        row.names = c(NA, -5L), 
                        class = "data.frame")

tabletext <- cbind(c("Variable","Fumador","","BMI",""),
                   c("Category", "ref", "Si", "ref", "BMI"))

table_data %>% 
  forestplot(labeltext = tabletext, 
             is.summary = c(rep(TRUE, 1), rep(FALSE, 4)),
             clip = c(0.1, 2.5), 
             xlog = TRUE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"))



  