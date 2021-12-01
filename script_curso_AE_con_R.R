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


# RESOLUCION EJERCICIOS


tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
"coeficiente de variacion","desviación típica","minimo","maximo"),
Edad=NA,Peso=NA,Altura=NA,
stringsAsFactors=FALSE)

tabla[,"Edad"]<-c(mean(datos$edad), median(datos$edad),exp(mean(log(datos$edad))),
paste(round(quantile(datos$edad),digits=2),collapse=";"),(sd(datos$edad)/mean(datos$edad))*100,
sd(datos$edad),range(datos$edad)[1],range(datos$edad)[2])

tabla[,"Peso"]<-c(mean(datos$peso), median(datos$peso),exp(mean(log(datos$peso))),
paste(round(quantile(datos$peso),digits=2),collapse=";"),(sd(datos$peso)/mean(datos$peso))*100,
sd(datos$peso),range(datos$peso)[1],range(datos$peso)[2])

tabla[,"Altura"]<-c(mean(datos$altura), median(datos$altura),
exp(mean(log(datos$altura))),paste(round(quantile(datos$altura),digits=2),collapse=";"),
c(sd(datos$altura)/mean(datos$altura))*100,sd(datos$altura),
range(datos$altura)[1],range(datos$altura)[2])


library("openxlsx")

write.xlsx(tabla,file="/Users/pfernandezn/Desktop/tabla_descriptiva1.xlsx")


#################################################################
#################################################################
# INFERENCIA
#################################################################
#################################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE-master/datos/")

load("datos.curso1.RData")

hombres<-datos[datos$sexo%in%"Hombre",]
dim(hombres)
mean(hombres$peso)
sd(hombres$peso)
set.seed(20)
teorico_hombres<- rnorm(n=100000, mean = mean(hombres$peso), sd = sd(hombres$peso))

mujeres<-datos[datos$sexo%in%"Mujer",]
mean(mujeres$peso)
sd(mujeres$peso)
set.seed(20)
teorico_mujeres<- rnorm(n=100000, mean = mean(mujeres$peso), sd = sd(mujeres$peso))

par(mfrow=c(2,2))
hist(hombres$peso)
hist(teorico_hombres,main="Teorico hombres",xlim=c(50,85))
hist(mujeres$peso)
hist(teorico_mujeres,main="Teorico mujeres",xlim=c(50,85))


###################################################
###################################################
# Intervalo de confianza variable numerica
###################################################
###################################################

mean(hombres$"peso")

res<-t.test(hombres$"peso")
names(res)
res$"estimate"
res$"conf.int"    
mean_ic95<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
round(res$"conf.int"[2],2),")",sep="")  
mean_ic95


res<-t.test(hombres$"peso", conf.level = 0.99)
mean_ic99<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
round(res$"conf.int"[2],2),")",sep="")  
mean_ic99


teorico_ambos<- rnorm(n=100000, mean = mean(datos$peso), sd = sd(datos$peso))
par(mfrow=c(2,1))
hist(datos$peso)
hist(teorico_ambos,main="Teorico ambos",xlim=c(30,100))

res<-t.test(datos$"peso")
mean_ic95<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
round(res$"conf.int"[2],2),")",sep="")  
mean_ic95


###################################################
###################################################
# Comparacion variables numericas
###################################################
###################################################

diferencia<-mean(hombres$"peso")-mean(mujeres$"peso")

teorico_diferencia<-rnorm(n=100000, mean = diferencia, sd = 1)
hist(teorico_diferencia,xlim=c(0,30))


# Comparamos peso hombres y mujeres

res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95)
res$"p.value"


# Comparamos con valor de la OMS para peso hombres (90 sd=1)

hist(teorico_hombres,main="Teorico hombres",xlim=c(50,85))
res<-t.test(hombres$"peso",mu = 90,sd=1, conf.level = 0.95)
res$"p.value"


# Contraste unilateral o bilateral (una cola o dos colas)
# Comparamos con valor de la OMS para peso hombres (79.50 sd=1)

res_dos_colas<-t.test(hombres$"peso",mu = 79.80,sd=1, conf.level = 0.95,alternative = c("two.sided"))
res_dos_colas$"p.value"

res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1, conf.level = 0.95,alternative = c("less"))
res_less$"p.value"

res_greater<-t.test(hombres$"peso",mu = 79.80,sd=1, conf.level = 0.95,alternative = c("greater"))
res_greater$"p.value"


# Asunciones (normalidad)

par(mfrow=c(2,2))
hist(hombres$"peso")
hist(mujeres$"peso")
qqnorm(hombres$"peso")
qqnorm(mujeres$"peso")

shapiro.test(hombres$"peso")
shapiro.test(mujeres$"peso")

set.seed(10)
teorica_normal<-rnorm(5000)
hist(teorica_normal)
shapiro.test(teorica_normal)


# Asunciones (homogeneidad de varianzas)

var.test(x=hombres$"peso",y=mujeres$"peso")


# Podemos tener en cuenta que las muestras no tienen varianzas homogeneas

res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95)
res

res<-t.test(hombres$"peso",mujeres$"peso", conf.level = 0.95,var.equal = TRUE)
res


# Test no parametricos


res<-wilcox.test(hombres$"peso",mujeres$"peso")
res

res<-wilcox.test(hombres$"peso",mu=79.80)
res

res<-t.test(hombres$"peso",mu=79.80)
res


# Mas de dos grupos (en hombres: peso por nivel de estudios)

table(hombres$"nivel.estudios")
tapply(hombres$"peso",hombres$"nivel.estudios",mean)


anova(lm(hombres$"peso"~hombres$"nivel.estudios")) 
# summary(lm(hombres$"peso"~hombres$"nivel.estudios")) 


pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),p.adj="bonferroni") 

bartlett.test(hombres$"peso"~hombres$"nivel.estudios")
oneway.test(hombres$"peso"~hombres$"nivel.estudios") 


# No parametrico

kruskal.test(hombres$"peso"~hombres$"nivel.estudios")


# Correccion de p-valores por comparaciones multiples

pvalores<-c(0.001,0.43,0.54300)

p.adjust(pvalores,method="BH")





###################################################
###################################################
# Comparacion proporciones
###################################################
###################################################

fumadores <- c( 83, 70 ) 

pacientes <- c( 86, 82 ) 

prop1<-prop.test(fumadores[1],pacientes[1])

paste(round(prop1$"estimate",2)," (",round(prop1$"conf.int"[1],2),"-",
round(prop1$"conf.int"[2],2),")",sep="")

prop2<-prop.test(fumadores[2],pacientes[2])

paste(round(prop2$"estimate",2)," (",round(prop2$"conf.int"[1],2),"-",
round(prop2$"conf.int"[2],2),")",sep="")

prop.test(fumadores,pacientes) 

chisq.test(matrix(c(83,70,3,8),2)) 

fisher.test(matrix(c(83,70,3,8),2))


fumadores <- c( 83, 90, 129, 70 ) 
pacientes <- c( 86, 93, 136, 82 ) 
prop.test(fumadores,pacientes) 
prop.trend.test(fumadores,pacientes)



###########################################################################
###########################################################################
# Regresion lineal (outcome, y , variable independiente) es numerico
###########################################################################
###########################################################################


# Peso y la altura

plot(y=datos$"altura",x=datos$"peso")
abline(lm(datos$"altura"~datos$"peso"))

modelo1 <- lm(datos$"altura"~datos$"peso")

# lm(altura~peso,data=datos)
resultado<-summary(modelo1)
names(resultado)
resultado$"coefficients"[2,1] # estimador
resultado$"coefficients"[2,4] # p-value
confint(modelo1)
plot(modelo1)

hist(resid(modelo1))
qqnorm(resid(modelo1))


cor(datos$"altura",datos$peso)
cor.test(datos$"altura",datos$peso)
cor(datos$"altura",datos$peso,method="spearman")
cor(datos$"altura",datos$peso)^2


# Altura y el sexo

sexo_num<-as.numeric(as.factor(datos$"sexo"))-1

modelo2 <- lm(datos$"altura" ~ datos$"sexo")
summary(modelo2)

tapply(datos$"altura",datos$"sexo",mean)
169.7716-149.9015

t.test(datos$altura[datos$sexo%in%"Mujer"],datos$altura[datos$sexo%in%"Hombre"],var.equal=TRUE)


##########################
# Confusion
##########################

modelo_ambos<-lm(altura~peso,data=datos)
# Peso: 0.980124

modelo_ambos_multiple<-lm(altura~peso+sexo,data=datos)
summary(modelo_ambos_multiple)
# Peso: 0.08816


##########################
# Interaccion
##########################

modelo_hombres<-lm(altura~peso,data=datos[datos$sexo%in%"Hombre",])
# Peso: 0.1167
confint(modelo_hombres)
modelo_mujeres<-lm(altura~peso,data=datos[datos$sexo%in%"Mujer",])
# Peso: 0.06498
confint(modelo_mujeres)

modelo_interaccion<-lm(altura ~ peso + sexo + peso*sexo,data=datos)
summary(modelo_interaccion)



modelo_celmira<-lm(altura~peso+edad,data=datos)
modelo_celmira_crudo<-lm(altura~peso,data=datos)
edad_gr<-cut(datos$edad,breaks=quantile(datos$edad,prob=seq(0,1,1/3)),
include.lowest=T)

modelo_celmira<-lm(altura~peso+edad_gr,data=datos)
modelo_celmira_crudo<-lm(altura~peso,data=datos)


##########################
# Regresion logistica
##########################

# Outcome, variable dependiente,...


table(datos$"cancer.mama",exclude=NULL)
class(datos$"cancer.mama")


datos$"cancer.m"<-datos$cancer.mama
datos$"cancer.m"[datos$cancer.mama%in%"No"]<-0
datos$"cancer.m"[datos$cancer.mama%in%"Si"]<-1
table(datos$"cancer.m")
datos$"cancer.m"<-as.factor(datos$"cancer.m")


# Variables explicativas

class(datos$"estado.civil")
datos$"ec"<-as.factor(datos$"estado.civil")
table(datos$"ec",exclude=NULL)
levels(datos$"ec") # "Casado"     "Divorciado" "Soltero"

nuevo.orden<- c("Soltero","Casado","Divorciado")
datos$"ec"<- factor(datos$"ec",levels=nuevo.orden)
levels(datos$"ec")



# Modelo

modelo_logistica<- glm(cancer.m ~  ec , data=datos,family="binomial")
summary(modelo_logistica)
exp(cbind(OR=coef(modelo_logistica),confint(modelo_logistica)))
anova(modelo_logistica,test="Chisq")

# test de tendencia
modelo_logistica<- glm(cancer.m ~  as.numeric(ec) , data=datos,family="binomial")
summary(modelo_logistica)
exp(cbind(OR=coef(modelo_logistica),confint(modelo_logistica)))


# Modelo multivariable

class(datos$fumador)

datos$"bmi"<-c(datos$"peso")/c(c(datos$"altura"/100)^2)

modelo_logistica<- glm(cancer.m ~  as.factor(fumador) + bmi, data=datos,
family="binomial")
summary(modelo_logistica)
exp(cbind(OR=coef(modelo_logistica),confint(modelo_logistica)))




#####################################################################################


# EJEMPLO DE RMARKDOWN DESDE R

library("rmarkdown")

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_ONLINE-master/")

render(input="prueba.curso.Rmd",
# output_dir="para_publicar",
output_file="informe_prueba_curso.pdf")
















