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

setwd("/Users/pfernandezn/Desktop/ANALISIS_ESTADISTICO_CON_R_FI/datos/")

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

# install.packages("crosstable")
# install.packages("knitr")

library(crosstable)library(knitr)
res<-crosstable(datos, c(peso,edad))kable(res)

res<-crosstable(datos, c(peso,edad),
funs=c(mean, quantile,sd,IQR),
by=sexo, num_digits = 3,total="both")

kable(res)

crosstable(datos, c(peso, altura), funs=c("MEDIANA"=median, 
                                          "MEDIA (IC)"=meanCI, "DES"=sd))

#  https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html

# Variables cuantitativas agrupadas

tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",sd)

tapply(datos$"peso",datos$"sexo",mi.funcion)

library(crosstable)
res<-crosstable(datos, c(peso,edad), 
                by=sexo, num_digits = 3,total="both")

par(mfrow=c(1,2))
hist(datos$"edad"[datos$"sexo"%in%"Hombre"],
     main="Edad Hombres",xlab="Edad")
hist(datos$"edad"[datos$"sexo"%in%"Mujer"],
     main="Edad Mujeres",xlab="Edad")

# Variables cualitativas

table(datos$"estado.civil",exclude=NULL) # frecuencias absolutas

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100 # frecuencias relativas


# Graficos variable cualitativa

barplot(table(datos$"estado.civil"),ylim=c(0,90),xlab="Estado Civil",
        ylab="Frecuencia Absoluta (N)")

pie(table(datos$"estado.civil"),
    labels=paste(c("Casado","Divorciado","Soltero"), 
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

par(mfrow=c(2,1))

barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Hombre"]),main="Hombres",ylim=c(0,50))

barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Mujer"]),main="Mujeres",ylim=c(0,50))


# Tablas descritptivas (variables cualitativas)

# Tabla clasica

# Es dificil         


library(crosstable)

crosstable(datos,c(estado.civil),by=sexo)

crosstable(datos,c(peso,edad,estado.civil),by=sexo,total="both")                                                                                                                                                                                                     

library("gmodels")

CrossTable(datos$"estado.civil",datos$"sexo",
                    prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)


#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

############################
# Tabla total descriptiva
############################

# Y si tengo yo mi propia funcion descriptiva (variables cuantitativas)

library("beepr")
beep(sound = 3, expr = NULL)

descript <- function(x,nombre){
  
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


descript(x=datos$"edad",nombre="edad_curso")


tapply(datos$"edad",datos$"sexo",descript,nombre="edad")


#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

### INFERENCIA

# data.frame

sum(is.na(datos$"sexo"))

hombres <- datos[datos$sexo=="Hombre" ,  ] # Cuando no tiene missing sexo

hombres <- datos[datos$sexo%in%"Hombre" ,  ]

hombres <- subset(datos,sexo=="Hombre")

t.test(hombres$"peso")

# 79.991 (79.7997 - 80.1823)

res <- t.test(hombres$"peso")
names(res)
res$"statistic"
res$"conf.int"
res$"estimate"

mean_ic95 <- paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",round(res$"conf.int"[2],2),")",sep="")mean_ic95

res<-t.test(hombres$"peso", conf.level = 0.99)res
mean_ic99<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",round(res$"conf.int"[2],2),")",sep="")mean_ic99


# CONTRASTE DE HIPÓTESIS

hombres <- subset(datos,sexo=="Hombre")

mujeres <- subset(datos,sexo=="Mujer")

t.test(hombres$"peso",mujeres$"sexo", data = datos)



t.test(peso ~ sexo, data = datos)

RES1 <- t.test(peso ~ sexo, data = datos, var.equal = FALSE)

RES2 <- t.test(peso ~ sexo, data = datos, var.equal = TRUE)

shapiro.test(hombres$"peso")
shapiro.test(mujeres$"peso")

set.seed(10)teorica_normal<-rnorm(5000)shapiro.test(teorica_normal)$p.value

var.test(x=hombres$"peso",y=mujeres$"peso") # utiliza un Bartlett.test

var.test(peso~sexo,data=datos)

bartlett.test(peso~sexo,data=datos)

library(car)
leveneTest(peso ~ sexo, data=datos)


res_dos_colas <- t.test(hombres$"peso",mu = 79.80,sd=1,conf.level = 0.95,alternative = c("two.sided"))

res_dos_colas$"p.value"

res_less<-t.test(hombres$"peso",mu = 79.80 ,sd=1,conf.level = 0.95,alternative = c("less"))

res_less$"p.value"

res_greater<-t.test(hombres$"peso",mu = 79.80,sd=1,conf.level = 0.95,alternative = c("greater"))

res_greater$"p.value"

####################################################

shapiro.test(hombres$"peso")
shapiro.test(mujeres$"peso")
var.test(peso~sexo,data=datos)

# Test paramétrico

t.test(peso ~ sexo, data = datos, var.equal = TRUE)

t.test(peso ~ sexo, data = datos, var.equal = FALSE) # Welch

# Test no-parametrico

wilcox.test(peso ~ sexo, data = datos)

wilcox.test(peso ~ sexo, data = datos)$"p.value"


model <- lm(peso~sexo,data=datos)
anova(model)
anova(model)$"Pr(>F)"

t.test(peso ~ sexo, data = datos, var.equal = TRUE)$"p.value"

# Comparación de más de dos grupos (parametrico)

table(hombres$"nivel.estudios")
tapply(hombres$"peso",hombres$"nivel.estudios",mean)

anova(lm(hombres$"peso"~hombres$"nivel.estudios"))

shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Alto"])
shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Medio"])
shapiro.test(hombres$"peso"[hombres$"nivel.estudios"%in%"Bajo"])
bartlett.test(hombres$"peso"~hombres$"nivel.estudios") # varianzas tengo dudas!!!!

# No paramétrico

kruskal.test(hombres$"peso"~hombres$"nivel.estudios")


# En el caso que se observen alguna diferencia entre medias:
# Contrastes dos a dos (post-hoc) ajuste por comparaciones multiples

pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                     p.adj="BH")

pairwise.wilcox.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                     p.adj="BH")


# COMPARACION PROPORCIONES

# Comparar con un valor 0.40

table(datos$"fumador",exclude=NULL)

prop.table(table(datos$"fumador",exclude=NULL))


prop.test(as.numeric(table(datos$"fumador"))[2] , 
dim(datos)[1] ,
0.40,
alternative="greater")

prop.test(92,200, 0.40 ,alternative="greater")


# Comparar la proporcion de fumadores entre hombres y mujeres

table(datos$sexo,datos$fumador)[,c(2,1)]

prop.table(table(datos$sexo,datos$fumador),1)

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)],correct=FALSE)

# Otras peticiones

prop.table(table(datos$diabetes,exclude=NULL))

table(datos$sexo,datos$diabetes,exclude=NULL)

prop.table(table(datos$sexo,datos$diabetes,exclude=NULL),1)

prop.test(table(datos$sexo,datos$diabetes)[,c(2,1)])

# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)

# igual que el anova en variables cuantitativas
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])

prop.trend.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2)],
                margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1))


############
# TABLAS
############


# TABLA 0 DESCRIPTIVA CON COMPARACIONES

library("gtsummary")
library("kableExtra")

datos0<- subset(datos,select=c(edad,estado.civil,peso,cancer.prostata))

datos0 %>%
  tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p()

datos0 %>%
  tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p() %>%
  as_kable_extra(booktabs = TRUE, longtable = FALSE, linesep = "")



# TABLA 1 DESCRIPTIVA CON COMPARACIONES

require("gtsummary") # library

datos_1<-subset(datos,select= - c(ID,sexo,cancer.mama))

datos_1 %>%    
tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p()



# Exportacion de tabla

require("gtsummary")

datos_1<-subset(datos,select=-c(ID,sexo,cancer.mama))

# datos_1 <- subset(datos_1,year==1990)

tabla_exportar<-datos_1 %>%    
  tbl_summary(by=c("cancer.prostata")) %>%
  add_overall() %>%
  add_p()

library("flextable")

tf <- tempfile(fileext = ".docx")
tf<-("tabla_exportar.docx")
ft1 <- as_flex_table(tabla_exportar)
save_as_docx(ft1, path = tf)

getwd() # el archivo se ha guardado aqui


# TABLA 2 DESCRIPTIVA CON COMPARACIONES varios test

datos1 <- subset(datos,select=c(ID,altura,edad,sexo))

require("gtsummary")

tbl0<-subset(datos1,select=-c(ID,sexo)) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean}") %>%
  add_ci(pattern = "{stat} ({ci})")

tbl1<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo",
              statistic = all_continuous() ~ "{mean}")%>%
  add_ci(pattern = "{stat} ({ci})")

tbl2<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "t.test")%>%
  modify_column_hide(all_stat_cols())

tbl3<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "wilcox.test")%>%
  modify_column_hide(all_stat_cols())

tbl_final <- 
  tbl_merge(list(tbl0,tbl1, tbl2, tbl3)) %>%
  modify_spanning_header(everything() ~ NA)

tbl_final

# Exportacion en png

setwd("/Users/pfernandezn/Desktop/")

library(flextable)
tf <- tempfile(fileext = ".png")
tf<-("tabla_resultados.png")
ft1 <- as_flex_table(tbl_final)
save_as_image(ft1, path = tf)


# TABLA 3 DESCRIPTIVA CON COMPARACIONES

datos3<-subset(datos,select=c(altura,edad,nivel.estudios,cancer.prostata))

tabla_3<-datos3 %>%    
  tbl_summary(by="cancer.prostata",
              statistic=list(all_continuous() ~ "{mean}", 
                             all_categorical() ~ "{n} ({p}%)")) %>%
  add_ci()%>%
  add_overall() %>%
  add_p(test=list(all_continuous() ~ "t.test", 
                  all_categorical() ~ "chisq.test"))









