

load('~/Desktop/curso.analisis.estadistico.con.R.2020_ONLINE/carpeta_para_alumnos/datos/datos/datos.curso1.RData')


datos$bmi <- datos$peso/c(c(datos$altura/100)^2)

hist(datos$"bmi")

mean(datos$"bmi")

median(datos$"bmi")

qqnorm(datos$"bmi")

shapiro.test(datos$bmi)

quantile(datos$"bmi",prob=seq(0,1,1/4))
quantile(datos$"bmi")

sd(datos$"bmi")

sd(datos$"bmi")/mean(datos$"bmi")

# Intervalo de confianza

resultado1 <- t.test(datos$"bmi",conf.level = 0.95)
resultado1$"estimate"
resultado1$"conf.int"

resultado2 <- t.test(datos$"bmi",conf.level = 0.99)
resultado2$"estimate"
resultado2$"conf.int"


# Comparacion con un valor (one-sample test)

t.test(datos$"bmi",mu=25)
resultado3 <- t.test(datos$"bmi",mu=25)
resultado3$"p.value"


resultado4 <- wilcox.test(datos$bmi,mu=25)
resultado4$"p.value"

# Contraste a una cola

resultado5 <-t.test(datos$bmi,mu=25,alternative="greater")

resultado5$"p.value"


###############################
# Dos muestras NO PAREADAS
###############################

# Homogeneidad de varianzas (H0 = son homogeneas)
bartlett.test(datos$bmi,datos$sexo)
# Normalidad (H0 = siguen una distribucion normales)
tapply(datos$bmi,datos$sexo,shapiro.test)

t.test(datos$"bmi"~datos$"sexo",var.equal = FALSE)
t.test(datos$"bmi"~datos$"sexo",var.equal = TRUE)

wilcox.test(datos$"bmi"~datos$"sexo")


# Otra manera de programar el analisis

bmi.hombres<-datos$bmi[datos$sexo%in%"Hombre"]
bmi.mujeres<-datos$bmi[datos$sexo%in%"Mujer"]
t.test(bmi.hombres,bmi.mujeres)


###############################
# Dos muestras PAREADAS
###############################

set.seed(10)
datos$"bmi.post"<- datos$bmi + sample(c(1:5),dim(datos)[1],replace=T)

mean(datos$"bmi")
mean(datos$"bmi.post")

t.test(datos$bmi,datos$bmi.post,paired=T)

t.test(datos$bmi.post,datos$bmi,paired=T)




###############################
# Mas de dos muestras (ANOVA)
###############################

table(datos$"nivel.estudios",exclude=NULL)

hist(datos$"peso")

qqnorm(datos$"peso")

mean(datos$"peso")


tapply(datos$"peso",datos$"nivel.estudios",mean)

tapply(datos$"peso",datos$"nivel.estudios",shapiro.test)

bartlett.test(datos$"peso",datos$"nivel.estudios")

# H0 = todos los grupos son iguales; H1 = algun grupo difiere de otro
anova(lm(datos$"peso"~datos$"nivel.estudios"))

pairwise.t.test(datos$"peso",datos$"nivel.estudios",p.adj="bonferroni")

pairwise.t.test(datos$"peso",datos$"nivel.estudios",p.adj="BH")

# No parametrico

kruskal.test(datos$"peso"~datos$"nivel.estudios")

kruskal.test(datos$"peso"~as.factor(datos$"nivel.estudios")) # Otra manera de codificar




# VARIABLES CUALITATIVAS

table(datos$"nivel.estudios")

margin.table(table(datos$"nivel.estudios"))

prop.table(table(datos$"nivel.estudios"))

# Intervalo de confianza

prop.test(as.numeric(table(datos$"nivel.estudios"))[1] , dim(datos)[1] )
prop.test(as.numeric(table(datos$"nivel.estudios"))[2] , dim(datos)[1] )
prop.test(as.numeric(table(datos$"nivel.estudios"))[3] , dim(datos)[1] )

table(datos$"fumador")
prop.table(table(datos$"fumador"))

prop.test(as.numeric(table(datos$"fumador"))[1] , dim(datos)[1] )
prop.test(as.numeric(table(datos$"fumador"))[2] , dim(datos)[1] )

# Comparar con un valor 0.40

prop.test(as.numeric(table(datos$"fumador"))[2] , dim(datos)[1] ,0.40)

# Comparar la proporcion de fumadores entre hombres y mujeres
table(datos$sexo,datos$fumador)[,c(2,1)]
prop.table(table(datos$sexo,datos$fumador)[,c(2,1)],1)
prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])

# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
prop.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])





prop.table(table(datos$nivel.estudios,datos$fumador),1)[c(2,3,1),c(2,1)]
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])



