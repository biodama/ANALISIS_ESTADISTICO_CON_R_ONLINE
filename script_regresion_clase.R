

load('~/Desktop/curso.analisis.estadistico.con.R.2020_ONLINE/carpeta_para_alumnos/datos/datos/datos.curso1.RData')

# Asociado el peso y la altura

Peso = cte + pendiente*altura + error

modelo1<-lm(datos$"peso"~datos$"altura")
summary(modelo1)

# Asociacion igual en hombres y mujeres

datos.mujeres<-datos[datos$sexo%in%"Mujer",]

datos.hombres<-datos[datos$sexo%in%"Hombre",]

modelo_mujeres<-lm(datos.mujeres$"peso"~datos.mujeres$"altura")
summary(modelo_mujeres)

modelo_hombres<-lm(datos.hombres$"peso"~datos.hombres$"altura")
summary(modelo_hombres)


modelo_crudo<-lm(datos$"peso"~ datos$"altura")

modelo_ajustado<-lm(datos$"peso"~ datos$"altura" + datos$"sexo")
summary(modelo_ajustado)


# Logistica

table(datos$"cancer.prostata")
table(datos$"cancer.prostata",datos$"sexo")

datos$"cancer.p"<-datos$"cancer.prostata"
datos$"cancer.p"[datos$"cancer.prostata"%in%"No"]<-0
datos$"cancer.p"[datos$"cancer.prostata"%in%"Si"]<-1
datos$"cancer.p"<-as.factor(datos$"cancer.p")

# Esta asociado el cancer de prostata con la edad

modelo.logistico<-glm(cancer.p ~ edad, data=datos,family=binomial)
summary(modelo.logistico)

exp(cbind(OR=coef(modelo.logistico),confint(modelo.logistico)))

# Esta asociado el cancer de prostata con fumar

modelo.logistico2<-glm(cancer.p ~ fumador, data=datos,family=binomial)
summary(modelo.logistico2)

