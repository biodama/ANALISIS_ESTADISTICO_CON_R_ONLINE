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

datos <-  read.table("C:/Users/biodama/datos/datos.curso1.txt",header=T) # una manera

load("C:/Users/biodama/datos/datos.curso1.RData") # cargo un workspace. OTRA manera

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
boxplot(datos$"edad")# boxplot







