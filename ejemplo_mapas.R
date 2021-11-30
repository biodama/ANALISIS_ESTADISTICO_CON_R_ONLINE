

library("rgdal")
library("tmap")

mapa_muni<-readOGR("/Users/pfernandezn/Desktop/gadm36_ESP_shp/gadm36_ESP_4.shp")

proj4string(mapa_muni) 

head(mapa_muni@data)
mapa_muni@data$ine<-seq(1,dim(mapa_muni@data)[1],1)
head(mapa_muni@data)



datos<-data.frame(ine=mapa_muni@data$"ine",INC=NA,NC=NA,stringsAsFactors=F)
set.seed(10)
datos$"INC"<-sample(c(0:200),dim(mapa_muni@data)[1],replace=T)
set.seed(40)
datos$"NC"<-sample(c(0,300),dim(mapa_muni@data)[1],replace=T)
set.seed(30)
datos<-datos[sample(datos$ine),]
head(datos)

sum(datos$"ine"==mapa_muni@data$"ine") # 3

length(intersect(datos$"ine",mapa_muni@data$"ine"))

indice<-match(mapa_muni@data$"ine",datos$"ine")
datos<-datos[indice,]

sum(datos$"ine"==mapa_muni@data$"ine") # 8302


head(mapa_muni@data)

mapa_muni@data$"INC"<-datos$"INC"
mapa_muni@data$"NC"<-datos$"NC"

tmap_mode("plot")

tm_shape(mapa_muni) +  tm_fill(col = "INC") + tm_borders()

tm_shape(mapa_muni) +  tm_polygons(col = "INC", n = 10, palette = "Blues")

tm_shape(mapa_muni) +  tm_fill(col = "INC") + tm_borders()



tmap_mode("view")

tm_shape(mapa_muni) +  tm_polygons(col = "INC", n = 10, palette = "Blues")


tmap_mode("plot")
m1 <- tm_shape(mapa_muni) + tm_borders() +
  tm_fill(col = "INC", n = 5) 
m2 <- tm_shape(mapa_muni) +  tm_borders() +
  tm_fill(col = "NC", n = 5, palette = "Blues") 
tmap_arrange(m1, m2)



library(ggplot2)
library(sf)
st_muni <- as(mapa_muni, "sf")
g1 <- ggplot() +
  geom_sf(data = st_muni, aes(fill = INC))

g1


