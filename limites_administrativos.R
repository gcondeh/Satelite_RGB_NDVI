## Datos Auxiliares --- límites administrativos de Andalucía, un municipio, sus distritos y barrios.
## Carga de datos geográficos como objetos de sf para límites administrativos de Andalucía (obtenidos del IECA)
# Límites administrativos Andalucía: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/DERA/g13.htm
# Carga en memoria:
#   municipios -> Todos los municipios de Andalucía
#   distritos ->  Todos los distritos de un municipio, está filtrado a "Sevilla"
#   barrios -> Todos los barrios de un municipios, está filtrado a "Sevilla"
#   diferencia_muncipios_barrios -> La región del municipio que no está cubierto por los barrios del municipio
#   un_municipio -> Un municipio de Andalucía, está filtrado a "Sevilla"
#   un_barrio -> Un barrio del municipio Andalucía, está filtrado a "Bellavista"
#   un_municipio_y_barrios -> Todo el municipio considerando los barrios y el resto que no se cubre


library(tidyverse)
library(sf)
library(leaflet)

ciudad = "Sevilla"
barrio = "Bellavista"

##Carga de datos y transformación de las CRS (requerido para representarlo con leaflet)

# Geometría de los límites administrativos de los municipios de Andalucía
municipios <- st_read(dsn="./shp/13_01_TerminoMunicipal.shp")
municipios <-st_transform(municipios,crs=4326)


# Geometría de los límites administrativos de los barrios y distritos de los principales municipios de Andalucía
# No coinciden exactamente con las límites de los municipios

barrios <- st_read(dsn="./shp/13_24_BarrioUrbano.shp")
barrios <-st_transform(barrios,crs=4326)


## Filtramos las distintas geometrías que vamos a necesitar

## Límites administrativos de Sevilla

municipios %>%
  filter(nombre==ciudad) -> un_municipio

## Límites administrativos de los barrios de Sevilla.  

barrios %>%
  filter(municipio==ciudad) -> barrios

## Límites administrativos de los distritos

barrios %>%
  group_by(municipio, distrito) %>%
  summarize(geometry=st_union(geometry)) -> distritos 

## Límites administrativos del barrio de Bellavista

# barrios[barrios$distrito=="Sur",] ## Filtraría los límites adminsitrativos de un distrito, que selecciona varios barrios

barrios[barrios$nombre ==barrio,] -> un_barrio

un_barrio %>%
  group_by(municipio, distrito, nombre) %>%
  summarize(geometry=st_union(geometry)) -> un_barrio 


## Identificamos las zonas del municipio que no están cubiertas por los barrios y la unimos a las que si lo están

barrios %>%
  group_by(municipio) %>%
  summarize(geometry=st_union(geometry)) %>%
  ungroup() -> barrios_tmp

st_difference (un_municipio, barrios_tmp) -> diferencia_muncipios_barrios

diferencia_muncipios_barrios %>%
  select(nombre,municipio,geometry) %>%
  mutate(distrito = "Resto") %>%
  mutate(nombre = "Resto") -> diferencia_tmp

barrios %>%
  select(nombre,municipio,distrito,geometry) -> barrios_tmp

rbind(barrios_tmp, diferencia_tmp) -> un_municipio_y_barrios

rm(list=c("barrios_tmp", "diferencia_tmp"))

un_municipio_y_barrios %>%
  group_by(municipio, distrito, nombre) %>%
  summarize(geometry=st_union(geometry))%>%
  ungroup()-> un_municipio_y_barrios

## Representación gráfica de los límites administrativos

plot(un_municipio_y_barrios)

leaflet()  %>% 
  addTiles() %>%
  addPolylines (data=un_municipio)

