<<<<<<< HEAD
## Datos Auxiliares --- l�mites administrativos de Andaluc�a, un municipio, sus distritos y barrios.
## Carga de datos geogr�ficos como objetos de sf para l�mites administrativos de Andaluc�a (obtenidos del IECA)
# L�mites administrativos Andaluc�a: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/DERA/g13.htm
# Carga en memoria:
#   municipios -> Todos los municipios de Andaluc�a
#   distritos ->  Todos los distritos de un municipio, est� filtrado a "Sevilla"
#   barrios -> Todos los barrios de un municipios, est� filtrado a "Sevilla"
#   diferencia_muncipios_barrios -> La regi�n del municipio que no est� cubierto por los barrios del municipio
#   un_municipio -> Un municipio de Andaluc�a, est� filtrado a "Sevilla"
#   un_barrio -> Un barrio del municipio Andaluc�a, est� filtrado a "Bellavista"
#   un_municipio_y_barrios -> Todo el municipio considerando los barrios y el resto que no se cubre


library(tidyverse)
library(sf)
library(leaflet)

ciudad = "Sevilla"
barrio = "Bellavista"

##Carga de datos y transformaci�n de las CRS (requerido para representarlo con leaflet)

# Geometr�a de los l�mites administrativos de los municipios de Andaluc�a
municipios <- st_read(dsn="./shp/13_01_TerminoMunicipal.shp")
municipios <-st_transform(municipios,crs=4326)


# Geometr�a de los l�mites administrativos de los barrios y distritos de los principales municipios de Andaluc�a
# No coinciden exactamente con las l�mites de los municipios

barrios <- st_read(dsn="./shp/13_24_BarrioUrbano.shp")
barrios <-st_transform(barrios,crs=4326)


## Filtramos las distintas geometr�as que vamos a necesitar

## L�mites administrativos de Sevilla

municipios %>%
  filter(nombre==ciudad) -> un_municipio

## L�mites administrativos de los barrios de Sevilla.  

barrios %>%
  filter(municipio==ciudad) -> barrios

## L�mites administrativos de los distritos

barrios %>%
  group_by(municipio, distrito) %>%
  summarize(geometry=st_union(geometry)) -> distritos 

## L�mites administrativos del barrio de Bellavista

# barrios[barrios$distrito=="Sur",] ## Filtrar�a los l�mites adminsitrativos de un distrito, que selecciona varios barrios

barrios[barrios$nombre ==barrio,] -> un_barrio

un_barrio %>%
  group_by(municipio, distrito, nombre) %>%
  summarize(geometry=st_union(geometry)) -> un_barrio 


## Identificamos las zonas del municipio que no est�n cubiertas por los barrios y la unimos a las que si lo est�n

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

## Representaci�n gr�fica de los l�mites administrativos

plot(un_municipio_y_barrios)

leaflet()  %>% 
  addTiles() %>%
  addPolylines (data=un_municipio)

=======
## Datos Auxiliares --- l�mites administrativos de Andaluc�a, un municipio, sus distritos y barrios.
## Carga de datos geogr�ficos como objetos de sf para l�mites administrativos de Andaluc�a (obtenidos del IECA)
# L�mites administrativos Andaluc�a: https://www.juntadeandalucia.es/institutodeestadisticaycartografia/DERA/g13.htm
# Carga en memoria:
#   municipios -> Todos los municipios de Andaluc�a
#   distritos ->  Todos los distritos de un municipio, est� filtrado a "Sevilla"
#   barrios -> Todos los barrios de un municipios, est� filtrado a "Sevilla"
#   diferencia_muncipios_barrios -> La regi�n del municipio que no est� cubierto por los barrios del municipio
#   un_municipio -> Un municipio de Andaluc�a, est� filtrado a "Sevilla"
#   un_barrio -> Un barrio del municipio Andaluc�a, est� filtrado a "Bellavista"
#   un_municipio_y_barrios -> Todo el municipio considerando los barrios y el resto que no se cubre


library(tidyverse)
library(sf)
library(leaflet)

ciudad = "Sevilla"
barrio = "Bellavista"

##Carga de datos y transformaci�n de las CRS (requerido para representarlo con leaflet)

# Geometr�a de los l�mites administrativos de los municipios de Andaluc�a
municipios <- st_read(dsn="./shp/13_01_TerminoMunicipal.shp")
municipios <-st_transform(municipios,crs=4326)


# Geometr�a de los l�mites administrativos de los barrios y distritos de los principales municipios de Andaluc�a
# No coinciden exactamente con las l�mites de los municipios

barrios <- st_read(dsn="./shp/13_24_BarrioUrbano.shp")
barrios <-st_transform(barrios,crs=4326)


## Filtramos las distintas geometr�as que vamos a necesitar

## L�mites administrativos de Sevilla

municipios %>%
  filter(nombre==ciudad) -> un_municipio

## L�mites administrativos de los barrios de Sevilla.  

barrios %>%
  filter(municipio==ciudad) -> barrios

## L�mites administrativos de los distritos

barrios %>%
  group_by(municipio, distrito) %>%
  summarize(geometry=st_union(geometry)) -> distritos 

## L�mites administrativos del barrio de Bellavista

# barrios[barrios$distrito=="Sur",] ## Filtrar�a los l�mites adminsitrativos de un distrito, que selecciona varios barrios

barrios[barrios$nombre ==barrio,] -> un_barrio

un_barrio %>%
  group_by(municipio, distrito, nombre) %>%
  summarize(geometry=st_union(geometry)) -> un_barrio 


## Identificamos las zonas del municipio que no est�n cubiertas por los barrios y la unimos a las que si lo est�n

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

## Representaci�n gr�fica de los l�mites administrativos

plot(un_municipio_y_barrios)

leaflet()  %>% 
  addTiles() %>%
  addPolylines (data=un_municipio)

>>>>>>> a250c6606af6f3685e77340c91483f184554f1e0
