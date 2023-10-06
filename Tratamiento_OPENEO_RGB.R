<<<<<<< HEAD
# Documentación: 
# https://openeo.org/documentation/1.0/r/
# https://r.iresmi.net/posts/2023/copernicus_openeo_ndvi_time_series/
# https://open-eo.github.io/openeo-r-client/articles/openeo-02-sample_data.html

## Cargamos librerías

#rm(list = ls())

library(openeo)
library(stars)
library(tidyverse)
library(sf)
library(leaflet)
library(leafem) 

## Cargamos datos de límites administrativos de Andalucía y Sevilla

source("./limites_administrativos.R")

## Descargamos los datos del satelite (Copernicus)

conexion <-  connect(host = "https://openeo.dataspace.copernicus.eu")
# Abre un navegador donde meter las credenciales de la plataforma de datos de Copernicus.
login()

procesos <- processes()
# process_viewer(procesos) ## Ayuda de los procesos
# procesos$load_collection  ## print metadata of the process with ID "load_collection"
# list_processes() ##Lista de procesos y metadatos

colecciones <- list_collections()
formats <- list_file_formats()

fechas <- c("2023-04-01","2023-04-30")


## BANDS: "B02": Blue ,"B03": Green ,"B04": Red ,"B08": NIR,"SCL": Clasificación

# Seleccionamos los datos a descargar, filtrado por bandas, fechas, un poligono (selecciona un rectangulo que contiene al polígono) y menos de un % de nubes
datos_coleccion <- procesos$load_collection(id = colecciones$SENTINEL2_L2A,
                                            bands=c("B02","B03","B04","SCL"),
                                            spatial_extent = un_municipio,
                                            temporal_extent = fechas,
                                            properties = list("eo:cloud_cover" = function(x) x <= 30))

# Recortamos los datos con un polínono que marca los límites administrativos del municipios
datos_colleccion_crop <- procesos$mask_polygon(datos_coleccion, un_municipio)

# Guardamos los resultados en formato NCDF
resultados <- procesos$save_result(data = datos_colleccion_crop, format = formats$output$netCDF)

# Creamos y lanzamos el proceso de descarga de los datos. 
job <- create_job(graph = resultados , title = "Bandas RGB de un mes para un municipio")
start_job(job = job)
  list_jobs()

# Descargamos los datos. Da errores en ficheros grandes o medianos, al final los bajé de la plataforma web.
download_results(job = job, folder = "./salidas/")  
  # delete_job(job=job) # Para borrar el proceso. 
file.rename("./salidas/openEO.nc", "./salidas/openEO_Sevilla_Abril_RGB.nc")

# Cargamos los datos para representarlos 
nc_file = "./salidas/openEO_Sevilla_Abril_RGB.nc"
read_stars(nc_file) -> nc_rgb

# nc_rgb
# st_dimensions(nc_rgb)
# attributes(nc_rgb)$dimensions$t$values

# Preparamos los datos para representarlos con PLOT
# Seleccionamos las 3 bandas RGB, y las reordenamos pq el satelite Sentinel 2 las ordena como BGR, y filtramos la primera fecha con datos.
nc_rgb[c(3,2,1),,,1] -> nc_rgb_f 
# Transformamos los datos, uniendo las 3 bandas en un array y creando una dimensión con ellos
merge(nc_rgb_f) -> nc_rgb_m
names(nc_rgb_m) = "RGB"

# reducimos la dimensión RGB y en atributos ponemos el color que le correspondería al punto, Se le aplica una corrección para adaptarlo a un color mas realista

st_rgb(nc_rgb_m, dimension = 4, use_alpha = FALSE, stretch = "percent",probs = c(0.01, 0.97)) -> nc_rgb_plot
# st_rgb(nc_rgb_m, dimension = 4, use_alpha = FALSE, stretch = "histogram") -> nc_rgb_plot
plot(nc_rgb_plot)


# Preparamos los datos para representarlos con Leaflet

##Seleccionamos las bandas B R G, quitamos la dimensión tiempo (para poder representarlo con addStarRGB que espera que la info de las bandas esté en la dimensión 3) quedandonos con la primera fecha, consolidamos las bandas en un único array y lo llevamos a las dimensiones.
nc_rgb %>% 
  select("B02","B03","B04") %>%
  slice(t,1) %>%
  merge() %>%
  st_set_dimensions(names = c("x", "y", "band")) -> nc_rgb_lf

# Representamos la imagen raster sobre un mapa. También correguimos los colores y si el punto no tiene información lo ponemos transparente.

leaflet()  %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addStarsRGB(nc_rgb_lf, quantiles = c(0.01, 0.95), na.color = "#00000000", opacity = 0.8)

=======
# Documentación: 
# https://openeo.org/documentation/1.0/r/
# https://r.iresmi.net/posts/2023/copernicus_openeo_ndvi_time_series/
# https://open-eo.github.io/openeo-r-client/articles/openeo-02-sample_data.html

## Cargamos librerías

#rm(list = ls())

library(openeo)
library(stars)
library(tidyverse)
library(sf)
library(leaflet)
library(leafem) 

## Cargamos datos de límites administrativos de Andalucía y Sevilla

source("./limites_administrativos.R")

## Descargamos los datos del satelite (Copernicus)

conexion <-  connect(host = "https://openeo.dataspace.copernicus.eu")
# Abre un navegador donde meter las credenciales de la plataforma de datos de Copernicus.
login()

procesos <- processes()
# process_viewer(procesos) ## Ayuda de los procesos
# procesos$load_collection  ## print metadata of the process with ID "load_collection"
# list_processes() ##Lista de procesos y metadatos

colecciones <- list_collections()
formats <- list_file_formats()

fechas <- c("2023-04-01","2023-04-30")


## BANDS: "B02": Blue ,"B03": Green ,"B04": Red ,"B08": NIR,"SCL": Clasificación

# Seleccionamos los datos a descargar, filtrado por bandas, fechas, un poligono (selecciona un rectangulo que contiene al polígono) y menos de un % de nubes
datos_coleccion <- procesos$load_collection(id = colecciones$SENTINEL2_L2A,
                                            bands=c("B02","B03","B04","SCL"),
                                            spatial_extent = un_municipio,
                                            temporal_extent = fechas,
                                            properties = list("eo:cloud_cover" = function(x) x <= 30))

# Recortamos los datos con un polínono que marca los límites administrativos del municipios
datos_colleccion_crop <- procesos$mask_polygon(datos_coleccion, un_municipio)

# Guardamos los resultados en formato NCDF
resultados <- procesos$save_result(data = datos_colleccion_crop, format = formats$output$netCDF)

# Creamos y lanzamos el proceso de descarga de los datos. 
job <- create_job(graph = resultados , title = "Bandas RGB de un mes para un municipio")
start_job(job = job)
  list_jobs()

# Descargamos los datos. Da errores en ficheros grandes o medianos, al final los bajé de la plataforma web.
download_results(job = job, folder = "./salidas/")  
  # delete_job(job=job) # Para borrar el proceso. 
file.rename("./salidas/openEO.nc", "./salidas/openEO_Sevilla_Abril_RGB.nc")

# Cargamos los datos para representarlos 
nc_file = "./salidas/openEO_Sevilla_Abril_RGB.nc"
read_stars(nc_file) -> nc_rgb

# nc_rgb
# st_dimensions(nc_rgb)
# attributes(nc_rgb)$dimensions$t$values

# Preparamos los datos para representarlos con PLOT
# Seleccionamos las 3 bandas RGB, y las reordenamos pq el satelite Sentinel 2 las ordena como BGR, y filtramos la primera fecha con datos.
nc_rgb[c(3,2,1),,,1] -> nc_rgb_f 
# Transformamos los datos, uniendo las 3 bandas en un array y creando una dimensión con ellos
merge(nc_rgb_f) -> nc_rgb_m
names(nc_rgb_m) = "RGB"

# reducimos la dimensión RGB y en atributos ponemos el color que le correspondería al punto, Se le aplica una corrección para adaptarlo a un color mas realista

st_rgb(nc_rgb_m, dimension = 4, use_alpha = FALSE, stretch = "percent",probs = c(0.01, 0.97)) -> nc_rgb_plot
# st_rgb(nc_rgb_m, dimension = 4, use_alpha = FALSE, stretch = "histogram") -> nc_rgb_plot
plot(nc_rgb_plot)


# Preparamos los datos para representarlos con Leaflet

##Seleccionamos las bandas B R G, quitamos la dimensión tiempo (para poder representarlo con addStarRGB que espera que la info de las bandas esté en la dimensión 3) quedandonos con la primera fecha, consolidamos las bandas en un único array y lo llevamos a las dimensiones.
nc_rgb %>% 
  select("B02","B03","B04") %>%
  slice(t,1) %>%
  merge() %>%
  st_set_dimensions(names = c("x", "y", "band")) -> nc_rgb_lf

# Representamos la imagen raster sobre un mapa. También correguimos los colores y si el punto no tiene información lo ponemos transparente.

leaflet()  %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addStarsRGB(nc_rgb_lf, quantiles = c(0.01, 0.95), na.color = "#00000000", opacity = 0.8)

>>>>>>> a250c6606af6f3685e77340c91483f184554f1e0
