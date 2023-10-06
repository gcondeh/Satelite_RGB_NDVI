# Documentación: 
  # https://openeo.org/documentation/1.0/r/
# https://r.iresmi.net/posts/2023/copernicus_openeo_ndvi_time_series/
# https://open-eo.github.io/openeo-r-client/articles/openeo-02-sample_data.html

## Cargamos librerías

# rm(list = ls())

library(openeo)
library(stars)
library(tidyverse)
library(sf)
library(leaflet)
library(highcharter)
library(zoo)
library(forecast)

## Cargamos datos de límites administrativos de Andalucía y Sevilla ####

source("./limites_administrativos.R")

## Descargamos los datos del satélite (Copernicus) ####

conexion <-  connect(host = "https://openeo.dataspace.copernicus.eu")
login() # Abre un navegador donde meter las credenciales de la plataforma de datos de Copernicus.

procesos <- processes()
  # process_viewer(procesos) ## Ayuda de los procesos
  # procesos$load_collection  ## print metadata of the process with ID "load_collection"
  # list_processes() ##Lista de procesos y metadatos

colecciones <- list_collections()
formats <- list_file_formats()

# Filtro de fechas, filtramos trozos pq el proceso falla si el volumen de datos es muy grande.
fechas <- c("2021-01-01","2023-12-31")


## BANDS: "B02": Blue ,"B03": Green ,"B04": Red ,"B08": NIR,"SCL": Clasificación
# Seleccionamos los datos a descargar, filtrado por bandas, fechas, un poligono (selecciona un rectangulo que contiene al polígono) y menos de un % de nubes

datos_coleccion <- procesos$load_collection(id = colecciones$SENTINEL2_L2A,
                                            bands=c("B04","B08","SCL"),
                                            spatial_extent = un_municipio,
                                            temporal_extent = fechas,
                                            properties = list("eo:cloud_cover" = function(x) x <= 50))

# Recortamos los datos con un polínono que marca los límites administrativos del municipios
datos_colleccion_crop <- procesos$mask_polygon(datos_coleccion, un_municipio)
  
## Quitar datos que enturbien análisis: filtros a los puntos clasificados como Vegetación y no vegetación)

  # Label	Classification (SLC)
  # https://sentinels.copernicus.eu/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm-overview
  # 0	NO_DATA
  # 1	SATURATED_OR_DEFECTIVE
  # 2	CAST_SHADOWS
  # 3	CLOUD_SHADOWS
  # 4	VEGETATION
  # 5	NOT_VEGETATED
  # 6	WATER
  # 7	UNCLASSIFIED
  # 8	CLOUD_MEDIUM_PROBABILITY
  # 9	CLOUD_HIGH_PROBABILITY
  # 10	THIN_CIRRUS
  # 11	SNOW or ICE

#Generamos la función con la que vamos a crear la mascara que filtrará los datos.
filtro_slc <- function(data, context) {
  scl <- data[3]
  !(scl == 4 | scl == 5)
}

#Creamos una mascara con los puntos que tienen nubes. 
mask_slc <- procesos$reduce_dimension(data = datos_coleccion, 
                                      dimension = "bands",
                                      reducer = filtro_slc)

# Eliminamos los puntos con nubes de cada capa
datos_coleccion_mask <- procesos$mask(datos_colleccion_crop, mask_slc)

# Calculamos el indice de vegetación agregando las distintas bandas. Usamos una función predefinida en OpenEO
## El cálculo del indice es: NVDI = (NIR-VIS) / (NIR + VIS)  ...> (B08-B04) / (B08+B04) 
ndvi <- procesos$ndvi(data= datos_coleccion_mask)

# Agregamos los datos por mes, la función que aplicamos para agregar los datos es la media
ndvi_mes <- procesos$aggregate_temporal_period(data=ndvi, period = "month", reducer = mean)

# Guardamos los resultados en formato NCDF
resultados_ndvi <- procesos$save_result(data = ndvi_mes, format = formats$output$netCDF)

# Creamos y lanzamos el proceso de descarga de los datos. 
job <- create_job(graph = resultados_ndvi, title = "Datos de Ndvi para un municipio agregados mensualmente (21 a 23)")
start_job(job = job)
  list_jobs()
  log_job(job = job)
  #delete_job(job = "j-fe85449e9777454d9923199e58de7c0a")

# Descargamos los datos. Da errores en ficheros grandes o medianos, al final los bajé de la plataforma web.
download_results(job = job, folder = "./salidas/")
file.rename("./salidas/openEO.nc", "./salidas/openEO_ndvi_2021_2023.nc")

##Carga y tratamiento de los mapas ####
nc_file_1 = "./salidas/openEO_ndvi_2016_2018_50.nc"
nc_file_2 = "./salidas/openEO_ndvi_2019_2020_50.nc"
nc_file_3 = "./salidas/openEO_ndvi_2021_2023_50.nc"

c(nc_file_1,nc_file_2, nc_file_3) -> files


read_stars(files, proxy = FALSE, along = 3) -> nc
names(nc) <- "NDVI"

## Ver mapa en verde para NDVI
#   The value range of the NDVI is -1 to 1. 
#   Negative values of NDVI (values approaching -1) correspond to water. 
#   Values close to zero (-0.1to 0.1) generally correspond to barren areas of rock, sand, or snow.
#   Low, positive values represent shrub and grassland (approximately 0.2 to 0.4)
#   While high values indicate temperate and tropical rainforests (values approaching 1).
#   Paleta de colores: https://custom-scripts.sentinel-hub.com/sentinel-2/ndvi/


breaks <- c(-1, -0.2, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
cols <- c("#000000", "#a50026", "#fdae61", "#ffffbf", "#d9ef8b", "#a6d96a", "#99B718", "#74A901", "#66A000",  "#529400",  "#3E8601", "#207401")


plot(nc, breaks=breaks ,col = cols)

leaflet()  %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  # addTiles() %>%
  addPolylines (data=un_municipio_y_barrios ) %>%
  addGeoRaster(nc, colorOptions =  colorOptions(palette=cols, breaks=breaks, na.color = "#00000000"),opcacity =0.5 )


## Gráfica NDVI por meses por barrio o distrito

## Camino 1. openEO. Usar funciones de openEO para que nos den el dato ya procesado. ######
## No es lo que he hecho por los tiempos de respuesta, los errores si tenía un alto volumen de datos y por poder usar distintas agregaciones a mi gusto.
## La forma de hacerlo sería:

## Incluir pasos adicionales en la descarga de datos. 
## concretamente una agregación de datos por polígonos
ndvi_barrio <- procesos$aggregate_spatial( data = ndvi_mes, geometries = un_municipio_y_barrios, reducer = mean) 

## descarga de datos en un formato mas amigable, CSV
procesos$save_result(data=ndvi_barrio,  format = "CSV") -> resultados_ndvi

## Ejecutar los procesos
job <- create_job(graph = resultados_ndvi, title = "Annio de datos de Ndvi para un municipio y barrios mensualmente")
start_job(job = job)
download_results(job = job, folder = "./salidas/")

## Cargamos y masajeamos los datos para que tengan un formato fácil de representar

read_csv(file = "./salidas/timeseries.csv") -> ndvi_serie
names(ndvi_serie) <- c("Mes","indice","NDVI")

ndvi_serie %>%
  # mutate_at("NDVI",  ~replace_na(., 0)) %>%
  mutate(Mes = as.Date(Mes)) %>%
  arrange(Mes, indice)-> ndvi_serie

## Representamos con ggplot
ndvi_serie %>%
  ggplot(aes(x=Mes, y = NDVI, group = indice, color = indice)) + geom_line()

## Representamos con higcharter
hc <-   hchart(ndvi_serie, "line", hcaes(x=Mes, y = NDVI, group = indice)) %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis( min=min(ndvi_serie$NDVI)-0.1, max = max(ndvi_serie$NDVI)+0.1, gridLineWidth=0)

hc


## Camino 2. Trabajamos en R con Stars. Podemos tener problemas de memoria dependiendo del objeto que manejemos. ######

## vamos a agregar datos en función de los polígonos que definen los barrios.
## tenemos que poner los polígonos en el mismo sistema de referencia que el objeto con los NDVI georeferenciados.

un_municipio_y_barrios <-st_transform(un_municipio_y_barrios,crs=st_crs(nc))
distritos <-st_transform(distritos,crs=st_crs(nc))

# Después los agregamos
ndvi_barrios <- aggregate(nc, by = un_municipio_y_barrios, FUN = mean, na.rm=TRUE)

# st_dimensions(ndvi_barrios)
# attributes(ndvi_barrios)$dimensions$geometry

## rm(list=c("nc"))

## Gráfica NDVI por meses por barrio

# st_set_dimensions(ndvi_barrios, "geometry", un_municipio_y_barrios$nombre) -> ndvi_barrios_ds

paleta_ndvi <- colorBin(cols, bins = breaks, na.color = "#00000000")

ndvi_barrios %>%
  st_as_sf(long=TRUE) %>%
  mutate(color = paleta_ndvi(NDVI)) %>%
  st_join(un_municipio_y_barrios, join = st_equals )%>%
  st_transform(crs=4326) %>%
  mutate(t = as.Date(t))-> ndvi_barrios_sf

ndvi_barrios_sf %>%
  filter(year(t)=="2022") -> ndvi_barrios_slice




# Gráfica de NDVI por barrios sobre mapa.

## Representación de múltiples mapas, abiertos por fecha
ndvi_barrios_slice %>%
  ggplot()+
  geom_sf(fill = ndvi_barrios_slice$color)+
  facet_wrap(ndvi_barrios_slice$t)+
  theme_bw()


## Representación interactiva sobre un mapa, por defectos selecciona la primera capa ... creo

leaflet()  %>% 
  addTiles() %>%
  addPolylines (data=ndvi_barrios_slice$geometry, color="blue", opacity = 0.2, weight = 1, fill = TRUE, fillColor = ndvi_barrios_slice$color, fillOpacity = 0.1, label = paste(ndvi_barrios_slice$nombre, round(ndvi_barrios_slice$NDVI,digits = 2), sep = " : "))



## Vemos cuales barrios tienen los mayores y menores NDVI a lo largo del tiempo. 

ndvi_barrios_sf %>%
  group_by(t) %>%
  filter(NDVI == max(NDVI, na.rm = TRUE) | NDVI == min(NDVI, na.rm = TRUE)) -> ndvi_extremos


hchart(ndvi_extremos, "point", hcaes(x=t, y = NDVI, group = nombre)) %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis( min=min(ndvi_extremos$NDVI)-0.1, max = max(ndvi_extremos$NDVI)+0.1, gridLineWidth=0) %>%
  hc_tooltip(pointFormat = '{series.name} <br> {point.x: %d-%m-%y}' )


ndvi_barrios_sf %>%
  group_by(nombre) %>%
  filter(!ymd(t)  %in% c("2016-05-01", "2021-04-01", "2017-03-01") )%>% # Quitamos mayo del 16 que parece que tiene anomalías. 
  filter(NDVI == max(NDVI, na.rm = TRUE) | NDVI == min(NDVI, na.rm = TRUE)) %>%
  mutate(tipo = ifelse( NDVI == max(NDVI), "NDVI_MAX","NDVI_MIN") ) %>%
  ungroup() %>%
  arrange(NDVI) -> ndvi_barrios_extremos

  
hchart(ndvi_barrios_extremos,"point", hcaes(x = nombre, y = NDVI, group=tipo, color = color)) %>%
        hc_tooltip(pointFormat = '{point.name} <br> NDVI: {point.y: ,.2f}' ) %>%
        hc_add_theme(hc_theme_538())
        # Anomalías en mayo 16, abril 2021 y marzo 2017


hchart(ndvi_barrios_extremos,"point", hcaes(x=nombre, y = NDVI, group = tipo , color = color)) %>%
  hc_chart(polar = TRUE) %>%
  hc_tooltip(pointFormat = '{point.name} <br> NDVI: {point.y: ,.2f}' )%>%
  hc_add_theme(hc_theme_538())


# Ver un barrio completo. Su evolución a lo largo del tiempo

ndvi_barrios_sf %>%
  filter(nombre %in% c("Árbol Gordo","Bellavista", "Barriada de Pineda" )) -> ndvi_barrios_linea_temp


hchart(ndvi_barrios_linea_temp,"spline", hcaes(x = t, y = NDVI, color = color, group = nombre)) %>%
  hc_xAxis(type = "datetime")  %>%
  hc_tooltip(pointFormat = 'NDVI: {point.y: ,.2f} <br> {point.x: %d-%m-%y}' )


## Gráficos Low_Hig y experimento radar

ndvi_barrios_extremos %>%
  as_tibble()%>%
  select(distrito,nombre,tipo,NDVI) %>%
  pivot_wider(names_from = tipo, values_from = NDVI) %>% 
  rename( "low"= "NDVI_MIN") %>%
  rename( "high"= "NDVI_MAX")%>%
  # mutate (rango = high - low) %>%
  arrange((high - low)/2)-> ndvi_barrios_hl

ndvi_barrios_hl %>% 
  hchart( "columnrange",hcaes(x = nombre, high = high, low = low, color= paleta_ndvi(high))) %>%
  hc_chart(inverted = TRUE) %>%
  # hc_chart polar = TRUE) %>%
  hc_plotOptions(columnrange = list(dataLabels = list(
    enabled = TRUE,
    format = "{y: ,.2f}" ))) %>%
  hc_tooltip(pointFormat = '{x}' ) %>%
  hc_legend(enabled = FALSE) %>% 
  hc_title(text = "Umbrales NDVI por barrio") %>% 
  hc_xAxis(title = list(enabled = FALSE) ) %>%
  hc_yAxis(title = list(text = "NDVI"), max = max(ndvi_barrios_hl$high)+0.1, gridLineWidth=0) %>%
  hc_add_theme(hc_theme_darkunica())


ndvi_barrios_hl %>% 
  hchart( "bar",hcaes(x = nombre, y = high))  %>%
  hc_chart(inverted = FALSE, polar = TRUE) %>%
  hc_yAxis(title = list(text = "NDVI"), max = max(ndvi_barrios_hl$high)+0.1, gridLineWidth=0) 

## Gráficos de flujo

ndvi_barrios_sf %>%
  hchart( "streamgraph",hcaes(x = t, y = round(NDVI,3), group = distrito), stacking = list(enabled = TRUE))  %>%
  hc_xAxis(type = "datetime") %>%
  hc_tooltip(table = FALSE, sort = TRUE)


## Tratamiento como serie de datos temporales ####

# Preparamos los datos quitando periodos incompletos o con datos anómalos

ndvi_barrios_sf %>%
  filter(!ymd(t)  %in% c("2016-05-01", "2021-04-01", "2017-03-01") )%>% # Quitamos mayo del 16 que parece que tiene anomalías. 
  filter(year(t) %in% as.character(seq(2017,2022))) %>%
  #filter(nombre %in% c("Bellavista" )) %>%
  as_tibble() %>%
  #mutate(nombre = paste(distrito,nombre, sep = "#"))%>%
  select(t,NDVI,nombre) %>%
  complete(t = seq.Date(min(t), max(t) , by= "month"))%>%
  pivot_wider(names_from = "nombre", values_from = "NDVI")-> ndvi_barrios_linea_temp

ndvi_barrios_linea_temp[,-113] -> ndvi_barrios_linea_temp

# Creamos la serie de datos

ts(ndvi_barrios_linea_temp[,-1], start = c(2017,1), frequency = 12) -> serie

#is.regular(serie, strict = TRUE)

# rellenamos los NA con el valor medio entre los extremos. 

na.approx(serie) -> serie_completa

# Dibujamos la serie con o sin huecos.
# par(mfrow = c(2,1))
# plot(serie)
# plot(serie_completa)

# descomponemos la serie en la parte estacional, tendencia y ruido.

decompose(serie_completa)-> decompose_serie # calculamos la descomposición
serie_completa - decompose_serie$seasonal -> serie_sin_estacional # Calculamos la serie sin la componente estacional

# Representamos los componentes de las series para hacernos una idea
plot(decompose(serie_completa[,1]))
plot(stl(serie_completa[,"Las Letanías"],"per"))
plot(decompose_serie$trend[,11:20])
plot(serie_sin_estacional[,"serie_completa.Barriada de Pineda"])

# Hacemos una regresión lineal a la tendencia de las series y analizamos si crece o no
tslm(serie_completa ~ trend) -> lm_serie

# creamos un dataframe con los barrios y el crecimiento, o no, del NDVI
as.data.frame(lm_serie$coefficients[2,]) -> pendientes
  pendientes$nombre <- row.names(pendientes)
  names(pendientes) <- c("pendiente","nombre")
  arrange(pendientes,pendiente)-> pendientes

# vamos a representar un mapa del municipio, con el valor del NDVI por barrio y si tiene tendencia creciente o no.

# calculamos los centros de los polígonos que delimitan cada barrio y preparamos los datos a representar
st_centroid(un_municipio_y_barrios, of_largest_polygon=TRUE) -> centro_barrios 

centro_barrios %>% 
  left_join(pendientes) %>%
  mutate(tendencia = case_when(
        round(pendiente,4) == 0 ~ "igual",
        pendiente >0 ~ "sube",
        pendiente <0 ~ "baja",
        TRUE ~ "Pendiente" )) %>% 
  st_transform(centro_barrios_slope, crs=4326)-> centro_barrios_slope

# Creamos los iconos a representar 
tendencia <- iconList(
  sube = makeIcon("./iconos/sube.png", 0 ,15, 15 ,0),
  baja = makeIcon("./iconos/baja.png" ,0 ,15, 15 ,0),
  igual = makeIcon("./iconos/mantiene.png", 0 ,15, 5 ,0 )
)

# Creamos el mapa iconos a representar 
leaflet()  %>% 
  addTiles() %>%
  addPolylines (data=ndvi_barrios_slice$geometry, color="blue", opacity = 0.2, weight = 1, fill = TRUE, fillColor = ndvi_barrios_slice$color, fillOpacity = 0.1, label = paste(ndvi_barrios_slice$nombre, round(ndvi_barrios_slice$NDVI,digits = 2), sep = " : ")) %>%
  addMarkers(data=centro_barrios_slope, icon = tendencia[centro_barrios_slope$tendencia])

# Gráficos para comparar todas las series de datos
library(mvtsplot)
mvtsplot(serie_completa)
mvtsplot(serie_sin_estacional)