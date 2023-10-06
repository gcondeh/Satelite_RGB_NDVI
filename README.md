# Satelite_RGB_NDVI
Descarga y tratamiento de datos de satélite (Sentinel 2) con OpenEO. RGB y NDVI

Se han generado 3 ficheros:
1. limites_administrativos.R
  Son datos Auxiliares que se usarán despues. Se cargan los límites administrativos de Andalucía, un municipio, sus distritos y barrios.
  Los datos geográficos se han objtenido del IECA (https://www.juntadeandalucia.es/institutodeestadisticaycartografia/DERA/g13.htm) y se rabajan como objetos de sf.
  Se genearn los siguientes datos:
    municipios -> Todos los municipios de Andalucía
    distritos ->  Todos los distritos de un municipio, está filtrado a "Sevilla"
    barrios -> Todos los barrios de un municipios, está filtrado a "Sevilla"
    diferencia_muncipios_barrios -> La región del municipio que no está cubierto por los barrios del municipio
    un_municipio -> Un municipio de Andalucía, está filtrado a "Sevilla"
    un_barrio -> Un barrio del municipio Andalucía, está filtrado a "Bellavista"
    un_municipio_y_barrios -> Todo el municipio considerando los barrios y el resto que no se cubre

2. Tratamiento_OPENEO_RGB.R
   Se descargan los datos espacionales del municipio seleccionado desde el hub de datos comernicus de la UE (https://openeo.dataspace.copernicus.eu/) usando la librería OpenEO.
   En concreto se descargan las bandas RGB y SCL (clasificación de escenas para quitar las nubes)
   Se representan los datos.

3. Tratamiento_OPENEO_NVDI.R
   Se descargan los datos de indicador NDVI (indice de vegetación) agregados mensualmente, quitando nubes.
   La información se trata de distintas formas:
   -  Representación en un mapa por puntos
   -  Representación del valor agregado por barrios en un mapa
   -  Representación del valor agregado por barrios en un mapa y su tendencia
   -  Ranking de los barios por NDVI máximo y mínimo.
   -  Variaciones de NDVI por barrios
   -  Evolución de las series temporales.
  
  Adicionalmente se dejan los ficheros de trabajo. 
   
     
