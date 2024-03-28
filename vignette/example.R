library(iconDWD)
library(glue)
library(R.utils)
library(terra)
library(lubridate)
library(readr)
library(dplyr)


directorioBase <- "D:/INIA/ICON/prueba_2/20240319/18"

rutas_bz2 <- list.files(directorioBase, pattern = "\\.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 6)


rutas_grib2 <- list.files(directorioBase, pattern = "\\.grib2$", full.names = TRUE, recursive = TRUE)
ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl)


#dir<-"D:/INIA/ICON/prueba_2/20240319/18/NetCDF/"
dir<-"E:/ICON/20240319/18/NetCDF"

dir_files<-FiltrarVariable(dir,variable = "TOT_PREC")
icon_18<-rast(dir_files)


nuble<-vect("D:/crop/comunas/R16.shp")
nuble<-project(nuble,icon_18)

chile<-vect("F:/shape/chile/Regional.shp")
chile<-project(chile,icon_18)
icon_chile<-crop(icon_18,chile,mask=T)

raster<-icon_chile
time(raster)<-time(raster)-hours(3)

inicio <- ymd_hm("2024-03-20 00:00")
fin <- ymd_hm("2024-03-21 00:00")
raster_subset <- raster[[time(raster) >= inicio & time(raster) <= fin]]
plet(raster_subset[[nlyr(raster_subset)]],tiles="Streets")



# Obtener todas las estaciones de la región "Coquimbo"
estaciones_coquimbo <- coordenadas_estaciones(region = "Coquimbo",institucion = "CEAZA")
plet(estaciones_coquimbo)
# Obtener todas las estaciones de la comuna "Illapel"
estaciones_illapel <- coordenadas_estaciones(comuna = "Illapel")
plet(estaciones_illapel)
# Obtener todas las estaciones con primera lectura después de una fecha específica
estaciones_recientes <- coordenadas_estaciones(Inicio = ymd("2020-01-01"), institucion = "INIA")
plet(estaciones_recientes)





#
resultado<-terra::extract(raster,estaciones_recientes,ID=F)
resultado<-t(resultado)
colnames(resultado)<-estaciones_recientes$nombre
fechas<-time(raster)
df_final<-data.frame(fechas,resultado)
df_final










