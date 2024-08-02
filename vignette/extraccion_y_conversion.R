library(iconDWD)
library(glue)
library(R.utils)
library(terra)
library(lubridate)
library(readr)
library(dplyr)




directorioBase <- "E:/viento_19-21/"
ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
#extraccion y conversion solo para precipitacion
rutas_bz2 <- list.files(directorioBase, pattern = "\\U_10M.grib2.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)

rutas_grib2 <- list.files(directorioBase, pattern = "\\U_10M.grib2$", full.names = TRUE, recursive = TRUE)
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = T,ruta_script = ruta_script_wsl,ncores = 6)



rutas_bz2 <- list.files(directorioBase, pattern = "\\V_10M.grib2.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)

rutas_grib2 <- list.files(directorioBase, pattern = "\\V_10M.grib2$", full.names = TRUE, recursive = TRUE)
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = T,ruta_script = ruta_script_wsl,ncores = 6)



#extraccion y conversion solo para Temperatura maxima
rutas_bz2 <- list.files(directorioBase, pattern = "\\TMAX_2M.grib2.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)

rutas_grib2 <- list.files(directorioBase, pattern = "\\TMAX_2M.grib2$", full.names = TRUE, recursive = TRUE)
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl,ncores = 6)


#extraccion y conversion solo para Temperatura minima
rutas_bz2 <- list.files(directorioBase, pattern = "\\TMIN_2M.grib2.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)

rutas_grib2 <- list.files(directorioBase, pattern = "\\TMIN_2M.grib2$", full.names = TRUE, recursive = TRUE)
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl,ncores = 6)


ruta <- "E:/ppbiobio_11_06/"
fecha_inicio <- as.Date("2024-06-11")
fecha_fin <- as.Date("2024-06-12")
variable <- "TOT_PREC"
hora_modelo <- "00"
datos <- cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = TRUE)

chile<-cargar_regiones()
R08<-cargar_regiones("R08")
R08<-project(R08,datos)
datos_biobio<-crop(datos,R08,mask=T,touches=T)

plot(datos_biobio)
ext_chile<-vect("inst/extdata/regiones/ext_chile.shp")
