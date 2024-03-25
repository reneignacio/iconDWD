library(iconDWD)
library(glue)
library(R.utils)

directorioBase <- "E:/ICON/20240319/18"

rutas_bz2 <- list.files(directorioBase, pattern = "\\.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 6)


rutas_grib2 <- list.files(directorioBase, pattern = "\\.grib2$", full.names = TRUE, recursive = TRUE)
ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl)


coords_estacion <- obtener_coords_estacion(nombre_estacion = "Ce arroz")
valores_y_fechas <- extraer_valores_y_fechas(icon_18, coords_estacion)
print(valores_y_fechas)


library(iconDWD)
library(terra)

dir_files<-FiltrarVariable(dir = "E:/ICON/20240319/06/NetCDF",variable = "T_2M")

icon_18<-rast(dir_files)

icon_18<-icon_18-273.15

nuble<-vect("D:/crop/comunas/R16.shp")
nuble<-project(nuble,icon_18)

icon_18_resampled <- disagg(icon_18[[1]], fact=2, method="bilinear")
icon_18_resampled<-crop(icon_18_resampled,nuble,mask=T)
