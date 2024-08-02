library(iconDWD)
library(glue)
library(R.utils)
library(terra)
library(lubridate)
library(readr)
library(dplyr)

# Definir la ruta base para guardar los archivos
ruta_save_tif <- "E:/INFORMES_PRECIPITACION_CODE_R/Informes_precipitacion/informe_pp_2024-06-17_a_2024-06-22_Ñuble/"

###################################################### #Precipitación ###############################################

# Parámetros
ruta <- "E:/ICON/descargas_icon"
fecha_inicio <- as.Date("2024-06-28")
fecha_fin <- as.Date("2024-07-03")
variable <- "TOT_PREC"
hora_modelo <- "00"
region <- "R16"

# Descargar y procesar datos
datos <- prediccion_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = T, block_size = 5, ExtChile = T)
Maule <- cargar_regiones(region)
Maule <- project(Maule, datos)
icon_daily_Maule <- crop(datos, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule <- disagg(icon_daily_Maule, fact = 100, method = "near")
icon_daily_Maule <- crop(icon_daily_Maule, Maule, mask = TRUE, touches = TRUE)


# Guardar cada capa como un archivo TIFF
for (i in 1:nlyr(icon_daily_Maule)) {
  capa <- icon_daily_Maule[[i]]
  fecha_capa <- as.Date(fecha_inicio + (i - 1))
  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_{variable}.tif")
  writeRaster(capa, filename = nombre_archivo, overwrite = TRUE)
}

print("Archivos TIFF guardados exitosamente.")

###################################################### #Temperatura Minima ###############################################

# Parámetros
ruta <- "E:/ICON/descargas_icon"
fecha_inicio <- as.Date("2024-06-28")
fecha_fin <- as.Date("2024-07-03")
variable <- "TMIN_2M"
hora_modelo <- "00"
region <- "R16"

# Descargar y procesar datos
datos <- prediccion_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, block_size = 5, ExtChile = T)
Maule <- cargar_regiones(region)
Maule <- project(Maule, datos)
icon_Maule <- crop(datos, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule <- tapp(icon_Maule, "days", min)
icon_daily_Maule<-icon_daily_Maule-273.15

icon_daily_Maule <- disagg(icon_daily_Maule, fact = 100, method = "near")
icon_daily_Maule <- crop(icon_daily_Maule, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule
plot(icon_daily_Maule)



# Guardar cada capa como un archivo TIFF
for (i in 1:nlyr(icon_daily_Maule)) {
  capa <- icon_daily_Maule[[i]]
  fecha_capa <- as.Date(fecha_inicio + (i - 1))
  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_{variable}.tif")
  writeRaster(capa, filename = nombre_archivo, overwrite = TRUE)
}

print("Archivos TIFF guardados exitosamente.")


###################################################### #Velocidad del viento U ###############################################

# Parámetros
ruta <- "E:/viento_19-21"
fecha_inicio <- as.Date("2024-06-20")
fecha_fin <- as.Date("2024-06-21")
variable <- "V_max"
hora_modelo <- "00"
region <- "R07"

# Descargar y procesar datos
datos<-cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, ExtChile = T)
datos


terra::plet(datos)
terra::summary(datos)


Maule <- cargar_regiones(region)
Maule <- project(Maule, datos)
icon_Maule <- crop(datos, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule <- tapp(icon_Maule, "days", max)
plet(icon_daily_Maule)

icon_daily_Maule <- disagg(icon_daily_Maule, fact = 100, method = "near")
icon_daily_Maule <- crop(icon_daily_Maule, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule
plot(icon_daily_Maule)



# Guardar cada capa como un archivo TIFF
for (i in 1:nlyr(icon_daily_Maule)) {
  capa <- icon_daily_Maule[[i]]
  fecha_capa <- as.Date(fecha_inicio + (i - 1))
  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_{variable}.tif")
  writeRaster(capa, filename = nombre_archivo, overwrite = TRUE)
}

print("Archivos TIFF guardados exitosamente.")







