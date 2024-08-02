# Cargar librerías necesarias
library(terra)



# Función para calcular la velocidad del viento
calcular_velocidad_viento <- function(u, v) {
  sqrt(u^2 + v^2)
}




# Cargar datos U y V
ruta <- "E:/viento_19-21"
ruta_save_tif<-ruta
fecha_inicio <- as.Date("2024-06-20")
fecha_fin <- as.Date("2024-06-21")
hora_modelo <- "00"
region <- "R07"

u_data <- cargar_datos_icon("U_10M", fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, ExtChile = T)
v_data <- cargar_datos_icon("V_10M", fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, ExtChile = T)

# Calcular velocidad del viento
velocidad_viento <- calcular_velocidad_viento(u_data, v_data)
# Calcular la velocidad máxima diaria usando tapp
vel_max_diaria <- tapp(velocidad_viento, "days", max)
Maule <- cargar_regiones(region)
Maule <- project(Maule, vel_max_diaria)
icon_daily_Maule<-vel_max_diaria

icon_daily_Maule <- crop(icon_daily_Maule, Maule, mask = TRUE, touches = TRUE)
icon_daily_Maule <- disagg(icon_daily_Maule, fact = 100, method = "near")
icon_daily_Maule <- crop(icon_daily_Maule, Maule, mask = TRUE, touches = TRUE)

icon_daily_Maule


icon_daily_Maule<-icon_daily_Maule * factor
plot(icon_daily_Maule)
# Guardar cada capa como un archivo TIFF
for (i in 1:nlyr(icon_daily_Maule)) {
  capa <- icon_daily_Maule[[i]]
  fecha_capa <- as.Date(fecha_inicio + (i - 1))
  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_vmax4.tif")
  writeRaster(capa, filename = nombre_archivo, overwrite = TRUE)
}





##################################### ÑUBLE #####################################


# Cargar datos U y V
ruta <- "E:/viento_19-21"
fecha_inicio <- as.Date("2024-06-20")
fecha_fin <- as.Date("2024-06-21")
hora_modelo <- "00"
region <- "R16"

u_data <- cargar_datos_icon("U_10M", fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, ExtChile = T)
v_data <- cargar_datos_icon("V_10M", fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = F, ExtChile = T)

# Calcular velocidad del viento
velocidad_viento <- calcular_velocidad_viento(u_data, v_data)
# Calcular la velocidad máxima diaria usando tapp
vel_max_diaria <- tapp(velocidad_viento, "days", max)
Ñuble <- cargar_regiones(region)
Ñuble <- project(Ñuble, velocidad_viento)
icon_daily_Ñuble<-vel_max_diaria
icon_daily_Ñuble

icon_daily_Ñuble <- crop(icon_daily_Ñuble, Ñuble, mask = TRUE, touches = TRUE)
icon_daily_Ñuble <- disagg(icon_daily_Ñuble, fact = 100, method = "near")
icon_daily_Ñuble <- crop(icon_daily_Ñuble, Ñuble, mask = TRUE, touches = TRUE)
icon_daily_Ñuble
plot(icon_daily_Ñuble)


icon_daily_Nuble<-icon_daily_Ñuble*3.6 * (2/10)^0.2 * 1.3402+3.6639
icon_daily_Nuble
plot(icon_daily_Ñuble)


   ifel(icon_daily_Nuble > 40, 1, 0)


# Guardar cada capa como un archivo TIFF
for (i in 1:nlyr(icon_daily_Nuble)) {
  capa <- icon_daily_Nuble[[i]]
  fecha_capa <- as.Date(fecha_inicio + (i - 1))
  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_vmax6.tif")
  #writeRaster(capa, filename = nombre_archivo, overwrite = TRUE)

  nombre_archivo <- glue("{ruta_save_tif}/{fecha_capa}_{region}_vmax6_recla_40.tif")
  icon_daily_Nuble_recla<-ifel(icon_daily_Nuble[[i]] > 40, 1, 0)
  writeRaster(icon_daily_Nuble_recla, filename = nombre_archivo, overwrite = TRUE)
}
