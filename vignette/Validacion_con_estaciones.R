library(iconDWD)
library(readr)
library(terra)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)

# Función para formatear los datos
formatear_datos <- function(df) {
  df_formateado <- as.data.frame(t(df))
  colnames(df_formateado) <- rownames(df)
  df_formateado <- df_formateado %>%
    rownames_to_column(var = "Code") %>%
    as_tibble()
  colnames(df_formateado)[-1] <- format(as.Date(gsub("d_", "", colnames(df_formateado)[-1]), format = "%Y.%m.%d"), "%d-%m-%Y")
  return(df_formateado)
}

# Leer las fechas de los eventos
fechas_eventos <- read_csv("E:/EVENTOS_ICON/rango_de_fechas_eventos.csv")
estaciones <- vect("E:/EVENTOS_ICON/estaciones/TA_MAX_R16_22012024_a_25012024.shp")
estaciones <- estaciones[,1:6]

errores <- list()

# Iterar sobre cada rango de fechas y cada variable
for (i in 1:nrow(fechas_eventos)) {
  evento <- fechas_eventos$Evento[i]
  fecha_inicio <- as.Date(fechas_eventos$fecha_inicio[i], format = "%d-%m-%Y")
  fecha_fin <- as.Date(fechas_eventos$fecha_fin[i], format = "%d-%m-%Y")
  ruta <- paste0("E:/EVENTOS_ICON/datos_estimados_ICON_netcdf/", evento)

  print(paste0("Procesando evento: ", evento))
  print(paste0("Fecha de inicio: ", fecha_inicio))
  print(paste0("Fecha de fin: ", fecha_fin))

  # Precipitación
  tryCatch({
    variable <- "TOT_PREC"
    datos <- cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = "00", acum = TRUE, ExtChile = TRUE)
    if (!is.null(datos)) {
      print("Datos de precipitación cargados:")
      print(datos)
      nuble <- cargar_regiones("R16")
      nuble <- project(nuble, datos)
      icon_daily_nuble <- crop(datos, nuble, mask = TRUE, touches = TRUE)
      pp_daily_ICON <- t(terra::extract(icon_daily_nuble, estaciones, ID = FALSE))
      colnames(pp_daily_ICON) <- estaciones$Code
      pp_daily_ICON <- formatear_datos(pp_daily_ICON)
      dir.create(paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento), recursive = TRUE, showWarnings = FALSE)
      write_csv(pp_daily_ICON, paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento, "/ICON_PP_SUM_", evento, ".csv"))
    } else {
      print(paste0("No se encontraron datos de precipitación para el evento: ", evento))
    }
  }, error = function(e) {
    errores[[length(errores) + 1]] <- list(evento = evento, variable = "TOT_PREC", error = e)
  })

  # Temperatura máxima
  tryCatch({
    variable <- "TMAX_2M"
    t_max_datos <- cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = "00", acum = FALSE, ExtChile = TRUE)
    if (!is.null(t_max_datos)) {
      print("Datos de temperatura máxima cargados:")
      print(t_max_datos)
      nuble <- cargar_regiones("R16")
      nuble <- project(nuble, t_max_datos)
      t_max_icon <- crop(t_max_datos, nuble, mask = TRUE, touches = TRUE)
      t_max_icon <- t_max_icon - 273.16
      t_max_icon_daily <- tapp(t_max_icon, "days", max)
      t_max_icon_daily <- t(terra::extract(t_max_icon_daily, estaciones, ID = FALSE))
      colnames(t_max_icon_daily) <- estaciones$Code
      t_max_icon_daily <- formatear_datos(t_max_icon_daily)
      dir.create(paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento), recursive = TRUE, showWarnings = FALSE)
      write_csv(t_max_icon_daily, paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento, "/ICON_TA_MAX_", evento, ".csv"))
    } else {
      print(paste0("No se encontraron datos de temperatura máxima para el evento: ", evento))
    }
  }, error = function(e) {
    errores[[length(errores) + 1]] <- list(evento = evento, variable = "TMAX_2M", error = e)
  })

  # Temperatura mínima
  tryCatch({
    variable <- "TMIN_2M"
    t_min_datos <- cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = "00", acum = FALSE, ExtChile = TRUE)
    if (!is.null(t_min_datos)) {
      print("Datos de temperatura mínima cargados:")
      print(t_min_datos)
      nuble <- cargar_regiones("R16")
      nuble <- project(nuble, t_min_datos)
      t_min_icon <- crop(t_min_datos, nuble, mask = TRUE, touches = TRUE)
      t_min_icon <- t_min_icon - 273.16
      t_min_icon_daily <- tapp(t_min_icon, "days", min)
      t_min_icon_daily <- t(terra::extract(t_min_icon_daily, estaciones, ID = FALSE))
      colnames(t_min_icon_daily) <- estaciones$Code
      t_min_icon_daily <- formatear_datos(t_min_icon_daily)
      dir.create(paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento), recursive = TRUE, showWarnings = FALSE)
      write_csv(t_min_icon_daily, paste0("E:/EVENTOS_ICON/datos_estimados_ICON_estaciones/", evento, "/ICON_TA_MIN_", evento, ".csv"))
    } else {
      print(paste0("No se encontraron datos de temperatura mínima para el evento: ", evento))
    }
  }, error = function(e) {
    errores[[length(errores) + 1]] <- list(evento = evento, variable = "TMIN_2M", error = e)
  })
}

# Mostrar los errores al final
if (length(errores) > 0) {
  print("Errores encontrados durante la ejecución:")
  for (error in errores) {
    print(paste0("Evento: ", error$evento, ", Variable: ", error$variable))
    print(error$error)
  }
} else {
  print("Ejecución completada sin errores.")
}

# Verificar los datos formateados
print("Datos de precipitación:")
if (exists("pp_daily_ICON")) print(head(pp_daily_ICON, 3))
print("Datos de temperatura máxima:")
if (exists("t_max_icon_daily")) print(head(t_max_icon_daily, 3))
print("Datos de temperatura mínima:")
if (exists("t_min_icon_daily")) print(head(t_min_icon_daily, 3))
