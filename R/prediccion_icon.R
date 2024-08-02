#' Cargar y Procesar Datos del Modelo ICON
#'
#' Esta función carga y procesa datos del modelo ICON según la variable especificada.
#' La función maneja tanto datos de temperatura como de precipitación, y ajusta los tiempos
#' de acuerdo con un desplazamiento UTC especificado.
#'
#' @param variable Character. Nombre de la variable a procesar. Las opciones incluyen
#' "TOT_PREC", "snow_gsp", "snow_con", "h_snow", "rain_con", "rain_gsp" para datos de precipitación,
#' y "T_2M", "TMIN_2M", "TMAX_2M" para datos de temperatura.
#' @param fecha_inicio Date. Fecha de inicio del rango de datos.
#' @param fecha_fin Date. Fecha de fin del rango de datos.
#' @param ruta Character. Ruta del directorio donde se encuentran los archivos .nc.
#' @param utc_offset Numeric. Desplazamiento UTC en horas para ajustar los tiempos de los datos.
#' @param hora_modelo Character. Hora del modelo, usualmente "00".
#' @param acum Logical. Indica si se debe realizar un acumulado diario para las variables de precipitación.
#' El valor predeterminado es FALSE.
#' @param ExtChile Logical. Indica si se debe recortar los datos a la extensión de Chile. El valor predeterminado es FALSE.
#'
#' @return Un objeto SpatRaster con los datos procesados. Si `acum` es TRUE, se devuelve el acumulado diario.
#'
#' @examples
#' ruta <- "E:/ICON_INFORME/ICON"
#' fecha_inicio <- as.Date("2024-06-05")
#' fecha_fin <- as.Date("2024-06-07")
#' variable <- "TOT_PREC"
#' hora_modelo <- "00"
#'
#' datos <- prediccion_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = TRUE, ExtChile = TRUE)
#'
#' print(datos)
#'
#' nuble <- cargar_regiones("R16")
#' nuble <- terra::project(nuble, datos)
#' icon_daily_nuble <- terra::crop(datos, nuble, mask = TRUE, touches = TRUE)
#'
#' plot(icon_daily_nuble)
#'
#' @export
#' @importFrom terra rast ext vect project crop time nlyr clamp tapp
#' @importFrom lubridate hours as_datetime

# Función principal para cargar y procesar datos del modelo ICON
prediccion_icon <- function(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = "00", acum = FALSE, ExtChile = FALSE) {

  # Función para mostrar la barra de progreso
  mostrar_progreso <- function(progreso, total, ancho = 50) {
    porcentaje <- round((progreso / total) * 100)
    completado <- round((progreso / total) * ancho)
    barra <- paste0("[", strrep("=", completado), strrep("-", ancho - completado), "] ", porcentaje, "%")
    cat("\r", barra, sep = "")
    if (progreso == total) cat("\n")
  }

  cat("Iniciando procesamiento...\n")

  # Formatear la fecha al formato de directorio necesario (YYYYMMDD)
  fecha_formateada <- format(fecha_inicio, "%Y%m%d")

  # Crear el patrón de búsqueda para los archivos
  pattern <- paste0(variable, "\\.nc$")

  # Construir el directorio de búsqueda
  dir_busqueda <- file.path(ruta, fecha_formateada, hora_modelo)

  # Utilizar list.files para buscar archivos que cumplan con el patrón en el directorio
  archivos_encontrados <- list.files(path = dir_busqueda, pattern = pattern, full.names = TRUE, recursive = TRUE)

  if (length(archivos_encontrados) == 0) {
    stop(paste("No se encontraron archivos para la fecha", fecha_formateada))
  }

  # Crear el raster con los archivos seleccionados
  icon_global <- terra::rast(archivos_encontrados)

  if (ExtChile) {
    # Definir la extensión de Chile
    chile <- terra::rast(ncol=100, nrow=100, xmin=-109.454916156, xmax=-66.4155940109999, ymin=-56.537765819, ymax=-17.498399336)
    extension_ajustada <- terra::ext(chile) + 1
    extension_chile <- terra::vect(extension_ajustada)
    terra::crs(extension_chile) <- "epsg:4326"
    extension_chile <- terra::project(extension_chile, icon_global)
    icon_global <- terra::crop(icon_global, extension_chile, touches = TRUE)
  }

  # Ajustar el tiempo del raster
  terra::time(icon_global) <- terra::time(icon_global) - lubridate::hours(utc_offset)

  # Convertir fecha_inicio y fecha_fin ajustadas a UTC
  fecha_inicio_utc <- lubridate::as_datetime(fecha_inicio, tz = "UTC")
  fecha_fin_utc <- lubridate::as_datetime(fecha_fin, tz = "UTC") + lubridate::hours(23)

  # Filtrar el raster para incluir solo las horas entre fecha_inicio y fecha_fin especificadas en UTC
  capas <- which(terra::time(icon_global) >= fecha_inicio_utc & terra::time(icon_global) <= fecha_fin_utc)
  if (length(capas) == 0) {
    stop("No se encontraron capas dentro del rango de fecha especificado.")
  }
  icon_global <- terra::subset(icon_global, capas)

  if (variable %in% c("TOT_PREC", "SNOW_GSP", "SNOW_CON", "H_SNOW", "RAIN_CON", "RAIN_GSP")) {
    # Crear un SpatRaster para las diferencias horarias
    iconn_hourly <- icon_global

    # Configurar la barra de progreso
    total_layers <- terra::nlyr(icon_global)
    cat("Procesando [")
    progress_width <- 50  # Ancho de la barra de progreso

    # Procesar todas las capas sin bloques
    for (j in 2:total_layers) {
      # Calcular la diferencia entre la capa actual y la capa anterior
      iconn_hourly[[j]] <- icon_global[[j]] - icon_global[[j - 1]]

      # Actualizar la barra de progreso
      mostrar_progreso(j - 1, total_layers - 1, progress_width)
    }

    cat("\n")

    # Eliminar los valores negativos (si los hay) resultantes de la diferencia
    iconn_hourly <- terra::clamp(iconn_hourly, lower = 0, values = TRUE)

    # La primera capa debe ser igual a la original ya que no hay capa previa para restar
    iconn_hourly[[1]] <- icon_global[[1]]

    # Liberar memoria del objeto original
    rm(icon_global)
    gc()

    # Asignar nombres a las capas basados en los tiempos
    names(iconn_hourly) <- format(terra::time(iconn_hourly), "%Y-%m-%d %H:%M:%S")

    if (acum) {
      # Realizar el acumulado diario
      icon_daily <- terra::tapp(iconn_hourly, "days", sum)

      # Liberar memoria del objeto intermedio
      rm(iconn_hourly)
      gc()

      return(icon_daily)
    } else {
      return(iconn_hourly)
    }
  } else if (variable %in% c("T_2M", "TMIN_2M", "TMAX_2M")) {
    return(icon_global)
  } else {
    stop("Variable no soportada")
  }
}
