#' Cargar y Procesar Datos del Modelo ICON
#'
#' Esta función carga y procesa datos del modelo ICON según la variable especificada. La función maneja tanto datos de temperatura como de precipitación, y ajusta los tiempos de acuerdo con un desplazamiento UTC especificado.
#'
#' @param variable Character. Nombre de la variable a procesar. Las opciones incluyen "TOT_PREC", "snow_gsp", "snow_con", "h_snow", "rain_con", "rain_gsp" para datos de precipitación, y "T_2M", "TMIN_2M", "TMAX_2M" para datos de temperatura.
#' @param fecha_inicio Date. Fecha de inicio del rango de datos.
#' @param fecha_fin Date. Fecha de fin del rango de datos.
#' @param ruta Character. Ruta del directorio donde se encuentran los archivos .nc.
#' @param utc_offset Numeric. Desplazamiento UTC en horas para ajustar los tiempos de los datos.
#' @param hora_modelo Character. Hora del modelo, usualmente "00".
#' @param acum Logical. Indica si se debe realizar un acumulado diario para las variables de precipitación. El valor predeterminado es FALSE.
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
#' datos <- cargar_datos_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = hora_modelo, acum = TRUE, ExtChile = TRUE)
#'
#' print(datos)
#'
#' nuble <- cargar_regiones("R16")
#' nuble <- terra::project(nuble, datos)
#' icon_daily_nuble <- terra::crop(datos[[1]], nuble, mask = TRUE, touches = TRUE)
#'
#' plot(icon_daily_nuble)
#'
#' @export
#' @importFrom terra rast ext vect project crop clamp time tapp nlyr
#' @importFrom lubridate hours as_datetime days

cargar_datos_icon <- function(variable, fecha_inicio, fecha_fin, ruta, utc_offset = 4, hora_modelo = "00", acum = FALSE, ExtChile = FALSE) {

  # Definir la extensión de Chile
  chile <- terra::rast(ncol=100, nrow=100, xmin=-109.454916156, xmax=-66.4155940109999, ymin=-56.537765819, ymax=-17.498399336)
  extension_ajustada <- terra::ext(chile) + 1
  extension_chile <- terra::vect(extension_ajustada)
  terra::crs(extension_chile) <- "epsg:4326"

  # Función interna para procesar datos de precipitación
  cargar_datos_precipitacion_icon <- function(variable, fecha_inicio, fecha_fin, ruta, utc_offset, hora_modelo, acum, ExtChile) {

    # Ajustar la fecha_fin para incluir un día adicional
    fecha_fin <- fecha_fin + lubridate::days(1)
    # Generar todas las fechas en el rango
    fechas <- base::seq(from = fecha_inicio, to = fecha_fin, by = "days")

    # Formatear las fechas al formato de directorio necesario (YYYYMMDD)
    fechas_formateadas <- base::format(fechas, "%Y%m%d")

    # Crear el patrón de búsqueda para los archivos
    pattern <- base::paste0("_(0(0[1-9]|1[0-9]|2[0-4]))_", variable, "\\.nc$")

    # Inicializar una lista para almacenar los resultados
    archivos_seleccionados_lista <- base::list()

    # Bucle para buscar archivos en cada uno de los directorios de fechas
    for (i in base::seq_along(fechas_formateadas)) {
      fecha <- fechas_formateadas[i]
      # Construir el directorio de búsqueda
      dir_busqueda <- base::file.path(ruta, fecha, hora_modelo)

      # Utilizar base::list.files para buscar archivos que cumplan con el patrón en el directorio
      archivos_encontrados <- base::list.files(path = dir_busqueda, pattern = pattern, full.names = TRUE, recursive = TRUE)

      # Agregar los archivos encontrados a la lista de resultados
      if (base::length(archivos_encontrados) > 0) {
        archivos_seleccionados_lista[[i]] <- archivos_encontrados
      } else {
        base::warning(base::paste("Advertencia: No se encontraron archivos para la fecha", fecha))
      }
    }

    # Inicializar una lista para los SpatRaster diarios procesados
    icon_global_list <- base::list()

    # Procesar cada día por separado
    for (i in base::seq_along(archivos_seleccionados_lista)) {
      if (base::length(archivos_seleccionados_lista[[i]]) > 0) {
        # Cargar los archivos como un SpatRaster
        icon_global <- terra::rast(archivos_seleccionados_lista[[i]])

        if (ExtChile) {
          # Recortar a la extensión de Chile
          extension_chile <- terra::project(extension_chile, icon_global)
          icon_global <- terra::crop(icon_global, extension_chile, touches = TRUE)
        }

        # Crear un SpatRaster para las diferencias horarias
        iconn_hourly <- icon_global

        # Iterar sobre cada capa y calcular la diferencia para obtener valores instantáneos
        for (j in 2:terra::nlyr(icon_global)) {
          # Calcular la diferencia entre la capa actual y la capa anterior
          iconn_hourly[[j]] <- icon_global[[j]] - icon_global[[j - 1]]
        }

        # Eliminar los valores negativos (si los hay) resultantes de la diferencia
        iconn_hourly <- terra::clamp(iconn_hourly, lower = 0, values = TRUE)

        # La primera capa debe ser igual a la original ya que no hay capa previa para restar
        iconn_hourly[[1]] <- icon_global[[1]]

        # Ajustar el tiempo después de calcular las diferencias
        terra::time(iconn_hourly) <- terra::time(iconn_hourly) - lubridate::hours(utc_offset)

        # Agregar el SpatRaster procesado a la lista
        icon_global_list[[i]] <- iconn_hourly

        # Liberar memoria del objeto original
        base::rm(icon_global)
        base::gc()
      }
    }

    # Unir todos los SpatRaster procesados en uno solo usando Reduce con c
    icon_global_final <- base::Reduce(base::c, icon_global_list)

    # Convertir fecha_inicio y fecha_fin ajustada a UTC
    fecha_inicio_utc <- lubridate::as_datetime(fecha_inicio)
    fecha_fin_utc <- lubridate::as_datetime(fecha_fin) - lubridate::days(1) + lubridate::hours(23)

    # Filtrar el raster para incluir solo las horas entre 00:00 y 23:00 del día específico en UTC
    icon_global_subset <- terra::subset(icon_global_final, base::which(terra::time(icon_global_final) >= fecha_inicio_utc & terra::time(icon_global_final) <= fecha_fin_utc))

    # Verificar si algún día no tiene todos sus datos
    horas_por_dia <- base::diff(terra::time(icon_global_subset))
    dias_incompletos <- base::which(horas_por_dia != 1)

    if (base::length(dias_incompletos) > 0) {
      base::warning(base::paste("Advertencia: Los siguientes días no tienen todos sus datos hasta las 23:00:", fechas[dias_incompletos]))
    }

    if (acum) {
      # Realizar el acumulado diario
      icon_daily <- terra::tapp(icon_global_subset, "days", base::sum)
      return(icon_daily)
    } else {
      return(icon_global_subset)
    }
  }

  # Función interna para procesar datos de temperatura
  cargar_datos_temperatura_icon <- function(variable, fecha_inicio, fecha_fin, ruta, utc_offset, hora_modelo, ExtChile) {
    # Ajustar la fecha_fin para incluir un día adicional
    fecha_fin <- fecha_fin + lubridate::days(1)

    # Generar todas las fechas en el rango
    fechas <- base::seq(from = fecha_inicio, to = fecha_fin, by = "days")

    # Formatear las fechas al formato de directorio necesario (YYYYMMDD)
    fechas_formateadas <- base::format(fechas, "%Y%m%d")

    # Crear el patrón de búsqueda para los archivos
    pattern <- base::paste0("_(0(0[1-9]|1[0-9]|2[0-4]))_", variable, "\\.nc$")

    # Inicializar un vector para almacenar los resultados
    archivos_seleccionados <- c()

    # Bucle para buscar archivos en cada uno de los directorios de fechas
    for (fecha in fechas_formateadas) {
      # Construir el directorio de búsqueda
      dir_busqueda <- base::file.path(ruta, fecha, hora_modelo)

      # Utilizar base::list.files para buscar archivos que cumplan con el patrón en el directorio
      archivos_encontrados <- base::list.files(path = dir_busqueda, pattern = pattern, full.names = TRUE, recursive = TRUE)

      # Agregar los archivos encontrados al vector de resultados
      if (base::length(archivos_encontrados) > 0) {
        archivos_seleccionados <- c(archivos_seleccionados, archivos_encontrados)
      }
    }

    # Crear el raster con los archivos seleccionados
    icon_global <- terra::rast(archivos_seleccionados)

    if (ExtChile) {
      # Recortar a la extensión de Chile
      extension_chile <- terra::project(extension_chile, icon_global)
      icon_global <- terra::crop(icon_global, extension_chile, touches = TRUE)
    }

    # Ajustar el tiempo del raster
    terra::time(icon_global) <- terra::time(icon_global) - lubridate::hours(utc_offset)
    names(icon_global) <- terra::time(icon_global)

    # Definir las fechas de inicio y fin en hora UTC ajustadas
    fecha_inicio_utc <- lubridate::as_datetime(fecha_inicio)
    fecha_fin_utc <- lubridate::as_datetime(fecha_fin) - lubridate::days(1) + lubridate::hours(23)

    # Filtrar el raster para incluir solo las horas entre 00:00 y 23:00 del día específico en UTC
    icon_global_subset <- terra::subset(icon_global, base::which(terra::time(icon_global) >= fecha_inicio_utc & terra::time(icon_global) <= fecha_fin_utc))

    return(icon_global_subset)
  }

  # Variables que requieren el procesamiento específico de precipitación
  variables_precipitacion <- c("TOT_PREC", "SNOW_GSP", "SNOW_CON", "H_SNOW", "RAIN_CON", "RAIN_GSP")

  # Variables que requieren el procesamiento específico de temperatura
  variables_temperatura <- c("T_2M", "TMIN_2M", "TMAX_2M", "U_10M", "V_10M")

  if (variable %in% variables_precipitacion) {
    return(cargar_datos_precipitacion_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset, hora_modelo, acum, ExtChile))
  } else if (variable %in% variables_temperatura) {
    return(cargar_datos_temperatura_icon(variable, fecha_inicio, fecha_fin, ruta, utc_offset, hora_modelo, ExtChile))
  } else {
    base::stop("Variable no soportada")
  }
}
