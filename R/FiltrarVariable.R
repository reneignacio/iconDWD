#' Filtra archivos por variable y rango de fechas
#'
#' Esta función filtra archivos de proyecciones climáticas por una variable específica y un rango de fechas opcional.
#' Las variables admitidas en mayúsculas son: TMIN_2M, TMAX_2M, TOT_PREC, WW, CAPE_ML, T_2M.
#' Devuelve las rutas completas de los archivos que cumplen con los criterios especificados.
#'
#' @param dir Directorio que contiene los archivos a filtrar.
#' @param fecha_inicial Fecha de inicio del rango a filtrar en formato "YYYY-MM-DD HH:MM:SS" (opcional).
#'                      Si se omite, no se aplica filtro de fecha de inicio.
#' @param fecha_final Fecha final del rango a filtrar en formato "YYYY-MM-DD HH:MM:SS" (opcional).
#'                    Si se omite, no se aplica filtro de fecha final.
#' @param variable Nombre de la variable en mayúsculas por la cual filtrar, como TMIN_2M.
#' @return Un vector con las rutas completas de los archivos que cumplen con los criterios.
#' @examples
#' \dontrun{
#' FiltrarVariable(dir = "path/to/your/files",
#'                 fecha_inicial = "2024-03-19 00:00:00",
#'                 fecha_final = "2024-03-20 00:00:00",
#'                 variable = "T_2M")
#' }
#' @export
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr filter
FiltrarVariable <- function(dir, fecha_inicial = NA, fecha_final = NA, variable) {


  # Diccionario para relacionar nombre corto de la variable con nombre largo
  diccionario_variables <- list(
    T_2M = "Temperatura del aire a 2 metros de la superficie",
    TMIN_2M = "Temperatura mínima a 2 metros de la superficie",
    TMAX_2M = "Temperatura máxima a 2 metros de la superficie",
    TOT_PREC = "Precipitación total",
    WW = "Estado del tiempo",
    CAPE_ML = "Energía potencial convectiva disponible"
  )

  # Lista los archivos que coinciden con el patrón de la variable
  dir_files <- list.files(dir, pattern = paste0(variable, ".nc$"), full.names = TRUE)

  # Define una función para extraer información de cada archivo
  parse_file_info <- function(file_path) {
    # Obtiene solo el nombre del archivo de la ruta completa
    file_name <- basename(file_path)

    # Extrae la fecha, hora de ejecución y las horas de proyección desde el nombre del archivo
    execution_date_str <- substr(file_name, 38, 47)  # YYYYMMDDHH
    projection_hours_str <- substr(file_name, 49, 51)  # HHH

    # Convierte los strings a fecha/hora y número
    execution_date <- as.POSIXct(execution_date_str, format = "%Y%m%d%H", tz = "UTC")
    projection_hours <- as.numeric(projection_hours_str)

    # Calcula la fecha y hora de la proyección
    projection_date <- execution_date + lubridate::hours(projection_hours)

    return(data.frame(file_path = file_path, projection_date = projection_date))
  }

  # Aplica la función a cada archivo y crea un data frame
  info_list <- lapply(dir_files, parse_file_info)
  df <- do.call(rbind, info_list)

  # Asegura que la columna 'projection_date' sea de tipo POSIXct
  df$projection_date <- as.POSIXct(df$projection_date, tz = "UTC")

  # Extrae la hora de ejecución del modelo del primer archivo
  if (length(dir_files) > 0) {
    first_file_name <- basename(dir_files[1])
    execution_hour_str <- substr(first_file_name, 46, 47)  # HH

    # Extrae las fechas mínima y máxima disponibles
    fecha_min <- min(df$projection_date)
    fecha_max <- max(df$projection_date)

    # Busca el nombre largo de la variable
    nombre_largo <- ifelse(!is.null(diccionario_variables[[variable]]), diccionario_variables[[variable]], "Nombre desconocido")

    # Muestra el mensaje
    message(sprintf("Modelo ejecutado a las %s:00 UTC-0, datos disponibles desde %s hasta %s para %s (%s)",
                    execution_hour_str, format(fecha_min, "%Y-%m-%d %H:%M:%S"), format(fecha_max, "%Y-%m-%d %H:%M:%S"),
                    variable, nombre_largo))
  }

  # Devuelve las rutas de los archivos filtrados
  return(df$file_path)
}

