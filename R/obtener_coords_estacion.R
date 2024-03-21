#' Obtener Coordenadas de Estación Meteorológica
#'
#' Busca las coordenadas de una estación meteorológica en un archivo CSV incluido en el paquete,
#' permitiendo una búsqueda flexible del nombre de la estación.
#'
#' @param nombre_estacion Cadena de caracteres con el nombre de la estación meteorológica.
#' @importFrom readr read_csv
#' @importFrom stringdist stringdist
#' @export
obtener_coords_estacion <- function(nombre_estacion) {

  norm_nombre_estacion <- tolower(iconv(nombre_estacion, to = "ASCII//TRANSLIT"))

  archivo_datos <- system.file("extdata", "estaciones.csv", package = "grib2nc")

  if (!file.exists(archivo_datos)) {
    stop("Archivo de datos de estaciones meteorológicas no encontrado.")
  }

  datos_estaciones <- read_csv(archivo_datos, show_col_types = FALSE)

  datos_estaciones$nombre_norm <- tolower(iconv(datos_estaciones$nombre, to = "ASCII//TRANSLIT"))

  distancias <- stringdist::stringdist(norm_nombre_estacion, datos_estaciones$nombre_norm)

  if (all(is.na(distancias))) {
    warning("No se pudo calcular distancias. Verifica los nombres de las estaciones.")
    return(NULL)
  }

  indice_min_dist <- which.min(distancias)

  if (is.na(min(distancias, na.rm = TRUE)) || min(distancias, na.rm = TRUE) > nchar(norm_nombre_estacion) * 0.5) {
    warning("No se encontró una coincidencia cercana. Verifica el nombre de la estación.")
    return(NULL)
  }

  estacion <- datos_estaciones[indice_min_dist, ]
  lat <- estacion$latitud
  lon <- estacion$longitud

  # Mensaje de salida con el nombre, comuna y código de la estación
  mensaje <- sprintf("Coordenadas para '%s' (%s, %s): [%f, %f]",
                     estacion$nombre, estacion$comuna, estacion$codigo, lat, lon)
  print(mensaje)

  return(matrix(c(lon, lat), ncol = 2))
}
