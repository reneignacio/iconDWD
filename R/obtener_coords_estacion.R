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

  archivo_datos <- system.file("extdata", "estaciones.csv", package = "iconDWD")

  if (!file.exists(archivo_datos)) {
    stop("Archivo de datos de estaciones meteorológicas no encontrado.")
  }

  datos_estaciones <- readr::read_csv(archivo_datos, show_col_types = FALSE)

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

  # Crear un data.frame para hacer un SpatVector de puntos
  df_puntos <- data.frame(lon = estacion$longitud, lat = estacion$latitud)

  # Crear el SpatVector de puntos
  puntos <- vect(df_puntos, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

  # Añadir el resto de atributos al SpatVector
  for (nombre in names(estacion)) {
    puntos[[nombre]] <- estacion[[nombre]]
  }

  return(puntos)
}
