#' Buscar y filtrar estaciones meteorológicas
#'
#' Esta función permite buscar y filtrar estaciones meteorológicas basándose en criterios
#' como región, comuna, fecha de inicio de operaciones, institución y nombre de estación.
#' Retorna un objeto `SpatVector` con las coordenadas geográficas y atributos de las estaciones
#' que cumplen con los criterios especificados.
#'
#' @param region El nombre de la región donde buscar las estaciones meteorológicas.
#' Si se especifica `NULL` (valor por defecto), no se filtra por región.
#' @param comuna El nombre de la comuna donde buscar las estaciones meteorológicas.
#' Si se especifica `NULL` (valor por defecto), no se filtra por comuna.
#' @param Inicio Fecha de inicio de operaciones de la estación en formato Date o "YYYY-MM-DD HH:MM".
#' Si se especifica `NULL` (valor por defecto), no se filtra por fecha de inicio.
#' @param institucion Nombre de la institución a la que pertenece la estación meteorológica.
#' Si se especifica `NULL` (valor por defecto), no se filtra por institución.
#' @param nombre_estacion El nombre específico de la estación meteorológica a buscar.
#' Si se especifica `NULL` (valor por defecto), no se filtra por nombre de estación.
#'
#' @return Un objeto `SpatVector` que contiene las coordenadas geográficas y atributos
#' de las estaciones filtradas. Cada estación es un punto dentro del `SpatVector`, con
#' información adicional como código, nombre, elevación, comuna, región y fecha de la
#' primera lectura.
#'
#' @examples
#' # Ejemplo de uso sin filtrar por ningún criterio
#' coordenadas_estaciones()
#'
#' # Ejemplo de uso filtrando por región
#' coordenadas_estaciones(region = "Valparaíso")
#'
#' # Ejemplo de uso filtrando por región y comuna
#' coordenadas_estaciones(region = "Metropolitana", comuna = "Santiago")
#'
#' # Ejemplo de uso filtrando por institución y fecha de inicio
#' coordenadas_estaciones(institucion = "INIA", Inicio = ymd("2010-01-01"))
#'
#' # Ejemplo de uso filtrando por el nombre de la estación
#' coordenadas_estaciones(nombre_estacion = "Rapel")
#'
#' @import readr
#' @import dplyr
#' @import lubridate
#' @importFrom stringdist amatch
#' @importFrom terra vect


#' @export

coordenadas_estaciones <- function(region = NULL, comuna = NULL, Inicio = NULL, institucion = NULL, nombre_estacion = NULL) {
  archivo_datos <- system.file("extdata", "estaciones.csv", package = "iconDWD")

  if (!file.exists(archivo_datos)) {
    stop("Archivo de datos de estaciones meteorológicas no encontrado.")
  }

  datos_estaciones <- read_csv(archivo_datos, col_types = cols(
    institucion = col_character(),
    codigo = col_character(),
    nombre = col_character(),
    latitud = col_double(),
    longitud = col_double(),
    elevacion = col_double(),
    comuna = col_character(),
    region = col_character(),
    primera_lectura = col_character() # Leer como caracteres
  ))

  # Convertir 'primera_lectura' a POSIXct usando lubridate
  datos_estaciones$primera_lectura <- ymd_hm(datos_estaciones$primera_lectura)

  mensaje_filtro <- "Estaciones encontradas"

  # Aplicar filtros
  if (!is.null(region)) {
    region_cercana <- datos_estaciones$region[amatch(tolower(region), tolower(datos_estaciones$region), maxDist = 5)]
    datos_estaciones <- datos_estaciones %>% filter(region == region_cercana)
    mensaje_filtro <- paste(mensaje_filtro, "en la región", region_cercana)
  }

  if (!is.null(comuna)) {
    comuna_cercana <- datos_estaciones$comuna[amatch(tolower(comuna), tolower(datos_estaciones$comuna), maxDist = 5)]
    datos_estaciones <- datos_estaciones %>% filter(comuna == comuna_cercana)
    mensaje_filtro <- paste(mensaje_filtro, "y la comuna", comuna_cercana)
  }

  if (!is.null(institucion)) {
    datos_estaciones <- datos_estaciones %>% filter(tolower({{institucion}}) == tolower(institucion))
    mensaje_filtro <- paste(mensaje_filtro, "de la institución", institucion)
  }

  if (!is.null(nombre_estacion)) {
    datos_estaciones <- datos_estaciones %>% filter(tolower(nombre) == tolower(nombre_estacion))
    mensaje_filtro <- paste(mensaje_filtro, "con el nombre de estación", nombre_estacion)
  }

  if (!is.null(Inicio)) {
    fecha_filtrada <- Inicio
    if (!inherits(fecha_filtrada, "Date") && !inherits(fecha_filtrada, "POSIXt")) {
      fecha_filtrada <- ymd_hm(fecha_filtrada)
    }
    datos_estaciones <- datos_estaciones %>% filter(primera_lectura >= fecha_filtrada)
    mensaje_filtro <- paste(mensaje_filtro, "con inicio de funcionamiento después de", format(fecha_filtrada, "%Y-%m-%d %H:%M"))
  }

  # Crear el SpatVector con los puntos filtrados
  puntos <- vect(datos_estaciones, geom = c("longitud", "latitud"), crs = "+proj=longlat +datum=WGS84")

  # Añadir atributos al SpatVector
  for (col_name in names(datos_estaciones)) {
    puntos[[col_name]] <- datos_estaciones[[col_name]]
  }

  # Imprimir el mensaje con la cantidad de estaciones encontradas
  cat(nrow(datos_estaciones), mensaje_filtro, "\n")

  return(puntos)
}
