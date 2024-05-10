#' Cargar Regiones Simplificadas
#'
#' Esta función permite cargar shapefiles simplificados de regiones específicas de Chile almacenados dentro del paquete `iconDWD`. La función utiliza un método de distancia de texto Jaro-Winkler para identificar la región más cercana basada en un nombre ingresado o un código de región directo, permitiendo así cierta flexibilidad en la entrada.
#'
#' @param regiones vector de caracteres opcional que especifica las regiones que se desean cargar. Puede incluir nombres aproximados de regiones o códigos de región directos (p. ej., "R08"). Si no se especifica, se cargará la lista completa de regiones.
#'
#' @return Una lista de objetos `SpatVector`, donde cada elemento corresponde a una región cargada y está nombrado según el código de región (p. ej., "R01", "R02", etc.). Cada `SpatVector` también se asigna a una variable global con el mismo nombre del código de región.
#'
#' @examples
#' # Cargar todas las regiones disponibles
#' todas_las_regiones <- cargar_regiones()
#'
#' # Cargar regiones específicas con nombres aproximados
#' regiones_especificas <- cargar_regiones(c("maule", "metropolitana", "valparaiso"))
#'
#' # Cargar una región específica por su código
#' region_R08 <- cargar_regiones("R08")
#'
#' @export
#' @importFrom terra vect
#' @importFrom stringdist stringdistmatrix
#'
#' @details La función busca archivos .shp en el directorio especificado dentro del paquete y utiliza el método de Jaro-Winkler para determinar la región más cercana a la entrada proporcionada. Si se encuentra una coincidencia válida dentro del umbral especificado, el shapefile correspondiente se carga como un objeto `SpatVector` y se asigna a una variable global con el nombre del código de la región. Esto facilita el acceso directo y la manipulación de los datos geográficos.

cargar_regiones <- function(regiones = NULL) {
  nombres_regiones <- list(
    "R01" = "Tarapacá",
    "R02" = "Antofagasta",
    "R03" = "Atacama",
    "R04" = "Coquimbo",
    "R05" = "Valparaíso",
    "R06" = "O'Higgins",
    "R07" = "Maule",
    "R08" = "Bío-Bío",
    "R09" = "Araucanía",
    "R10" = "Los Lagos",
    "R11" = "Aysén",
    "R12" = "Magallanes",
    "R13" = "Metropolitana",
    "R14" = "Los Ríos",
    "R15" = "Arica y Parinacota",
    "R16" = "Ñuble"
  )

  dir_salida <- system.file("extdata/regiones", package = "iconDWD")
  files <- list.files(dir_salida, pattern = "\\.shp$", full.names = TRUE)
  regiones_cargadas <- list()
  message("Buscando archivos .shp en el directorio: ", dir_salida)

  if (!is.null(regiones)) {
    regiones <- tolower(regiones)
    nombres_simplificados <- tolower(unlist(nombres_regiones))
    distancias <- stringdistmatrix(regiones, nombres_simplificados, method = "jw")
    indices <- apply(distancias, 1, function(d) {
      matches <- which(d == min(d))
      matches[d[matches] <= 0.1]
    })

    regiones_codigo_directo <- names(nombres_regiones)[tolower(names(nombres_regiones)) %in% regiones]
    regiones_a_cargar <- unique(c(regiones_codigo_directo, names(nombres_regiones)[unique(unlist(indices))]))

    for (codigo_region in regiones_a_cargar) {
      pattern <- paste0(codigo_region, "\\.shp$")
      archivo_region <- files[grepl(pattern, files)]
      if (length(archivo_region) > 0) {
        message("Cargando región: ", codigo_region, " desde archivo: ", archivo_region)
        vect_region <- vect(archivo_region)
        regiones_cargadas[[codigo_region]] <- vect_region
        assign(codigo_region, vect_region, envir = .GlobalEnv)
        message("Región ", codigo_region, " cargada y asignada a la variable global.")
      } else {
        message("No se encontró el archivo para la región: ", codigo_region)
      }
    }
  } else {
    message("No se especificaron regiones para cargar.")
  }

  return(regiones_cargadas)
}
