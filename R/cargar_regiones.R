#' Cargar Regiones Simplificadas
#'
#' Esta función permite cargar shapefiles simplificados de regiones específicas de Chile almacenados dentro del paquete `iconDWD`.
#'
#' @param regiones Vector de caracteres opcional que especifica las regiones que se desean cargar.
#' @return Un objeto `SpatVector` si se carga una sola región o todas las regiones unidas, o una lista de objetos `SpatVector` si se cargan múltiples regiones.
#' @export

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

  # Obtener el directorio de los shapefiles dentro del paquete
  dir_salida <- base::system.file("extdata/regiones", package = "iconDWD")

  files <- base::list.files(dir_salida, pattern = "\\.shp$", full.names = TRUE)

  # Verificar que existan archivos .shp en el directorio
  if (length(files) == 0) {
    base::stop("No se encontraron archivos .shp en el directorio especificado.")
  }

  # Si no se especifican regiones o se especifica "chile", se cargan todas
  if (base::is.null(regiones) || base::any(base::tolower(regiones) == "chile")) {
    regiones <- base::names(nombres_regiones)
  } else {
    regiones <- base::as.character(regiones)
  }

  # Verificar las regiones válidas a cargar
  regiones_a_cargar <- base::intersect(regiones, base::names(nombres_regiones))

  # Verificar que haya regiones válidas para cargar
  if (base::length(regiones_a_cargar) == 0) {
    base::stop("No se especificaron regiones válidas para cargar.")
  }

  # Lista para almacenar los SpatVector cargados
  regiones_cargadas <- base::list()

  # Cargar los shapefiles correspondientes a cada región
  for (codigo_region in regiones_a_cargar) {
    pattern <- base::paste0(codigo_region, "\\.shp$")
    archivo_region <- base::grep(pattern, files, value = TRUE, ignore.case = TRUE)
    if (base::length(archivo_region) > 0) {
      vect_region <- base::tryCatch({
        terra::vect(archivo_region)
      }, error = function(e) {
        NULL
      })
      if (!base::is.null(vect_region)) {
        regiones_cargadas[[codigo_region]] <- vect_region
      }
    }
  }

  # Unir todas las regiones si se solicitó "chile"
  if (base::any(base::tolower(regiones) == "chile")) {
    todas_regiones <- base::tryCatch({
      base::Reduce(function(x, y) rbind(x, y), regiones_cargadas)
    }, error = function(e) {
      base::stop("No se pudieron unir las regiones:", e$message)
    })
    return(todas_regiones)
  }

  # Retornar un único SpatVector si se carga una sola región
  if (base::length(regiones_cargadas) == 1) {
    return(regiones_cargadas[[1]])
  }

  # Retornar la lista de regiones cargadas si no se solicitó "chile"
  if (base::length(regiones_cargadas) > 0) {
    return(regiones_cargadas)
  } else {
    base::stop("No se pudieron cargar las regiones especificadas.")
  }
}
