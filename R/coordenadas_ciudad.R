#' Obtener Coordenadas de una Localidad mediante la API de Nominatim
#'
#' Esta función consulta la API de Nominatim de OpenStreetMap para obtener las
#' coordenadas geográficas (latitud y longitud) de una localidad especificada por
#' el usuario. Las coordenadas se devuelven en un formato que puede ser utilizado
#' directamente con la función `extract` del paquete `terra`.
#'
#' @param localidad Una cadena de caracteres que especifica la localidad para la cual
#' se desean obtener las coordenadas. La cadena debe ser suficientemente descriptiva
#' para ser localizada por la API de Nominatim (e.g., "Los Angeles, Chile").
#'
#' @return Un objeto `SpatVector` con el punto de las coordenadas (longitud y latitud) de la localidad.
#'
#' @examples
#' coords <- coordenadas_ciudad("Los Angeles, Chile")
#' print(coords)
#' plet(coords)
#'
#' @export
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom terra vect

coordenadas_ciudad <- function(localidad) {
  # Define la URL base de la API de Nominatim
  base_url <- "https://nominatim.openstreetmap.org/search"

  # Prepara los parámetros de la consulta
  params <- list(q = localidad, format = "json", limit = 1)

  # Realiza la llamada a la API
  response <- httr::GET(url = base_url, query = params)

  # Verifica que la solicitud fue exitosa
  if (httr::status_code(response) == 200) {
    # Procesa la respuesta JSON
    data <- jsonlite::fromJSON(httr::content(response, "text"))

    # Extrae la latitud y longitud
    if (length(data) > 0) {
      lat <- as.numeric(data$lat[[1]])
      lon <- as.numeric(data$lon[[1]])

      # Crear un objeto SpatVector con el punto de las coordenadas
      punto <- terra::vect(matrix(c(lon, lat), ncol = 2), type = "points", crs = "+proj=longlat +datum=WGS84")

      return(punto)
    } else {
      stop("No se encontraron coordenadas para la localidad especificada.")
    }
  } else {
    stop("Error en la solicitud a la API")
  }
}
