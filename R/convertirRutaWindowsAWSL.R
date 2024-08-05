#' Convertir Ruta de Windows a WSL
#'
#' Esta función toma una ruta de archivo de Windows y la convierte en una ruta compatible con el Subsistema de Windows para Linux (WSL).
#'
#' @param rutaWindows Una cadena de texto que representa la ruta de archivo en el formato de Windows, como "C:/Users/NombreUsuario/Documentos".
#' @return Una cadena de texto que representa la ruta de archivo convertida en el formato de WSL, como "/mnt/c/Users/NombreUsuario/Documentos".
#' @examples
#' convertirRutaWindowsAWSL("C:/Users/NombreUsuario/Documentos")
#' @export
#' @importFrom glue glue
#'
#' @details La función primero normaliza las barras de la ruta de Windows a barras hacia adelante.
#' Luego, extrae la letra de la unidad y el resto de la ruta para construir la nueva ruta en el formato de WSL.
#' Es importante notar que esta función asume que se está trabajando con el Subsistema de Windows para Linux y
#' que las rutas de unidades en WSL se montan bajo "/mnt/" seguido de la letra de la unidad en minúsculas.
convertirRutaWindowsAWSL <- function(rutaWindows) {
  # Normalizar las barras a barras hacia adelante
  rutaNormalizada <- gsub("\\\\", "/", rutaWindows)

  # Reemplazar los espacios por '\ ' para escaparlos
  rutaNormalizada <- gsub(" ", "\\\\ ", rutaNormalizada)

  # Extraer la letra de la unidad y el resto de la ruta
  partes <- strsplit(rutaNormalizada, ":/", fixed = TRUE)[[1]]
  letraUnidad <- tolower(partes[1])
  restoRuta <- partes[2]

  # Construir la ruta de WSL usando glue
  rutaWSL <- glue::glue('/mnt/{letraUnidad}/{restoRuta}')

  return(rutaWSL)
}
