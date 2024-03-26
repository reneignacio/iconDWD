#' Extraer Valores y Fechas de un SpatRaster para Ubicaciones Específicas
#'
#' Esta función extrae los valores de un SpatRaster para una localización dada (punto o SpatVector)
#' y los formatea en un dataframe con las correspondientes fechas y valores. Soporta argumentos adicionales
#' de la función `extract` de `terra`, como `fun`, `touches`, entre otros.
#'
#' @param spatraster El objeto SpatRaster del cual se extraerán los valores.
#' @param coords Coordenadas de la localización para la extracción, que pueden ser un punto (matrix o data.frame con
#' columnas x, y) o un SpatVector (puntos, líneas, polígonos).
#' @param fun (Opcional) Función para resumir los datos extraídos por línea o geometría de polígono.
#' @param touches (Opcional) Lógico, si TRUE, se extraen valores para todas las celdas tocadas por líneas o polígonos.
#' @param ... Argumentos adicionales para la función `extract`.
#'
#' @return Un dataframe con dos columnas: Fecha y Valor, representando las fechas de cada capa
#' del SpatRaster y los valores extraídos para la localización dada.
#'
#' @examples
#' coords_estacion <- obtener_coords_estacion(nombre_estacion = "Ce arroz")
#' valores_y_fechas <- extraer_valores_y_fechas(icon_18, coords_estacion)
#' print(valores_y_fechas)
#'
#' @export
#'
extraer_valores_icon <- function(spatraster, coords, ...) {
  # Asegurarse de que terra está cargado
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("El paquete 'terra' es necesario para esta función.")
  }

  # Recolectar todos los argumentos adicionales en una lista
  args_list <- list(...)

  # Añadir 'spatraster' y 'coords' a la lista de argumentos
  args_list$x <- spatraster
  args_list$y <- coords

  # Llamar a terra::extract pasando dinámicamente los argumentos
  valores_y_fechas <- do.call(terra::extract, args_list)

  # Realizar el procesamiento para formatear los resultados
  # Nota: El siguiente código puede necesitar ajustes basado en la estructura exacta de 'valores_y_fechas'
  if (!is.null(args_list$fun)) {
    df_valores_y_fechas <- as.data.frame(valores_y_fechas)
  } else {
    df_valores_y_fechas <- as.data.frame(t(valores_y_fechas))
  }

  # Asociar fechas a los datos extraídos, si aplica
  fechas <- terra::time(spatraster)
  num_filas <- if (!is.null(args_list$fun)) length(valores_y_fechas) else nrow(df_valores_y_fechas)
  df_valores_y_fechas$FechaHora <- rep(fechas, each = num_filas / length(fechas))

  # Crear el dataframe final
  if (!is.null(args_list$fun)) {
    df_final <- data.frame(Fecha = df_valores_y_fechas$FechaHora, Valor = df_valores_y_fechas$layer.1)
  } else {
    df_final <- data.frame(Fecha = df_valores_y_fechas$FechaHora, Valor = df_valores_y_fechas$V1)
  }

  return(df_final)
}
