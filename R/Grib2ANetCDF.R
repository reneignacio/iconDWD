#' Convertir archivos .grib2 a .nc
#'
#' Esta función convierte archivos de formato .grib2 a formato .nc (NetCDF) utilizando
#' una llamada al sistema para ejecutar un script de bash en WSL. La función puede
#' operar en modo secuencial o paralelo.
#'
#' @param ruta_in Vector de caracteres con las rutas de los archivos de entrada .grib2.
#' @param ruta_script Ruta del script dentro de wsl("/home/...")
#' @param parallel Lógico, indica si la conversión debe realizarse en paralelo. Por defecto es `FALSE`.
#' @param ncores Entero, número de núcleos a utilizar para la conversión en paralelo. Solo aplica si `parallel` es `TRUE`. Por defecto, usa todos los núcleos disponibles menos uno.
#' @param verbose Lógico, indica si la función debe imprimir mensajes sobre el progreso de la conversión. Por defecto es `TRUE`.
#'
#' @details
#' Para el funcionamiento en modo paralelo, esta función depende del paquete `doSNOW` y
#' configura un clúster SOCK para la ejecución paralela de las conversiones. La conversión
#' de rutas de Windows a WSL se realiza mediante la función `convertirRutaWindowsAWSL`,
#' que debe estar definida previamente o ser parte del mismo paquete.
#'
#' La barra de progreso y el cálculo del tiempo estimado se añaden para mejorar la
#' interacción del usuario con la función, especialmente útil para conversiones de
#' larga duración.
#'
#' @return
#' La función no retorna un objeto. Los archivos .nc son escritos en las rutas especificadas
#' por `ruta_out`.
#'
#' @examples
#' \dontrun{
#' ruta_in <- c("/path/to/input/file1.grib2", "/path/to/input/file2.grib2")
#' ruta_script_wsl <- "/home/inia/ICON_0125/transform_0125.sh"
#' Grib2ANetCDF(ruta_in, ruta_out, ruta_script, parallel = TRUE, verbose = TRUE)
#' }
#'
#' @export
#' @importFrom glue glue
#' @import doSNOW
#' @import pbapply
#' @import R.utils

Grib2ANetCDF <- function(ruta_in,ruta_script, parallel = FALSE, ncores = detectCores() - 3, verbose = TRUE) {

  generarRutaSalida <- function(ruta) {
    # Asumimos la estructura de directorios y cambiamos la extensión de .grib2 a .nc
    ruta_salida <- sub("\\.grib2$", ".nc", ruta)
    return(ruta_salida)
  }

  ruta_out <<- sapply(ruta_in, generarRutaSalida, USE.NAMES = FALSE)

  modificarRutaSalida <- function(ruta) {
    separador <- .Platform$file.sep# Usar un separador adecuado para Windows
    partes <- unlist(strsplit(ruta, separador, fixed = TRUE))
    longitud <- length(partes)
    ruta_dir_padre <- do.call(file.path, as.list(partes[1:(longitud - 1)]))# Reconstruir la ruta hasta el directorio padre del archivo final
    ruta_dir_padre_netCDF <- file.path(ruta_dir_padre, "NetCDF")# Añadir "NetCDF" al directorio padre
    if (!dir.exists(ruta_dir_padre_netCDF)) {# Crear el directorio "NetCDF" si no existe
      dir.create(ruta_dir_padre_netCDF, recursive = TRUE, showWarnings = TRUE)
    }
    ruta_modificada <- file.path(ruta_dir_padre_netCDF, partes[longitud])# Construir la ruta modificada con "NetCDF" incluido
    cat("Ruta modificada:", ruta_modificada, "\n")    # Imprimir la ruta completa modificada
    return(ruta_modificada)
  }


  tiempo_inicio <- Sys.time() # Para estimar el tiempo total
  ruta_in<<-ruta_in
  ruta_script<<-ruta_script
  ruta_base_script<<- dirname(ruta_script)

  if (!parallel) {
    if (verbose) cat(glue("Iniciando la conversión de {length(ruta_in)} archivo(s) de .grib2 a .nc...\n"))

    for (i in seq_along(ruta_in)) {
      if (verbose) cat(glue("Procesando {ruta_in[i]}...\n"))

      rutaWSL_in <- convertirRutaWindowsAWSL(ruta_in[i])
      ruta_out_modificada <- modificarRutaSalida(ruta_out[i])
      rutaWSL_out <- convertirRutaWindowsAWSL(ruta_out_modificada)
      comando <- glue("wsl bash -c ' {ruta_script} {rutaWSL_in} {rutaWSL_out} {ruta_base_script} '")
      system(comando)

      # Actualizar al usuario sobre el progreso sin usar pbapply
      if (verbose) cat(glue("Progreso: {i}/{length(ruta_in)}\n"))
    }
    if (verbose) cat("Conversión completada.\n")
  }

  else {  #PARALELO
    if (verbose) cat(glue("Iniciando la conversión paralela de {length(ruta_in)} archivo(s) de .grib2 a .nc utilizando {ncores} núcleos...\n"))
    cl <- makeCluster(ncores, type = "SOCK")
    registerDoSNOW(cl)

    # En el modo paralelo, asegúrate de exportar correctamente la variable ruta_script
    clusterExport(cl, varlist = c("convertirRutaWindowsAWSL", "glue","ruta_base_script","ruta_out"))


    clusterEvalQ(cl, {
      library(glue)
      library(R.utils)
      library(pbapply)
      library(doSNOW)
    })

    # Usar pbapply en lugar de foreach para la barra de progreso
    resultados <- pbapply::pblapply(1:length(ruta_in), function(i) {
      rutaWSL_in_i <- convertirRutaWindowsAWSL(ruta_in[i])
      ruta_out_modificada_i <- modificarRutaSalida(ruta_out[i])
      rutaWSL_out_i <- convertirRutaWindowsAWSL(ruta_out_modificada_i)

      print(ruta_base_script)
      comando_i <- glue("wsl bash -c ' {ruta_script} {rutaWSL_in_i} {rutaWSL_out_i} {ruta_base_script} '")
      system(comando_i)
      if (verbose) glue("Archivo {ruta_in[i]} procesado.\n") else NULL
    }, cl = cl)

    stopCluster(cl)
    if (verbose) cat("Conversión paralela completada.\n")
  }

  tiempo_fin <- Sys.time() # Tiempo final
  tiempo_total <- tiempo_fin - tiempo_inicio # Calcular el tiempo total de ejecución
  if (verbose) cat(glue("Tiempo total de ejecución: {round(tiempo_total, 2)} segundos\n"))
}

