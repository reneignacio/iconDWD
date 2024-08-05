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
#' Grib2ANetCDF(ruta_in, ruta_script, parallel = TRUE, verbose = TRUE)
#' }
#'
#' @export
#' @importFrom glue glue
#' @import doSNOW
#' @import pbapply
#' @import R.utils

Grib2ANetCDF <- function(ruta_in, ruta_script, parallel = FALSE, ncores = parallel::detectCores() - 3, verbose = TRUE) {

  generarRutaSalida <- function(ruta) {
    ruta_salida <- sub("\\.grib2$", ".nc", ruta)
    return(ruta_salida)
  }

  modificarRutaSalida <- function(ruta) {
    separador <- .Platform$file.sep
    partes <- base::unlist(base::strsplit(ruta, separador, fixed = TRUE))
    longitud <- base::length(partes)
    ruta_dir_padre <- base::do.call(base::file.path, as.list(partes[1:(longitud - 1)]))
    ruta_dir_padre_netCDF <- base::file.path(ruta_dir_padre, "NetCDF")
    if (!base::dir.exists(ruta_dir_padre_netCDF)) {
      base::dir.create(ruta_dir_padre_netCDF, recursive = TRUE, showWarnings = TRUE)
    }
    ruta_modificada <- base::file.path(ruta_dir_padre_netCDF, partes[longitud])
    return(ruta_modificada)
  }

  # Verificar si hay archivos en ruta_in
  if (base::length(ruta_in) == 0) {
    base::stop("No hay archivos .grib2 para procesar.")
  }

  ruta_out <<- base::sapply(ruta_in, generarRutaSalida, USE.NAMES = FALSE)
  ruta_out_modificada <<- base::sapply(ruta_out, modificarRutaSalida, USE.NAMES = FALSE)

  # Debug print
  base::print("Rutas de salida generadas:")
  base::print(ruta_out)
  base::print("Rutas de salida modificadas:")
  base::print(ruta_out_modificada)

  archivos_existentes <- base::file.exists(ruta_out_modificada)

  # Debug print
  base::print("Archivos existentes:")
  base::print(archivos_existentes)

  ruta_in <<- ruta_in[!archivos_existentes]
  ruta_out_modificada <<- ruta_out_modificada[!archivos_existentes]

  # Debug print
  base::print("Rutas de entrada después de filtrar archivos existentes:")
  base::print(ruta_in)
  base::print("Rutas de salida modificadas después de filtrar archivos existentes:")
  base::print(ruta_out_modificada)

  if (base::length(ruta_in) == 0) {
    if (verbose) base::cat("No hay archivos nuevos para procesar.\n")
    return()
  }

  tiempo_inicio <- base::Sys.time()
  ruta_base_script <<- base::dirname(ruta_script)
  ruta_script <<- ruta_script

  if (!parallel) {
    if (verbose) base::cat(glue::glue("Iniciando la conversión de {base::length(ruta_in)} archivo(s) de .grib2 a .nc...\n"))

    for (i in base::seq_along(ruta_in)) {
      if (verbose) base::cat(glue::glue("Procesando {ruta_in[i]}...\n"))

      rutaWSL_in <- convertirRutaWindowsAWSL(ruta_in[i])
      rutaWSL_out <- convertirRutaWindowsAWSL(ruta_out_modificada[i])
      comando <- glue::glue("wsl bash -c ' {ruta_script} {rutaWSL_in} {rutaWSL_out} {ruta_base_script} '")
      print(glue("la ruta de ruta_script es: {ruta_script}"))
      print(glue("la ruta de rutaWSL_in es: {rutaWSL_in}"))
      print(glue("la ruta de rutaWSL_out es: {rutaWSL_out}"))
      print(glue("la ruta de ruta_base_script es: {ruta_base_script}"))
      base::system(comando)

      if (verbose) base::cat(glue::glue("Progreso: {i}/{base::length(ruta_in)}\n"))
    }
    if (verbose) base::cat("Conversión completada.\n")
  } else {
    if (verbose) base::cat(glue::glue("Iniciando la conversión paralela de {base::length(ruta_in)} archivo(s) de .grib2 a .nc utilizando {ncores} núcleos...\n"))
    cl <- parallel::makeCluster(ncores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)

    # Exportar todas las variables necesarias para los nodos
    parallel::clusterExport(cl, varlist = c("convertirRutaWindowsAWSL", "glue", "ruta_base_script", "ruta_out_modificada", "ruta_script", "ruta_in", "verbose"))

    parallel::clusterEvalQ(cl, {
      library(glue)
      library(R.utils)
      library(pbapply)
      library(doSNOW)
    })

    resultados <- pbapply::pblapply(1:base::length(ruta_in), function(i) {
      rutaWSL_in_i <- convertirRutaWindowsAWSL(ruta_in[i])
      rutaWSL_out_i <- convertirRutaWindowsAWSL(ruta_out_modificada[i])
      comando_i <- glue::glue("wsl bash -c ' {ruta_script} {rutaWSL_in_i} {rutaWSL_out_i} {ruta_base_script} '")
      base::system(comando_i)
      if (verbose) glue::glue("Archivo {ruta_in[i]} procesado.\n") else NULL
    }, cl = cl)

    parallel::stopCluster(cl)
    if (verbose) base::cat("Conversión paralela completada.\n")
  }

  tiempo_fin <- base::Sys.time()
  tiempo_total <- tiempo_fin - tiempo_inicio
  if (verbose) base::cat(glue::glue("Tiempo total de ejecución: {base::round(tiempo_total, 2)} segundos\n"))
}
