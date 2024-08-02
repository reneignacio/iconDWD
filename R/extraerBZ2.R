#' Extraer archivos .bz2 en paralelo o secuencialmente
#'
#' Esta función verifica si un archivo o archivos .bz2 existen en la ruta especificada y, en caso afirmativo,
#' los extrae en el mismo directorio de manera opcionalmente paralela.
#'
#' @param rutas Vector de rutas completas a los archivos .bz2 que se desean extraer.
#' @param parallel Lógico, indica si se debe ejecutar en paralelo.
#' @param ncores Número de núcleos a utilizar para la ejecución paralela.
#'
#' @details Si `parallel` es TRUE, la función configura un clúster usando el paquete doParallel
#' y extrae los archivos en paralelo utilizando el número especificado de núcleos.
#' Requiere que `bunzip2` esté instalado y disponible en el sistema.
#'
#' @return Imprime un mensaje indicando el resultado de la extracción.
#' @examples
#' extraerBZ2(c("path/to/your/file1.bz2", "path/to/your/file2.bz2"), parallel = TRUE, ncores = 2)
#' @export
#' @importFrom glue glue
#' @import doParallel doSNOW R.utils parallel foreach

extraerBZ2 <- function(rutas, parallel = FALSE, ncores = parallel::detectCores() - 1) {
  requireNamespace("parallel", quietly = TRUE)
  requireNamespace("doParallel", quietly = TRUE)
  requireNamespace("foreach", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("R.utils", quietly = TRUE)
  requireNamespace("doSNOW", quietly = TRUE)

  total_archivos <- length(rutas)

  if (!parallel) {
    # Ejecución secuencial
    pb <- utils::txtProgressBar(min = 0, max = total_archivos, style = 3)
    for (i in seq_along(rutas)) {
      ruta <- rutas[i]
      if (file.exists(ruta)) {
        R.utils::bunzip2(ruta, overwrite = TRUE)
      }
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else {
    # Configuración para ejecución paralela
    cl <- parallel::makeCluster(ncores, type = "PSOCK", outfile = "")
    doSNOW::registerDoSNOW(cl)

    # Exportar la variable 'total_archivos' al clúster
    parallel::clusterExport(cl, "total_archivos", envir = environment())

    # Barra de progreso
    pb <- utils::txtProgressBar(min = 0, max = total_archivos, style = 3)
    progress <- function(n) {
      utils::setTxtProgressBar(pb, n)
      cat(glue::glue(" ({n}/{total_archivos})"), "\r")
    }
    opts <- list(progress = progress)

    # Ejecución paralela con progreso
    foreach::foreach(i = seq_along(rutas), .packages = c("glue", "R.utils"), .options.snow = opts) %dopar% {
      ruta <- rutas[i]
      if (file.exists(ruta)) {
        R.utils::bunzip2(ruta, overwrite = TRUE)
      }
      progress(i)
    }

    close(pb)
    parallel::stopCluster(cl)
  }
}
