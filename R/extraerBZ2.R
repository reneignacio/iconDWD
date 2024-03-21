

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

extraerBZ2 <- function(rutas, parallel = FALSE, ncores = detectCores() - 1) {
  requireNamespace("parallel", quietly = TRUE)
  requireNamespace("doParallel", quietly = TRUE)

  if (!parallel) {
    # Ejecución secuencial
    sapply(rutas, function(ruta) {
      if(file.exists(ruta)) {
        bunzip2(ruta, overwrite = TRUE)
        cat(glue("Archivo {ruta} extraído.\n"))
      } else {
        cat(glue("El archivo {ruta} no existe.\n"))
      }
    })
  } else {
    # Configuración para ejecución paralela
    registerDoParallel(cores = ncores)
    cl <- makeCluster(ncores)
    clusterExport(cl, varlist = c( "glue", "R.utils"), envir = environment())
    clusterEvalQ(cl, {
      library(glue)
      library(R.utils)
      library(grib2nc)
      library(doSNOW)
      library(doParallel)
      library(foreach)
    })

    # Ejecución paralela
    resultados <- foreach(ruta = rutas, .packages = c("glue", "R.utils","grib2nc","doParallel","doSNOW","foreach")) %dopar% {
      if(file.exists(ruta)) {
        bunzip2(ruta, overwrite = TRUE)
        return(glue("Archivo {ruta} extraído.\n"))
      } else {
        return(glue("El archivo {ruta} no existe.\n"))
      }
    }
    cat(paste(resultados, collapse = ""))

    stopCluster(cl)

  }
  close()
}
