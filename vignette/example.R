library(grib2nc)
library(glue)
library(R.utils)

directorioBase <- "D:/INIA/ICON/prueba_2/18"
rutas <- list.files(directorioBase, full.names = TRUE, recursive = TRUE)

#Filtrar rutas .bz2 y extraer grib2 de los bz2
rutas_bz2 <- rutas[grepl("\\.bz2$", rutas)]
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)

#Actualizar rutas después de la extracción
rutas <- list.files(directorioBase, full.names = TRUE, recursive = TRUE)
rutas_grib2 <- rutas[grepl("\\.grib2$", rutas)]

rutass_nc <- gsub("\\.grib2$", ".nc", rutas_grib2) #Rutas de salida (.nc)


ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
Grib2ANetCDF(ruta_in = rutas_grib2, ruta_out = rutass_nc, parallel = T,verbose = F,ruta_script = ruta_script_wsl)

