library(iconDWD)
library(glue)
library(R.utils)

directorioBase <- "E:/ICON/20240319/06"

rutas_bz2 <- list.files(directorioBase, pattern = "\\.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 8)


rutas_grib2 <- list.files(directorioBase, pattern = "\\.grib2$", full.names = TRUE, recursive = TRUE)
ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
?Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl)
