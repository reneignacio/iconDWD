library(terra)
library(RColorBrewer)
library(magick)
library(lubridate)

# Definir la variable de interés
variable <- "T_2M"
# Puede ser CAPE_ML, SNOW_CON, SNOW_GSP, T_2M, TMAX_2M, TMIN_2M, TOT_PREC, WW

# Definir el rango de fechas
fecha_inicio <- ymd("2024-05-01")
fecha_fin <- ymd("2024-05-09")

# Generar todas las fechas en el rango
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "days")

# Formatear las fechas al formato de directorio necesario (YYYYMMDD)
fechas_formateadas <- format(fechas, "%Y%m%d")

# Crear el patrón de búsqueda para los archivos
pattern <- paste0("_(0(0[0-9]|1[0-9]|2[0-3]))_", variable, "\\.nc$")

# Inicializar un vector para almacenar los resultados
archivos_seleccionados <- c()

# Bucle para buscar archivos en cada uno de los directorios de fechas
for (fecha in fechas_formateadas) {
  # Construir el directorio de búsqueda
  dir_busqueda <- paste0("E:/icon_mayo//", fecha, "/00/")

  # Utilizar list.files para buscar archivos que cumplan con el patrón en el directorio
  archivos_encontrados <- list.files(path = dir_busqueda, pattern = pattern, full.names = TRUE, recursive = TRUE)

  # Agregar los archivos encontrados al vector de resultados
  if (length(archivos_encontrados) > 0) {
    archivos_seleccionados <- c(archivos_seleccionados, archivos_encontrados)
  }
}
# Resultado final
archivos_seleccionados
icon <- rast(archivos_seleccionados)


# Cargar y proyectar los datos
nuble <- vect("D:/crop/comunas/R16.shp")
nuble <- cargar_regiones("R16")
nuble <- project(nuble, icon)
icon_nuble <- crop(icon, nuble, mask = TRUE, touches = TRUE)
icon_nuble <- icon_nuble - 273.15
time(icon_nuble) <- time(icon_nuble) - hours(4)
names(icon_nuble) <- time(icon_nuble)

chile <- vect("D:/crop/chile/Regional.shp")
chile <- project(chile, icon)
icon_chile <- crop(icon, chile, mask = TRUE, touches = TRUE)
icon_chile <- icon_chile - 273.15
time(icon_chile) <- time(icon_chile) - hours(4)
names(icon_chile) <- time(icon_chile)

# Manejar valores NA
icon_nuble <- na.omit(icon_nuble)
icon_chile <- na.omit(icon_chile)

# Definir la paleta de colores
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

# Crear los breaks para que 0 °C sea uno de ellos
min_temp_nuble <- min(values(icon_nuble), na.rm = TRUE)
max_temp_nuble <- max(values(icon_nuble), na.rm = TRUE)
breaks_nuble <- seq(min_temp_nuble, max_temp_nuble, length.out = 100)
col_breaks_nuble <- color_palette(length(breaks_nuble) - 1)

min_temp_chile <- min(values(icon_chile), na.rm = TRUE)
max_temp_chile <- max(values(icon_chile), na.rm = TRUE)
breaks_chile <- seq(min_temp_chile, max_temp_chile, length.out = 100)
col_breaks_chile <- color_palette(length(breaks_chile) - 1)

# Guardar las tres primeras capas como imágenes
save_first_images <- function(raster, shapefile, col_breaks, breaks) {
  for (i in 1:min(3, nlyr(raster))) {
    png_filename <- paste0("frame_", sprintf("%03d", i), ".png")
    png(png_filename, width = 800, height = 600)
    plot(raster[[i]], col = col_breaks, breaks = breaks, legend = TRUE, type = "continuous",
         main = names(raster)[i])
    plot(shapefile, add = TRUE, border = "grey", lwd = 2)
    dev.off()
  }
}

# Crear una función para guardar las capas como imágenes y luego crear un GIF
create_gif <- function(raster, shapefile, col_breaks, breaks, gif_path) {
  frames <- list()

  for (i in 1:nlyr(raster)) {
    png_filename <- paste0("frame_", sprintf("%03d", i), ".png")
    png(png_filename, width = 800, height = 600)
    plot(raster[[i]], col = col_breaks, breaks = breaks, legend = TRUE, type = "continuous",
         main = names(raster)[i])
    plot(shapefile, add = TRUE, border = "grey", lwd = 2)
    dev.off()
    frames[[i]] <- image_read(png_filename)
  }

  gif <- image_animate(image_join(frames), fps = 4)
  image_write(gif, gif_path)

  # Eliminar los archivos temporales
  file.remove(unlist(lapply(1:nlyr(raster), function(i) paste0("frame_", sprintf("%03d", i), ".png"))))
}


# Crear el GIF para icon_nuble
#create_gif(icon_nuble, nuble, col_breaks_nuble, breaks_nuble, "icon_nuble_v3.gif")

# Crear el GIF para icon_chile
create_gif(icon_chile, chile, col_breaks_chile, breaks_chile, "icon_chile_v3.gif")
