library(terra)
library(RColorBrewer)

# Cargar el objeto SpatRaster
icon_daily_nuble_celsius <- readRDS("E:/ICON_INFORME/bichos/icon_daily_nuble_celsius2.rds")

# Calcular el mínimo y máximo de todas las capas
min_temp <- min(values(icon_daily_nuble_celsius), na.rm = TRUE)
max_temp <- max(values(icon_daily_nuble_celsius), na.rm = TRUE)

# Definir la paleta de colores continua
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

# Crear los intervalos de colores
breaks <- seq(min_temp, max_temp, length.out = 100)
colors <- color_palette(length(breaks) - 1)

# Crear la carpeta si no existe
output_folder <- "D:/INFORMES/frames"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Función para limpiar nombres de archivos
sanitize_filename <- function(filename) {
  gsub("[:]", "-", filename)
}

# Recorrer cada capa en el objeto SpatRaster
for (i in 1:nlyr(icon_daily_nuble_celsius)) {
  layer_to_plot <- icon_daily_nuble_celsius[[i]]

  # Obtener el nombre de la capa y limpiarlo
  layer_name <- names(icon_daily_nuble_celsius)[i]
  clean_layer_name <- sanitize_filename(layer_name)

  # Crear el archivo de salida
  output_file <- file.path(output_folder, paste0(clean_layer_name, ".png"))

  # Ploteo del raster con los colores continuos y suaves
  png(output_file, width = 800, height = 600)
  plot(layer_to_plot, col = colors, breaks = breaks, legend = FALSE, axes = FALSE)
  dev.off()
}

# Imprimir un mensaje de éxito
cat("Los plots se han guardado en", output_folder)
