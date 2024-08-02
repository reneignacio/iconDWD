# Unir los SpatRaster
icon <- c(icon_05, icon_06, icon_07_10)
terra::time(icon)<-terra::time(icon) - hours(4)
icon


# Cargar las librerías necesarias
library(terra)

# Suponiendo que ya tienes cargado el SpatRaster original llamado 'icon'
# Crear una copia del SpatRaster original para los valores instantáneos
icon_hourly <- icon

# Función para calcular la diferencia entre capas, considerando el reinicio diario
calculate_hourly_precip <- function(layer_current, layer_previous, time_current, time_previous) {
  # Si es la primera capa del día, el valor es el mismo
  if (as.Date(time_current) != as.Date(time_previous)) {
    return(layer_current)
  } else {
    # Calcular la diferencia entre la capa actual y la anterior
    return(layer_current - layer_previous)
  }
}

# Iterar sobre cada capa y calcular la diferencia
for (i in 2:nlyr(icon)) {
  # Calcular la diferencia entre la capa actual y la capa anterior
  icon_hourly[[i]] <- calculate_hourly_precip(icon[[i]], icon[[i - 1]], time(icon)[i], time(icon)[i - 1])
}

# Eliminar los valores negativos (si los hay) resultantes de la diferencia
icon_hourly <- clamp(icon_hourly, lower=0, values=TRUE)

# La primera capa debe ser igual a la original ya que no hay capa previa para restar
icon_hourly[[1]] <- icon[[1]]


# Mostrar el nuevo SpatRaster
print(icon_hourly)

# Realizar el acumulado diario
icon_daily <- tapp(iconn_hourly, "days", sum)

# Verificar el resultado
print(icon_daily)

library(iniamet)

iconDWD::coordenadas_estaciones()



# Asegúrate de que el directorio de salida existe
output_dir <- "E:/plantillas_mapas_qgis/tif_icon"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Guardar cada capa diaria como un archivo TIFF
for (i in 1:nlyr(icon_daily)) {
  # Obtener la fecha de la capa
  fecha <- time(icon_daily)[i]

  # Crear un nombre de archivo basado en la fecha
  filename <- paste0(output_dir, "/precipitation_v3", format(fecha, "%Y%m%d"), ".tif")

  # Guardar la capa como un archivo TIFF
  writeRaster(icon_daily[[i]], filename, overwrite = TRUE)
}

# Verificar que los archivos se hayan guardado correctamente
list.files(output_dir, pattern = "\\.tif$", full.names = TRUE)
