
library(lubridate)
library(terra)
library(RColorBrewer)

# Definir la variable de interés
variable <- "T_2M"
#puede ser CAPE_ML, SNOW_CON,SNOW_GSP, T_2M, TMAX_2M, TMIN_2M, TOT_PREC, WW

# Definir el rango de fechas
fecha_inicio <- ymd("2024-05-01")
fecha_fin <- ymd("2024-05-09")

# Generar todas las fechas en el rango
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = "days")

# Formatear las fechas al formato de directorio necesario (YYYYMMDD)
fechas_formateadas <- format(fechas, "%Y%m%d")

# Crear el patrón de búsqueda para los archivos
pattern <- paste0("_(0(0[0-9]|1[0-9]|2[0-3]))_", variable, "\\.nc$")

# Crear el patrón de búsqueda para los archivos que contienen "023" (SNOW_CON o SNOW_GSP o TOT_PREC)
#pattern <- paste0("_(023)_", variable, "\\.nc$")

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
icon<-rast(archivos_seleccionados)



nuble<-cargar_regiones("R16")
nuble<-vect("D:/crop/comunas/R16.shp")
nuble<-project(nuble,icon)
icon_nuble<-crop(icon,nuble,mask=F,touches=T)
icon_nuble<-icon_nuble-273.15
names(icon_nuble)<-time(icon_nuble)


# Definir la paleta de colores
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

# Ajustar el punto medio de la paleta de colores
breaks <- seq(min(values(icon_nuble)), max(values(icon_nuble)), length.out = 100)
col_breaks <- color_palette(length(breaks) - 1)

# Crear la animación con la nueva paleta de colores
#animate(icon_nuble,  breaks = breaks)


plot(icon_nuble, breaks = breaks)

time(icon)
#plet(icon_nuble[[23]],tiles="Streets")


icon_chile<-terra::crop(icon,chile,mask=F,touches=T)
icon_chile<-icon_chile-273.15
names(icon_chile)<-time(icon_chile)
animate(icon_chile)






SNOW_CON
SNOW_GSP

snow_total<-SNOW_CON+SNOW_GSP
plet(snow_total[[23]],tiles="Streets")

library(terra)
# Suponiendo que icon_nuble es tu SpatRaster cargado con datos horarios

# Utilizamos tapp con el índice "days" para agrupar por días y la función 'mean' para calcular la media diaria
icon_nuble_diario <- tapp(icon_nuble, index = "days", fun = sum)
icon_nuble_diario[[20]]
values(icon_nuble_diario)
icon_nuble_diario<-icon_nuble
# Para ver el resultado
print(icon_nuble_diario)

time(icon_nuble_diario)


library(terra)

# Supongamos que icon_nuble es tu SpatRaster cargado

# Calculando la temperatura mínima diaria
min_diario <- tapp(icon_nuble, index = "days", fun = min)

# Calculando la temperatura máxima diaria
max_diario <- tapp(icon_nuble, index = "days", fun = max)

# Calculando el promedio de los mínimos y máximos diarios
promedio_diario <- (min_diario + max_diario) / 2

# Ver el resultado
print(promedio_diario)




# Guardar el SpatRaster como un archivo .rds
saveRDS(icon_nuble_diario, "D:/INIA/Downscaling/RFmerge_2/DATOS/ICON/promedio_diario.rds")

# Cargar el SpatRaster en otra sesión de R
icon_nuble_diario <- readRDS("D:/INIA/Downscaling/RFmerge_2/DATOS/ICON/promedio_diario.rds")













# Cargar la librería necesaria
library(terra)

# Suponiendo que 'icon_nuble' es tu objeto SpatRaster ya cargado
# Define el nombre del archivo de salida
filename <- "D:/INIA/Downscaling/RFmerge_2/DATOS/ICON/icon_nuble.nc"

# Guarda el raster como NetCDF
resultado <- writeCDF(icon_nuble, filename, varname="2t", longname="2 metre temperature", unit="degrees C", overwrite=TRUE)

# Verifica si el archivo se ha creado correctamente
if (file.exists(filename)) {
  print("El archivo NetCDF se ha creado correctamente.")
} else {
  print("Hubo un error al crear el archivo NetCDF.")
}
