

dir<-"D:/INIA/ICON/prueba_2/20240319/18/NetCDF/"
#dir<-"E:/ICON/20240319/18/NetCDF"

dir_files<-FiltrarVariable(dir,variable = paste0("_(00[1-6])",variable,"\\.nc$"))

variable<-"TOT_PREC.nc"
list.files("D:/INIA/ICON/prueba_2/20240120/18/")

#aqui debo seleccionar solo los 001 a 006 que terminen en .nc de cualquier carpeta

# icon_global_icosahedral_single-level_2024031918_000_TOT_PREC.nc
# icon_global_icosahedral_single-level_2024031918_001_TOT_PREC.nc ...

# icon_global_icosahedral_single-level_2024031912_000_TOT_PREC.nc
# icon_global_icosahedral_single-level_2024031912_001_TOT_PREC.nc ...

#luego rast a eso y tendre un dia,

# Definir el directorio de búsqueda
dir_busqueda <- "E:/ICON_FEB/20240407/00/"

# Definir la variable de interés
variable <- "T_2M"

# Crear el patrón de búsqueda
# Este patrón busca archivos que terminen con un número de secuencia del 001 al 005,
# seguido de la variable específica y la extensión .nc
pattern <- paste0("_(00[1-6])_", variable, "\\.nc$")

# Utilizar list.files para buscar archivos que cumplan con el patrón
archivos_seleccionados <- list.files(path = dir_busqueda, pattern = pattern, full.names = TRUE,recursive = T)

# Imprimir las rutas de los archivos seleccionados
print(archivos_seleccionados)

a<-rast(archivos_seleccionados)
time(a)


icon_18<-a


chile<-vect("F:/shape/chile/Regional.shp")
chile<-project(chile,icon_18)
icon_chile<-crop(icon_18,chile,mask=T)


nuble<-vect("D:/crop/comunas/R16.shp")
nuble<-project(nuble,icon_18)
icon_nuble<-crop(icon_18,nuble,mask=T)
icon_nuble<-icon_nuble-273.15
names(icon_nuble)<-time(icon_nuble)

animate(icon_nuble)

daily_mean_temp <- tapp(icon_nuble, "days", min)

plet(daily_mean_temp)


animate(daily_mean_temp)

raster<-icon_chile
time(raster)<-time(raster)


inicio <- ymd_hm("2024-03-20 00:00")
fin <- ymd_hm("2024-03-23 00:00")
raster_subset <- raster[[time(raster) >= inicio & time(raster) <= fin]]
plet(raster_subset[[nlyr(raster_subset)]],tiles="Streets")



# Obtener todas las estaciones de la región "Coquimbo"
estaciones_coquimbo <- coordenadas_estaciones(region = "Coquimbo",institucion = "CEAZA")
plet(estaciones_coquimbo)
# Obtener todas las estaciones de la comuna "Illapel"
estaciones_illapel <- coordenadas_estaciones(comuna = "Illapel")
plet(estaciones_illapel)
# Obtener todas las estaciones con primera lectura después de una fecha específica
estaciones_recientes <- coordenadas_estaciones(Inicio = ymd("2020-01-01"), institucion = "INIA")
plet(estaciones_recientes)


estaciones_cauquenes<-coordenadas_estaciones(comuna = "Cauquenes",institucion = "INIA")

writeVector(estaciones_cauquenes,"E:/estaciones_cauquenes.shp")

#
resultado<-terra::extract(raster,estaciones_recientes,ID=F)
resultado<-t(resultado)
colnames(resultado)<-estaciones_recientes$nombre
fechas<-time(raster)
df_final<-data.frame(fechas,resultado)
df_final










