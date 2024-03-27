library(iconDWD)
library(glue)
library(R.utils)
library(terra)
library(lubridate)

directorioBase <- "D:/INIA/ICON/prueba_2/20240319/18"

rutas_bz2 <- list.files(directorioBase, pattern = "\\.bz2$", full.names = TRUE, recursive = TRUE)
extraerBZ2(rutas = rutas_bz2, parallel = TRUE, ncores = 6)


rutas_grib2 <- list.files(directorioBase, pattern = "\\.grib2$", full.names = TRUE, recursive = TRUE)
ruta_script_wsl = "/home/inia/ICON_0125/transform_0125.sh"
Grib2ANetCDF(ruta_in = rutas_grib2, parallel = T,verbose = F,ruta_script = ruta_script_wsl)




#dir<-"D:/INIA/ICON/prueba_2/20240319/18/NetCDF/"
dir<-"E:/ICON/20240319/18/NetCDF"

dir_files<-FiltrarVariable(dir,variable = "TOT_PREC")
icon_18<-rast(dir_files)




nuble<-vect("D:/crop/comunas/R16.shp")
nuble<-project(nuble,icon_18)
icon_nuble<-crop(icon_18,nuble,mask=T)
plot(icon_nuble)

icon_nuble_r <- disagg(icon_nuble, fact=2, method="bilinear")

inicio <- ymd_hms("2024-03-19 21:00:00")
fin <- ymd_hms("2024-03-20 21:00:00")

raster<-icon_nuble
raster <- raster[[time(raster) >= inicio & time(raster) <= fin]]

chillan<-subset(nuble,nuble$nom_com=="Chillán" )

values(nuble)


# Obtener todas las estaciones de la región "Coquimbo"
estaciones_coquimbo <- coordenadas_estaciones(region = "Coquimbo")
plot(estaciones_coquimbo)
# Obtener todas las estaciones de la comuna "Illapel"
estaciones_illapel <- coordenadas_estaciones(comuna = "Illapel")
plot(estaciones_illapel)
# Obtener todas las estaciones con primera lectura después de una fecha específica
estaciones_recientes <- coordenadas_estaciones(Inicio = ymd("2023-01-01"))

a<-coordenadas_estaciones(institucion = "INIA",region = "Ñuble")
plet(a)




coords_estacion <- obtener_coords_estacion(nombre_estacion ="Ce arroz")
coords_estacion<- coordenadas_estaciones()
coords_estacion[coords_estacion$region=="Coquimbo"]

plot(nuble[,7])


resultado<-terra::extract(raster,a,ID=F)
resultado
resultado<-t(resultado)
resultado
colnames(resultado)<-a$nombre
resultado
fechas<-time(raster)

df_final<-data.frame(fechas,resultado)








t(b)

length(nuble$nom_com)


coords_estacion


extract(raster,nuble[,7],ID=F)




valores_y_fechass <- extraer_valores_icon(raster,nuble[,7],ID=F)
valores_y_fechass
#el error es pq son 21 comunas es decir se crearan 21 columnas y luego en la funcion asigno nombres,
#supondiendo que son dos columnas, (fecha y valor), tengo que repetir la columna valor segun la cantidad o
#simplemente no asignarle un nombre
print(valores_y_fechas)




plet((icon_nuble[[length(time(icon_nuble))]]),tiles="Streets")



