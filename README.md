
# grib2nc

`grib2nc` es una herramienta diseñada para facilitar la utilización y transformación de datos meteorológicos del modelo DWD ICON, los cuales están disponibles en formato grib2. Este paquete permite convertir archivos grib2 a formato NetCDF (.nc) para su fácil uso, así como extraer archivos .bz2, tanto de forma secuencial como paralela, optimizando el manejo de grandes volúmenes de datos.

## Características

- **Conversión de Grib2 a NetCDF**: Transforma datos grib2 a formato NetCDF para su análisis y procesamiento.
- **Extracción de archivos BZ2**: Soporte para extracción secuencial y paralela de archivos .bz2, permitiendo un procesamiento eficiente de grandes datasets.
- **Optimización de Procesos**: Ejecución paralela para aprovechar al máximo los recursos de hardware disponibles.
- **Fácil de Usar**: Interfaces sencillas para usuarios de todos los niveles.

## Instalación WSL

```bash
wsl --install
```

Reiniciar y luego:

```bash
sudo apt update && sudo apt upgrade
```

```bash
sudo apt-get install cdo
```

instalar dependencias necesarias:
```bash
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
```
```bash
sudo apt-get update
```
```bash
sudo apt-get install libgdal-dev libgeos-dev libproj-dev  libnetcdf-dev libhdf5-dev gdal-bin -y
```
debes copiar el contenido de  'ICON_0125.rar" dentro de una carpeta en wsl (home/...)
Para descargar el archivo `ICON_0125.rar`, haz clic [aquí](https://github.com/reneignacio/grib2nc/raw/master/ICON_0125.rar).

ejecutar _chmod +x_ al archivo en consola, para dar permisos 
ejemplo:
```bash
chmod +x /home/user/ICON_0125/transform_0125.sh
```



## Instalación Paquete R
Puedes instalar `grib2nc` desde GitHub usando `devtools`:

```r
# instalar devtools si aún no lo has hecho
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

# instalar grib2nc
devtools::install_github("reneignacio/grib2nc")
```

## Uso

### Convertir Grib2 a NetCDF

Para convertir archivos grib2 a formato NetCDF:

```r
library(grib2nc)

ruta_in <- c("/ruta/a/tu/archivo1.grib2", "/ruta/a/tu/archivo2.grib2")
ruta_out <- c("/ruta/a/tu/archivo1.nc", "/ruta/a/tu/archivo2.nc")

Grib2ANetCDF(ruta_in, ruta_out, parallel = TRUE, ncores = 2)
```

### Extraer archivos BZ2

Para extraer archivos .bz2, ya sea de forma secuencial o paralela:

```r
extraerBZ2(c("/ruta/a/tu/archivo1.bz2", "/ruta/a/tu/archivo2.bz2"), parallel = TRUE, ncores = 2)
```

