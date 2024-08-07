% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordenadas_ciudad.R
\name{coordenadas_ciudad}
\alias{coordenadas_ciudad}
\title{Obtener Coordenadas de una Localidad mediante la API de Nominatim}
\usage{
coordenadas_ciudad(localidad)
}
\arguments{
\item{localidad}{Una cadena de caracteres que especifica la localidad para la cual
se desean obtener las coordenadas. La cadena debe ser suficientemente descriptiva
para ser localizada por la API de Nominatim (e.g., "Los Angeles, Chile").}
}
\value{
Un objeto \code{SpatVector} con el punto de las coordenadas (longitud y latitud) de la localidad.
}
\description{
Esta función consulta la API de Nominatim de OpenStreetMap para obtener las
coordenadas geográficas (latitud y longitud) de una localidad especificada por
el usuario. Las coordenadas se devuelven en un formato que puede ser utilizado
directamente con la función \code{extract} del paquete \code{terra}.
}
\examples{
coords <- coordenadas_ciudad("Los Angeles, Chile")
print(coords)
plet(coords)

}
