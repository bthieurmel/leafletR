% Generated by roxygen2 (4.0.2): do not edit by hand
\name{dfToGeoJSON}
\alias{dfToGeoJSON}
\title{Internal function of toGeoJSON}
\usage{
dfToGeoJSON(data, name, dest, lat.lon, overwrite)
}
\arguments{
\item{data}{\code{\link{data.frame}}.}

\item{name}{Name of the resulting GeoJSON file, as string.}

\item{dest}{Directory the file shall be saved to, as string.}

\item{lat.lon}{Names or indices of the columns in \code{data} containing the
coordinates, as vector of two: \code{c(latitude, longitude)}.}

\item{overwrite}{\code{TRUE} overwrites existing files with the same name.}
}
\value{
GeoJSON file path, as string.
}
\description{
Create GeoJSON file from data frame
}
\author{
Christian Graul
}
\seealso{
\code{\link{toGeoJSON}}
}
\keyword{internal}
\keyword{methods}

