#' Create GeoJSON file from spatial data
#' 
#' Creates a GeoJSON file from data frame, Spatial object or an external
#' spatial data file.
#' 
#' \code{toGeoJSON} can handle three types of spatial data: a simple
#' \code{data.frame} containing point coordinates and optional data columns,
#' \code{Spatial} objects and external spatial data files.
#' 
#' \emph{\code{Spatial} objects}
#' 
#' \code{Spatial} objects (\code{sp} package) should have geographical
#' coordinates (longlat, WGS84). If other projections are used,
#' \code{toGeoJSON} can transform the coordinates on the fly, using the
#' \code{rgdal} package.
#' 
#' \emph{Conversion of external spatial data files}
#' 
#' \code{toGeoJSON} uses the Ogre web API (\url{http://ogre.adc4gis.com}). See
#' the \href{http://ogre.adc4gis.comOgre} website for a list of supported
#' formats. Please note that for Shapefiles, MapInfo and VRT, Ogre only accepts
#' a zip file. The Ogre API does not support large files (>15 MB). Have a look
#' at the \code{rgdal} package and its \code{writeOGR} function, to convert
#' files on your local machine.
#' 
#' @aliases toGeoJSON tg
#' @param data Spatial data: \code{data.frame} with at least two columns,
#' representing the point coordinates, \code{Spatial} object (\code{sp}
#' package) or path to external spatial data file as string. See below for
#' details.
#' @param name Name of the resulting GeoJSON file, as string. Optional -- if
#' missing, the name of the data frame or data file is used.
#' @param dest Directory the file shall be saved to, as string. Optional -- if
#' missing, the current working directory is used.
#' @param lat.lon For data frame conversion only. Names or indices of the
#' columns in \code{data} containing the coordinates, as vector of two:
#' \code{c(latitude, longitude)}. Optional -- if missing, the first two columns
#' are used.
#' @param overwrite \code{TRUE} (which is the default) overwrites existing
#' files with the same name.
#' @return GeoJSON file path, as string.
#' @author Christian Graul
#' @seealso \code{\link{leaflet}}
#' @source The code for the conversion of external data files is taken from the
#' \code{togeojson} function of the \code{rgbif} package. Package import would
#' have unreasonably increased the dependencies of \code{leafletR}.
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # convert data frame
#' data(quakes)
#' toGeoJSON(data=quakes, name="quakes", dest=tempdir(), lat.lon=c(1,2))
#' 
#' # convert data frame - minimal call
#' # storing output file path in variable
#' data(quakes)
#' path <- toGeoJSON(data=quakes)
#' 
#' # preserve existing files from overwriting
#' toGeoJSON(data=quakes, overwrite=FALSE)
#' 
#' # convert Spatial objects
#' library(sp)
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' toGeoJSON(data=meuse, dest=tempdir()) # rgdal package required
#' 
#' crd <- coordinates(meuse)
#' msl <- SpatialLines(list(Lines(list(Line(crd)), "line1")), 
#'   proj4string=CRS("+init=epsg:28992"))
#' toGeoJSON(data=msl, dest=tempdir()) # rgdal package required
#' 
#' data(meuse.riv)
#' msp <- SpatialPolygons(list(Polygons(list(Polygon(meuse.riv)), 
#'   "meuse.riv")), proj4string=CRS("+init=epsg:28992"))
#' toGeoJSON(data=msp, dest=tempdir()) # rgdal package required
#' 
#' # convert a shapefile (in zipped archive)
#' toGeoJSON(data=system.file(package="leafletR", "files", "lynx.zip"), 
#'   name="lynx_telemetry", dest=tempdir())
#' 
#' # convert a KML/KMZ file
#' # using name of data file and saving to working directory
#' toGeoJSON(system.file(package="leafletR", "files", "peak_sk.kmz"))
#' }
#' 
#' @export toGeoJSON
toGeoJSON <-
function(data, name, dest, lat.lon, overwrite=TRUE) {
	if(missing(data)) stop("'data' is mandatory")
	if(missing(dest)) dest <- getwd()
	dest <- gsub("\\\\", "/", dest)
	if(substr(dest, nchar(dest), nchar(dest))=="/") dest <- substr(dest, 1, nchar(dest)-1)
	path <- NULL
	
	if(is.data.frame(data)) {
		if(missing(name)) name <- deparse(substitute(data))
		name <- gsub(" ", "_", name)
		if(missing(lat.lon)) lat.lon <- c(1,2)
		path <- dfToGeoJSON(data, name, dest, lat.lon, overwrite)
	} else if(class(data)=="character") {
		if(missing(name)) name <- paste(head(strsplit(tail(strsplit(data, "/")[[1]], 1), "[.]")[[1]], -1), collapse=".")
		name <- gsub(" ", "_", name)
		path <- fileToGeoJSON(data, name, dest, overwrite)
	} else if(class(data)[1]=="SpatialPoints" || class(data)[1]=="SpatialPointsDataFrame" || class(data)[1]=="SpatialLines" || class(data)[1]=="SpatialLinesDataFrame" || class(data)[1]=="SpatialPolygons" || class(data)[1]=="SpatialPolygonsDataFrame") {
		if(missing(name)) name <- deparse(substitute(data))
		name <- gsub(" ", "_", name)
		path <- spToGeoJSON(data, class(data)[1], name, dest, overwrite)
	} else {
		stop("Type of data not supported")
	}
	
	if(!is.null(path)) {
		message("\nFile saved under ", path)
		invisible(path)
	}
}
