#' Internal function of toGeoJSON
#' 
#' Create GeoJSON file from external spatial data file
#' 
#' 
#' @param data Path to external spatial data file, as string.
#' @param name Name of the resulting GeoJSON file, as string.
#' @param dest Directory the file shall be saved to, as string.
#' @param overwrite \code{TRUE} overwrites existing files with the same name.
#' @return GeoJSON file path, as string.
#' @author Christian Graul
#' @seealso \code{\link{toGeoJSON}}
#' @keywords methods internal
fileToGeoJSON <-
function(data, name, dest, overwrite) {
	if(!file.exists(data)) stop("Data file not found")
	if(file.exists(paste0(file.path(dest, name), ".geojson")) && !overwrite) stop("Abort - file already exists")
	stopifnot(requireNamespace("httr", quietly=TRUE))
	
	# taken from rgbif package: cran.r-project.org/package=rgbif‎
	# package import impractical, since rgbif imports several other packages
	url <- "http://ogre.adc4gis.com/convert"
	tt <- httr::POST(url, body=list(upload=httr::upload_file(data)))
	out <- httr::content(tt, as="text")
	fileConn <- file(paste0(file.path(dest, name), ".geojson"))
	writeLines(out, fileConn)
	close(fileConn)
    # end rgbif code

	return(paste0(file.path(dest, name), ".geojson"))
}
