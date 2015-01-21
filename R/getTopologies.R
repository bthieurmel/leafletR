#' Print topology objects of a TopoJSON file
#' 
#' Prints the names of all available topology objects of a TopoJSON file.
#' 
#' 
#' @aliases getTopologies topo
#' @param data Name of data file as string or TopoJSON object.
#' @param print If \code{TRUE} (default), the property names are printed.
#' @return Topology object names as string vector.
#' @author Christian Graul
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' #getTopologies()
#' }
#' @export getTopologies
getTopologies <-
function(data, print=TRUE) {
	
	# if file path is given
	if(is.character(data)) {
		# check if file exists and convert JSON
		if(!file.exists(data)) stop("Data file not found")
		data <- jsonlite::fromJSON(data)
		if(is.null(data$type)) stop("'data' requires TopoJSON file")
		if(tolower(data$type)!="topology") stop("'data' requires TopoJSON file")
	}
	
	# get objects
	obj <- unique(names(data$objects))
	
	# print and return
	if(print) print(obj)
	invisible(obj)
}
