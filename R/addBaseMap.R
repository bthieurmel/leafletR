#' Add custom base maps
#' 
#' Add a custom base map to the list of maps available in the function
#' \code{\link{leaflet}}.
#' 
#' 
#' @aliases addBaseMap base
#' @param name Name of the base map.
#' @param title Title of the base map, used in the layer control of the
#' resulting map. Optional -- if missing, \code{name} is used.
#' @param url URL for the base map. See
#' \url{http://leafletjs.com/reference.html#tilelayer} for more information.
#' @param options Optional list of additional options. See
#' \url{http://leafletjs.com/reference.html#tilelayer} for for a list of valid
#' options.
#' @author François Guillem
#' @seealso \code{\link{leaflet}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # duplicates osm base map
#' addBaseMap(
#'   name="myosm", 
#'   title="Duplicated OpenStreetMap", 
#'   url="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
#'   options=list(
#'     attribution='&copy; <a href="http://openstreetmap.org/copyright", target=
#'       "_blank">OpenStreetMap contributors</a>'
#'   )
#' )
#' 
#' map <- leaflet(base.map="myosm")
#' }
#' 
#' @export addBaseMap
addBaseMap <- function(name, title, url, options) {
  # get existing base maps
  baseMaps <-  getOption("leafletBaseMaps")
  
  # create base map
  if(missing(title)) title <- name
  newBaseMap <- list(title=title, url=url)
  if(missing(options)) options <- NULL
  newBaseMap$options <- options
  
  # add base map
  baseMaps[[name]] <- newBaseMap
  options(leafletBaseMaps=baseMaps)
}
