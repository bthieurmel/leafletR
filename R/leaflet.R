#' Create a Leaflet web-map
#' 
#' Creates a web-map of users' spatial data over open base maps. Output
#' consists of a ready to use HTML file (and a GeoJSON/TopoJSON data file).
#' 
#' 
#' @aliases leaflet leaf
#' @param data Name(s) of data file(s) (GeoJSON/TopoJSON format), as string or
#' a list of strings. Plotting order follows the file sequence.
#' 
#' @param dest Path to the \code{data} file, as string. Optional -- if missing,
#' the current working directory is used.
#' 
#' @param title Map title, as string. Default is \code{"map"}.
#' 
#' @param size Size of the map on the website in pixels, as numeric vector --
#' \code{c(width, height)}. Optional -- if missing, a fullscreen (browser
#' window) map is generated.
#' 
#' @param base.map Base map(s) in the background of the data, as string. One or
#' a list of \code{"osm"} (OpenStreetMap standard map), \code{"tls"}
#' (Thunderforest Landscape), \code{"mqosm"} (MapQuest OSM), \code{"mqsat"}
#' (MapQuest Open Aerial), \code{"water"} (Stamen Watercolor) or \code{"toner"}
#' (Stamen Toner). Default is \code{"osm"}. If \code{base.map} is a list, the
#' last item is used as default base map and a layer control button is added to
#' the map.
#' 
#' @param center Map center coordinates in decimal degrees, as vector of two
#' numeric values: \code{c(latitude, longitude)}. Optional -- if missing, the
#' data layer is centered automatically.
#' 
#' @param zoom Map zoom level, as integer value. Usually a value between
#' \code{0} (global small scale) and \code{18} (detailed large scale). The
#' MapQuest Open Aerial map (\code{base.map="mqsat"}) provides only 12 zoom
#' levels [0-11]. Optional -- if missing, the zoom level is calculated for the
#' bounding box of the data layer.
#' 
#' @param style Style(s) of the data layer(s). One or a list of style
#' object(s), created by \code{\link{styleSingle}}, \code{\link{styleGrad}} or
#' \code{\link{styleCat}}. Optional -- if missing, a default style is applied.
#' 
#' @param popup Properties (attributes) of the data to be shown in a popup when
#' a map object is clicked. String or a vector of strings. \code{"*"} adds all
#' available properties to the popup. A \code{list} of (vectors of) strings
#' specifies properties for multiple \code{data} layers. Per default no popups
#' are shown.
#' 
#' @param zoomlevel Zoom levels(s) of the data layer(s). One or a list of integer zoom range,
#' on which data layers will be visible. example : c(2,4), or list(c(2,4), c(4,6))
#'  Optional -- if missing, data.layer is always visible
#' 
#' @param controls List of controls to be added to the map. Available controls
#' are \code{"zoom"}, \code{"scale"}, \code{"layer"} and \code{"legend"}.
#' \code{"all"} (the default) adds all controls. Controls are only added if
#' necessary, e.g. in case of one data layer there is no legend. \code{NA}
#' omits all controls. Note: data layer controls only appear if
#' \code{incl.data} is set to \code{TRUE}.
#' 
#'@param incl.data If \code{TRUE}, \code{data} is included in the HTML file
#' itself. Per default (\code{incl.data=FALSE}) the data is saved in a separate
#' file. Including data in the HTML file allows for viewing the map locally on
#' some browsers (e.g. Chrome and Opera) and enables support for data layer
#' control.
#' 
#' @param overwrite \code{TRUE} (which is the default) overwrites existing
#' files with the same name.
#' 
#' @return HTML file path, as string.
#' 
#' @note Please note: \code{data} only accepts GeoJSON/TopoJSON files with one
#' geometry type and geographical coordinates (longlat, WGS84).
#' @author Christian Graul
#' @seealso \code{\link{styleSingle}}, \code{\link{styleGrad}},
#' \code{\link{styleCat}}
#' @references Base map tiles are provided by \tabular{lll}{ \tab OpenStreetMap
#' standard map \tab \url{http://www.openstreetmap.org} \cr \tab Thunderforest
#' Landscape \tab \url{http://www.thunderforest.com} \cr \tab MapQuest OSM \tab
#' \url{http://www.mapquest.com} \cr \tab MapQuest Open Aerial \tab
#' \url{http://www.mapquest.com} \cr \tab Stamen Watercolor \tab
#' \url{http://stamen.com} \cr \tab Stamen Toner \tab \url{http://stamen.com} }
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # prepare data
#' data(quakes)
#' dat <- toGeoJSON(data=quakes, dest=tempdir())
#' 
#' # create and view simple map
#' map <- leaflet(dat)
#' map  # redirects to browseURL(map)
#' 
#' # set output directory and map title
#' map <- leaflet(data=dat, dest=tempdir(), title="Fiji Earthquakes")
#' 
#' # set map size, center and zoom level
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   size=c(800,600), center=c(-18.35, 179.75), zoom=6)
#' 
#' # set base map and popup
#' # magnitude is used as popup (type names(quakes) for available properties)
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   base.map="mqsat", popup="mag")
#' 
#' # include data in HTML file
#' map <- leaflet(dat, incl.data=TRUE)
#' 
#' # preserve existing files from overwriting
#' map <- leaflet(dat, overwrite=FALSE)
#' 
#' # more than one base map
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   base.map=list("osm", "mqsat", "tls"))
#' 
#' # multiple properties in the popup
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   popup=c("mag", "depth"))
#' 
#' # all available properties in the popup
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   popup="*")
#' 
#' # change style
#' sty <- styleSingle(col="red", fill=NA)
#' map <- leaflet(data=dat, dest=tempdir(), base.map="mqsat", style=sty)
#' 
#' # controls
#' map <- leaflet(data=dat, dest=tempdir(), controls=NA)  # no controls
#' map <- leaflet(data=dat, dest=tempdir(), controls="scale")  # scale only
#' map <- leaflet(data=dat, dest=tempdir(), controls=c("zoom", "scale"))
#' 
#' # more than one data set
#' park <- system.file(package="leafletR", "files", "park_sk.geojson")
#' peak <- toGeoJSON(system.file(package="leafletR", "files", "peak_sk.kmz"), 
#'   dest=tempdir())
#' sty.1 <- styleSingle(col="green", fill="green")
#' sty.2 <- styleSingle(col="brown", fill="brown", rad=3)
#' map <- leaflet(data=list(park, peak), dest=tempdir(), 
#'   style=list(sty.1, sty.2), popup=list("*", "Name"))
#' 
#' # names in legend
#' # note: "_" and "." are replaced with blanks in the legend 
#' map <- leaflet(data=list(National_Parks=park, Peaks.(>600.m)=peak), 
#'   dest=tempdir(), style=list(sty.1, sty.2), popup=list("*", "Name"))
#' 
#' # add layer control
#' # if incl.data=TRUE a layer control is automatically added
#' map <- leaflet(data=list(park, peak), dest=tempdir(), 
#'   style=list(sty.1, sty.2), popup=list("*", "Name"), incl.data=TRUE)
#' }
#' 
#' #########
#' # zoomlevel on data
#' 
#' # zoomlevel on one dataset
#' map <- leaflet(data=dat, title="Fiji Earthquakes", incl.data = FALSE, zoomlevel = c(4,6))
#' 
#' # zoomlevel on two dataset
#' # on on level 7, other on 9, both on 8
#' park <- system.file(package="leafletR", "files", "park_sk.geojson")
#' peak <- toGeoJSON(system.file(package="leafletR", "files", "peak_sk.kmz"),
#'                   dest=tempdir())
#'
#' sty.1 <- styleSingle(col="green", fill="green")
#' sty.2 <- styleSingle(col="brown", fill="brown", rad=3)
#'                   
#' map <- leaflet(data=list(park, peak), style=list(sty.1, sty.2), popup=list("*", "Name"),
#'               zoomlevel = list(c(8,9), c(7,8)))
#' map
#' @export leaflet
leaflet <-function(data, dest, title, size, base.map="osm", center, zoom, style, popup, 
                   zoomlevel, controls="all", incl.data=FALSE, overwrite=TRUE) {	
  
  # prepare data
  if(missing(data)) data <- NA
  topojson <- NULL
  json <- list()
  if(length(data)>1) for(n in 1:length(data)) {
    if(!is.na(data[[n]])) {
      json[[n]] <- jsonlite::fromJSON(data[[n]])
      if(is.null(json[[n]]$type)) stop("'data' requires GeoJSON or TopoJSON files")
      if(tolower(json[[n]]$type)=="topology") topojson <- append(topojson, TRUE)
      else topojson <- append(topojson, FALSE)
    }
  } else {
    if(!is.na(data)) {
      json[[1]] <- jsonlite::fromJSON(data)
      if(is.null(json[[1]]$type)) stop("'data' requires GeoJSON or TopoJSON files")
      if(tolower(json[[1]]$type)=="topology") topojson <- TRUE
      else topojson <- FALSE
    }
  }
  
  # prepare output file destination
  if(missing(dest)) dest <- getwd()
  dest <- gsub("\\\\", "/", dest)
  if(missing(title)) {
    if(any(is.na(data))) title <- "map" 
    else {
      if(length(data)==1) title <- gsub("_", " ", paste(head(strsplit(basename(data), "[.]")[[1]], -1), collapse="_")) else title <- "map"
    }
  }
  
  # prepare base map
  basemaps <- getOption("leafletBaseMaps")
  bm <- names(basemaps)
  base.map <- bm[pmatch(base.map, bm)]
  if(any(is.na(base.map))) stop("Invalid base.map")
  
  # prepare style
  if(missing(style)) style <- NA
  if(any(!is.na(style))) {
    if(is.list(style) & !is(style, "leafletr.style")) {
      for(i in 1:length(style)) if(! is(style[[i]], "leafletr.style")) stop("At least one style object not recognized")
    } else if(! is(style, "leafletr.style")) stop("Style object not recognized")
  }
  if(length(data)>1 && !is.na(style)) if(length(style)<length(data) || !is.list(style)) stop("Number of styles must correspond to number of data files")
  if(file.exists(file.path(dest, gsub(" ", "_", title))) && !overwrite) stop("Abort - file already exists")
  
  # prepare popup
  if(!missing(popup)) {
    if(is.list(popup)) {
      for(n in 1:length(popup)) if(length(popup[[n]])==1) if(!is.na(popup[[n]])) if(popup[[n]]=="*") popup[[n]] <- getProperties(json[[n]], FALSE)
    } else {
      if(length(popup)==1) if(!is.na(popup)) if(popup=="*") popup <- getProperties(json[[1]], FALSE)
      popup <- list(popup)
    }
    if(length(popup)==length(unlist(popup))) multi.prop <- FALSE
    else multi.prop <- TRUE
  }
  
  # prepare zoomend
  if(missing(zoomlevel)){
    zoomlevel <- NA
  } 
  if(any(!is.na(zoomlevel))) {
    if(!is.list(zoomlevel)){
      if(!is.vector(zoomlevel)){
        stop("zoomlevel must be a list or a vector of two integer")
      }else{
        zoomlevel <- list(zoomlevel)
      }
    }
    
    if((length(data)< length(zoomlevel)) | (length(zoomlevel) > 1 & length(data)!=length(zoomlevel))){
      stop("Number of zoomlevel must be one or correspond to number of data")
    }else if(length(data) > length(zoomlevel) & length(zoomlevel) == 1){
      zoomlevel <- lapply(1:length(data), function(x){unlist(zoomlevel)})
    }
  }
  
  
  # prepare map parameter
  if(missing(size)) size <- NA
  if(missing(center)) center <- NA
  if(missing(zoom)) zoom <- NA
  if(any(is.na(data))) {
    center <- c(0,0)
    zoom <- 2
  }
  
  # prepare controls
  zoom.ctrl <- scale.ctrl <- layer.ctrl <- legend.ctrl <- FALSE
  if(length(controls)==1 && !is.na(controls)) if(controls=="all") controls <- list("zoom", "scale", "layer", "legend")
  if(!any(is.na(controls))) {
    if(any(controls=="zoom")) zoom.ctrl <- TRUE
    if(any(controls=="scale")) scale.ctrl <- TRUE
    if(any(controls=="layer")) layer.ctrl <- TRUE
    if(any(controls=="legend")) legend.ctrl <- TRUE
  }
  if(layer.ctrl && length(data)>1 && !incl.data) warning("To add data layers to layer control, set 'incl.data=TRUE'", call.=FALSE)
  
  # prepare file path
  dir.create(file.path(dest, gsub(" ", "_", title)), showWarnings=FALSE)
  if(any(!is.na(data)) && !incl.data) {
    for(n in 1:length(data)) file.copy(data[[n]], file.path(dest, gsub(" ", "_", title)), overwrite=overwrite)
  }
  filePath <- file.path(dest, gsub(" ", "_", title), paste0(gsub(" ", "_", title), ".html"))
  
  # brew
  brew(system.file("templates/main.brew", package="leafletR"), filePath) 
  
  # finish
  class(filePath) <- "leaflet"
  message("\nYour leaflet map has been saved under ", filePath)
  invisible(filePath)
}
