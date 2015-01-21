#' Categorized styling
#' 
#' Creates a categorized style based on an attribute
#' 
#' If \code{val} does not cover all data values, the unspecified data values
#' are colored gray. By adding an extra color for unspecified data values to
#' \code{style.val}, an "other"-category is shown in the legend.
#' 
#' @aliases styleCat cats
#' @param prop Property (attribute) of the data to be styled, as string.
#' @param val A vector giving the data values to be used as categories.
#' @param style.par Styling parameter as string. One of \code{"col"}
#' (categorized color) or \code{"rad"} (categorized radius). Categorized radius
#' can only be applied to points.
#' @param style.val Styling values, a vector of colors or radii applied to the
#' categories given by \code{val}. See details for unspecified data values.
#' @param leg Legend title as string. The line break sequence may be
#' used for line splitting.
#' @param \dots Additional styling parameters, see \code{\link{styleSingle}}
#' for details.
#' @return A categorized style object.
#' @author Christian Graul
#' @seealso \code{\link{styleSingle}}, \code{\link{styleGrad}},
#' \code{\link{leaflet}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # prepare data
#' dat <- system.file(package="leafletR", "files", "park_sk.geojson")
#' 
#' # simple categorizing
#' sty <- styleCat(prop="lynx", val=c("yes", "no"), 
#'   style.val=c("green", "red"), leg="Lynx occurrence")
#' map <- leaflet(data=dat, dest=tempdir(), title="Lynx", 
#'   style=sty)
#' 
#' # just one category
#' sty <- styleCat(prop="wisent", val="yes", style.val="red", 
#'   leg="Wisent occurrence")
#' map <- leaflet(data=dat, dest=tempdir(), title="Wisent", 
#'   style=sty)
#' 
#' # get nice colors using ColorBrewer
#' require(RColorBrewer)
#' pal <- brewer.pal(7, "Dark2")
#' sty <- styleCat(prop="year", val=c("1949", "1967", "1978", "1988", 
#'   "1997", "1998", "2002"), style.val=pal, leg="established:\n")
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   title="National parks", style=sty)
#' 
#' # add 'other'-category to legend
#' require(RColorBrewer)
#' pal <- brewer.pal(7, "Dark2")
#' sty <- styleCat(prop="year", val=c("1997", "1998", "2002"), 
#'   style.val=pal, leg="established:\n")
#' map <- leaflet(data=dat, dest=tempdir(), 
#'   title="National parks", style=sty)
#' 
#' # additional styling parameters
#' sty <- styleCat(prop="brown_bear", val=c("yes", "no"), 
#'   style.val=c("darkgreen", "red"), leg="Brown bear\noccurrence", 
#'   alpha=1, lwd=4, fill=NA)
#' map <- leaflet(data=dat, dest=tempdir(), title="Brown bear", 
#'   style=sty)
#' }
#' 
#' @export styleCat
styleCat <-
function(prop, val, style.par="col", style.val, leg, ...) {
	sp <- c("col", "rad")
	style.par <- sp[pmatch(style.par, sp)]
	
	if(style.par=="col") for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	if(!missing(leg)) leg <- gsub("\n", "<br>", leg)
	
	stl.val <- style.val
	if(style.par=="col") stl.val <- paste0("\"", style.val, "\"")
	cat.style <- paste0("return x == \"", val[1], "\" ? ", stl.val[1], " :")
	for(n in 2:length(val)) cat.style <- append(cat.style, paste0("       x == \"", val[n], "\" ? ", stl.val[n], " :"))
	if(length(style.val)>length(val)) cat.style <- append(cat.style, paste0("       ", stl.val[length(val)+1], ";"))
	else cat.style <- append(cat.style, paste("       \"\";", sep="")) 
	
	s <- list(...)
	single.style <- NULL
	if(length(s)>0) {
		if(any(names(s)=="col")) {
			if(is.na(s$col)) single.style <- append(single.style, "\"stroke\": false")
			else single.style <- append(single.style, paste0("\"color\": \"", getHex(s$col), "\""))
		} else if(style.par=="rad") single.style <- append(single.style, "\"color\": \"#0033ff\"")
		if(any(names(s)=="lwd")) single.style <- append(single.style, paste("\"weight\":", s$lwd))
		else if(style.par=="rad") single.style <- append(single.style, "\"weight\": 2")
		if(any(names(s)=="alpha")) single.style <- append(single.style, paste("\"opacity\":", s$alpha))
		else if(style.par=="rad") single.style <- append(single.style, "\"opacity\": 0.5")
		if(any(names(s)=="fill")) {
			if(is.na(s$fill)) single.style <- append(single.style, "\"fill\": false")
			else single.style <- append(single.style, paste0("\"fillColor\": \"", getHex(s$fill), "\""))
		} else if(style.par=="rad") single.style <- append(single.style, "\"fillColor\": \"#0033ff\"")
		if(any(names(s)=="fill.alpha")) single.style <- append(single.style, paste("\"fillOpacity\":", s$fill.alpha))
		else single.style <- append(single.style, "\"fillOpacity\": 0.5")
		if(any(names(s)=="rad")) single.style <- append(single.style, paste("\"radius\":", s$rad))
	} else {
		single.style <- append(single.style, "\"fillOpacity\": 0.5")
		if(style.par=="rad") {
			single.style <- append(single.style, "\"weight\": 2")
			single.style <- append(single.style, "\"color\": \"#0033ff\"")
			single.style <- append(single.style, "\"fillColor\": \"#0033ff\"")
			single.style <- append(single.style, "\"opacity\": 0.5")
		}
	}
	
	cat.style <- list(style=cat.style, add=single.style)
	attr(cat.style, "style.type") <- "categorized"
	attr(cat.style, "property") <- prop
	attr(cat.style, "values") <- val
	attr(cat.style, "style.par") <- style.par
	if(length(style.val)>length(val)) {
		attr(cat.style, "na") <- "other"
		#attr(cat.style, "na.val") <- stl.val[length(val)+1]
	}
	if(!missing(leg)) attr(cat.style, "leg") <- leg
	class(cat.style) <- c("leafletr.style", "categorized.style")
	return(cat.style)
}
