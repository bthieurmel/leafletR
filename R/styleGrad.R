#' Graduated styling
#' 
#' Creates a graduated style based on an attribute.
#' 
#' 
#' @aliases styleGrad grads
#' @param prop Property (attribute) of the data to be styled, as string.
#' @param breaks A vector giving the breakpoints between the desired classes.
#' @param right If \code{TRUE} (default) classes are right-closed (left-open)
#' intervals (>= breakpoint). Otherwise classes are left-closed (right-open)
#' intervals (> breakpoint).
#' @param out Handling of data outside the edges of \code{breaks}. One of
#' \code{0} (left and right-closed), \code{1} (left-closed, right-open),
#' \code{2} (left-open, right-closed) or \code{3} (left and right-open).
#' Default is \code{0}.
#' @param style.par Styling parameter as string. One of \code{"col"} (graduated
#' color) or \code{"rad"} (graduated radius). Graduated radius can only be
#' applied to points.
#' @param style.val Styling values, a vector of colors or radii applied to the
#' classes.
#' @param leg Legend title as string. The line break sequence  may be
#' used for line splitting.
#' @param \dots Additional styling parameters, see \code{\link{styleSingle}}
#' for details.
#' @return A graduated style object.
#' @author Christian Graul
#' @seealso \code{\link{styleSingle}}, \code{\link{styleCat}},
#' \code{\link{leaflet}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # prepare data
#' data(quakes)
#' qks <- toGeoJSON(data=quakes, dest=tempdir())
#' 
#' # prepare style
#' range(quakes$mag)	# gives 4.0 and 6.4
#' sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), 
#'   style.val=rev(heat.colors(5)), leg="Richter Magnitude")
#' 
#' # create map
#' map <- leaflet(data=qks, dest=tempdir(), 
#'   title="Fiji Earthquakes", style=sty)
#' 
#' # left-closed intervals
#' # note the gray points on the map: magnitude of 4 is outside the breaks
#' # (which are >4.0, >4.5, >5.0, >5.5, >6.0 and >6.5)
#' sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), right=FALSE, 
#'   style.val=rev(heat.colors(5)), leg="Richter Magnitude")
#' map <- leaflet(data=qks, dest=tempdir(), 
#'   title="Fiji Earthquakes", style=sty)
#' 
#' # handle outliers
#' # include outliers of the upper example by left-opening the break edges 
#' # and adding a sixth color
#' sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), right=FALSE, 
#'   out=2, style.val=rev(heat.colors(6)), leg="Richter Magnitude")
#' map <- leaflet(data=qks, dest=tempdir(), 
#'   title="Fiji Earthquakes", style=sty)
#' 
#' # graduated radius
#' sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), style.par="rad", 
#'   style.val=c(2,5,9,14,20), leg="Richter Magnitude")
#' map <- leaflet(data=qks, dest=tempdir(), 
#'   title="Fiji Earthquakes", style=sty)
#' 
#' # additional styling parameters
#' peak <- toGeoJSON(data=system.file(package="leafletR", "files", 
#'   "peak_sk.kmz"), dest=tempdir())
#' sty <- styleGrad(prop="Name", breaks=seq(750, 2500, by=250), out=3, 
#'   style.val=terrain.colors(9), leg="Elevation", 
#'   col=NA, fill.alpha=1, rad=3)
#' map <- leaflet(data=peak, dest=tempdir(), title="Peak elevation", 
#'   base.map="mqsat", style=sty, popup="Name")
#' }
#' 
#' @export styleGrad
styleGrad <-
function(prop, breaks, right=TRUE, out=0, style.par="col", style.val, leg, ...) {
	breaks <- rev(breaks)
	sp <- c("col", "rad")
	style.par <- sp[pmatch(style.par, sp)]
	style.val <- rev(style.val)
	if(style.par=="col") for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	if(!missing(leg)) leg <- gsub("\n", "<br>", leg)
	
	if(right) op <- ">= " else op <- "> "
	
	stl.val <- style.val
	if(style.par=="col") stl.val <- paste0("\"", style.val, "\"")
	if(style.par=="col") def <- "\"#808080\""
	else if(style.par=="rad") def <- "0"
	if(out==0) { # left and right closed
		grad.style <- paste0("return x ", op, breaks[1], " ? ", def, " :")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste0("       x ", op, breaks[n], " ? ", stl.val[n-1], " :"))
		grad.style <- append(grad.style, paste("       ", def, ";", sep=""))
	} else if(out==1) { # left closed right open
		grad.style <- paste("return x ", op, breaks[1], " ? ", stl.val[1], " :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste0("       x ", op, breaks[n], " ? ", stl.val[n], " :"))
		grad.style <- append(grad.style, paste0("       ", def, ";"))
	} else if(out==2) {
		grad.style <- paste0("return x ", op, breaks[1], " ? ", def, " :")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste0("       x ", op, breaks[n], " ? ", stl.val[n-1], " :"))
		grad.style <- append(grad.style, paste("       ", stl.val[n], ";", sep=""))
	} else { # left and right open
		grad.style <- paste("return x ", op, breaks[1], " ? ", stl.val[1], " :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste0("       x ", op, breaks[n], " ? ", stl.val[n], " :"))
		grad.style <- append(grad.style, paste0("       ", stl.val[n+1], ";"))
	}
	
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
			single.style <- append(single.style, "\"color\": \"#0033ff\"")
			single.style <- append(single.style, "\"weight\": 2")
			single.style <- append(single.style, "\"fillColor\": \"#0033ff\"")
			single.style <- append(single.style, "\"opacity\": 0.5")
		}
	}
	
	grad.style <- list(style=grad.style, add=single.style)
	attr(grad.style, "style.type") <- "graduated"
	attr(grad.style, "property") <- prop
	attr(grad.style, "breaks") <- rev(breaks)
	attr(grad.style, "right") <- right
	attr(grad.style, "out") <- out
	attr(grad.style, "style.par") <- style.par
	if(!missing(leg)) attr(grad.style, "leg") <- leg
	class(grad.style) <- c("leafletr.style", "graduated.style")
	return(grad.style)
}
