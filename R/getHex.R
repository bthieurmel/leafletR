#' Internal function for color conversion
#' 
#' Converts different color values to hexadecimal colors. Used by
#' \code{\link{styleSingle}}, \code{\link{styleGrad}} and
#' \code{\link{styleCat}}.
#' 
#' 
#' @param col Color value as number, name or hex code with alpha.
#' @return Hexadecimal color value as string.
#' @author Christian Graul
#' @seealso \code{\link{styleSingle}}, \code{\link{styleGrad}},
#' \code{\link{styleCat}}
#' @keywords methods internal
getHex <-
function(col) {
	if(class(col)=="character") {
		if(nchar(col)==7 && substr(col, 1, 1)=="#") col <- col
		else if(nchar(col)==9 && substr(col, 1, 1)=="#") col <- substr(col, 1, 7)
		else {
			if(!any(colors()==col)) stop("Unknown color")
			else col <- rgb(col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255)
		}
	} else if(is.numeric(col)) if(col>=0) {
		if(col>8) col <- col%%8
		col <- c("white", "black", "red", "green", "blue", "cyan", "magenta", "yellow", "gray")[col+1]
		col <- rgb(col2rgb(col)[1,], col2rgb(col)[2,], col2rgb(col)[3,], maxColorValue=255)
	} else stop("Unknown color")
	
	return(col)
}
