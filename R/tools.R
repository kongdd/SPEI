#' @name Generic-methods-for-spei-objects
#' @title Generic methods for \code{spei} objects.
#' 
#' @aliases print.spi plot.spei plot.spi summary.spei summary.spi
#' 
#' @description 
#' Generic methods for extracting information and plotting \code{spei} objects.
#' 
#' @usage
#' \method{print}{spei}(x, ...)
#' \method{summary}{spei}(object, ...)
#' \method{plot}{spei}(x, ttext, ...)
#' 
#' @param x an object of class \code{spei}.
#' @param object an object of class \code{spei}.
#' @param ttext text to use as part of the plot title
#' @param ... additional parameters, not used at present.
#' 
#' 
#' @details This functions allow extracting information and plotting \code{spei} 
#' objects. \code{print} yields the fitted values, i.e. a time series of SPEI or SPI values. 
#' \code{summary} reports the function call, the parameters of the PDF used, and the time 
#' series of SPEI or SPI values. \code{plot} produces a plot of the time series of SPEI or 
#' SPI values, with blue and red colors for positive and negative values, respectively. If 
#' a reference period was used in the function call it is shown by a shaded area. In the 
#' unlikely case that NA or Inf values were produced, these are shown by circles.
#' 
#' @references 
#' S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index 
#' sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. 
#' \emph{Journal of Climate} \bold{23}: 1696, DOI: 10.1175/2009JCLI2909.1.
#' 
#' 
#' @author Santiago Beguería
#'  
#' 
#' @export
#' 
print.spei <- function (x, ...) {
	print(x$fitted)
}

#' 
#' @title summary of spei/spi
#' 
#' 
#' @description See print.spei
#' 
#' 
#' @details See print.spei
#' 
#' 
#' @rdname Generic-methods-for-spei-objects
#' 
#' 
#' @export
#' 
summary.spei <- function (object, ...) {
	x <- object
	cat('Call:\n')
	print(x$call)
	cat('\nCoefficients:\n')
	for (i in 1:dim(x$coeff)[2]) {
		cat('\t',dimnames(x$coeff)[[2]][i],':\n',sep='')
		tab <- cbind(t(x$coeff[,i,]))
		rownames(tab) <- 1:dim(x$coeff)[3]
		print(tab)
		cat('\nFitted:\n')
		print(x$fitted)
	}
}
