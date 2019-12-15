#' Groundwater drought index
#' 
#' @description
#' `sgi`: generalized normal distribution (pelgno) used here. 
#' 
#' @inheritParams spi
#' 
#' @example man/examples/ex-sgi.R
#' @importFrom lmomco pargno
#' @export
sgi <- function(data, scale, kernel=list(type='rectangular',shift=0),
                distribution='GNorm', fit='ub-pwm', na.rm=FALSE,
                ref.start=NULL, ref.end=NULL, x=TRUE, params=NULL, ...){
  sol <- spei2(data, scale, kernel, distribution, fit, na.rm,
               ref.start, ref.end, x, params, ...)
  sol$call <- match.call(expand.dots=FALSE)
  
  values <- sol$fitted #%<>% as.numeric()
  values[is.na(data)] <- NA
  
  sol$fitted <- values
  sol$data   <- data
  return(sol)
}
