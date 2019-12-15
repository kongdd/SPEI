---------------------------
 NEWS for R Package "SPEI"
---------------------------



__Version history:

_Version 1.7.2, June 2017 (current on github).

1. Fix NA value error in SPI and SPEI
2. Add groundwater drought index (SGI)

_Version 1.7.1, June 2017 (current on github).

1. Corrected an error in `<spei>` function, which was not working when distribution was Gamma or PeasonIII and using user provided parameters. (Fixed by Emanuele Cordano, emanuele.cordano@gmail.com -- ecor)
2. Added probability of monthly precipitation = 0 (pze) when using user provided parameters. (Fixed by Emanuele Cordano, emanuele.cordano@gmail.com -- ecor) 

_Version 1.7, June 2017 (current on CRAN).

1. Corrected a bug in the \code{\link{kern}} function which resulted in a multiplicative kernel instead of an additive one such the one expected in the \code{\link{spei}} and \code{\link{spi}} functions.
2. Plotting methods for \code{spei} objects completely rewritten, now using \code{ggplot2}. Additionally, a bug causing a bad representation of time series starting in months other than January has been corrected.

_Version 1.6, September 2013.
1. Corrected an error in the function \code{\link{thornthwaite}} which resulted in wrong potential evapotranspiration estimates when a multivariate time series was used as input.
2. Corrected an error in the function \code{\link{spi}} which resulted in wrong handling of zero precipitation months when using the Gamma or PearsonIII distribution.
3. Minor fixes to the \code{\link{spi}} and \code{\link{plot.spei}} functions to correctly handle \code{spei} objects when they result from a call to \code{\link{spi}}.
4. Modification to the \code{\link{kern}} function, which now yields kernel coefficients averaging one.
5. Corrected an error in the functions \code{\link{spi}} and \code{\link{spei}} which resulted in ub-pwm method being used irrespective of the value of the \code{fit} parameter used, when using the 'Gamma' or 'PearsonIII' distributions.
6. Minor changes to the documentation.

_Version 1.5, May 2013.
1. Optimization of function \code{\link{spei}}, now using embed() for accumulating the data at the desired time scale.

_Version 1.4, May 2013.
1. Minor fixes to functions \code{\link{penman}} and \code{\link{pwm}}.
2. Documentation of the penman function defined by mistake ed as the saturation vapour pressure, while it should read 'actual vapour pressure'.
3. Function zzz.R added to display basic information about the SPEI package at startup.
4. Function \code{\link{SPEINews}} added to display the NEWS file.

_Version 1.3, March 2013.
1. Minor fixes to functions \code{\link{spei}} and \code{\link{penman}}.
2. Added new option for user-supplied SPEI parameters in the \code{\link{spei}} function. This overrides the fitting of a probability function to the data.
3. Added new dataset \code{\link{cabinda}} from Allen et al. (1998).

_Version 1.2, October 2012.
1. Fixed a bug causing several functions to fail when a time series not belonging to matrix class was provided.
2. Function \code{\link{plot.spei}} now distinguises between calls to spei and spi and labels the axis accordingly.

_Version 1.1, March 2012.
1. Functions \code{\link{spei}} and \code{\link{spi}} now yield an object of class "spei".
2. New functions for summarizing and plotting "spei" objects are provided.
3. An option to establish a reference period for the computation of the indices has been implemented in functions \code{\link{spei}} and \code{\link{spi}}.

_Version 1.0, January 2012.
First release of the SPEI package.

__To do (work in progress):

1. Complete documentation for pwmLC.Rd.
2. Review method plot.spei() that produces wrong results in some cases.
3. Implement parallel processing.
4. Analysis functions.
