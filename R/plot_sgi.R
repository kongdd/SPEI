#' Plot SGI
#' 
#' @param lwd.grid grid line width
#' @importFrom lubridate date
#' @export 
plot_sgi <- function(z, lwd.grid = 0.3){
    x     <- z$data
    # t     <- date(x)
    date = if (is.null(z$date)) seq_along(x) else z$date
    value <- z$fitted %>% as.numeric()
    
    lines_ylab = 1.8
    cex.main = 1.1
    par(mar = c(1.7, 3, 1, 3), mgp = c(2, 0.6, 0))
    plot(date, value, type = "l", col = "red", xlab =NULL, ylab = "", axes = FALSE)
    axis(2, col = "red", col.axis = "red")
    mtext("SGI", side=2, font = 2, cex = cex.main, line=lines_ylab, col = "red")
    abline(h = 0   , col = "black")
    abline(h = -0.8, col = "red", lty = 2)
    
    par(new = TRUE)    
    plot(date, x, type = "b", yaxt="n", ylab = "")
    axis(4, col = "blue", col.axis = "blue")
    mtext("Water level (m)", side=4, font = 2, cex = cex.main, line = lines_ylab, col = "blue")
    grid_year(date, lwd.grid)
    invisible()
}

#' yearly grid lines
#' @param date Date vector
#' 
#' @importFrom lubridate year ymd
#' @export 
grid_year <- function(date, lwd = 0.3) {
    if (!is.Date(date)) {
        grid()
        return()
    }
    years = year(date)
    # ylab = expression(paste('GPP ( gC ', m^-2, d^-1, ')'))
    if (!is.null(years)){
        date_beg <- ymd( min(years) *1e4 + 0101 )
        date_end <- ymd( max(years) *1e4 + 0101 )

        t_grids  <- seq.Date(date_beg, date_end, by = "year")
        abline(v = t_grids, col = "grey60", lty = 3, lwd = lwd)
        grid(nx = NA, NULL)
    }
}
