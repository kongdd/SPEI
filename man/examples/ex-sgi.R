library(readxl)
library(data.table)
library(lubridate)
library(magrittr)

d_gd <- read_xls('/home/kongdd/github/groudwater/inst/extdata/groundwater.xls') %>% 
    data.table() #%>% melt(c("year", "month"))
d_gd[, date := make_date(year, month, 1)]

{
    x <- d_gd$`L04（B）`
    plot(x, type = "b"); grid()
    
    r <- sgi(x,  scale = 3, na.rm = TRUE)
    value <- r$fitted %>% as.numeric() #%>% plot()
    
    par(new = TRUE)
    plot(value, type = "l", col = "red", axes = TRUE)
    abline(h = -0.8, col = "red")
}
