library(readxl)
library(data.table)
library(lubridate)
library(magrittr)
library(foreach)
library(iterators)

df <- read_xls("INPUT/gd_water.xls") %>% data.table()
df[, date := make_date(year, month, 1)]
df <- reorder_name(df, c("year", "month", "date"))

## 1. debug with one site, 修复钉值
sitenames = colnames(df)[-(1:3)] %>% set_names(., .)
sitename  = sitenames[1]

date <- df$date
x = df[, .SD, .SDcol = sitename][[1]]
z = sgi(x, 3, na.rm = TRUE, date = date)

{  
  write_fig(expression({
    plot_sgi(z,0.3)  
  }), "sgi_ex.pdf", 10, 5)
}

## 2. all sites
lst = foreach(sitename = sitenames, i = icount()) %do% {
  x = df[, .SD, .SDcol = sitename][[1]]
  z = sgi(x, 3, na.rm = TRUE, date = date)
}

write_fig(expression({
    par(mfrow = c(2, 4))
    temp = foreach(r = lst, sitename = sitenames, i = icount(1)) %do% {
      plot_sgi(r)
      title(sitename)
    }
}), "sgi_zhanj.pdf", 10, 5)

# d = r %>% {data.table(date =date(.$data), data = .$data, fit = .$fitted)}
