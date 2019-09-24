### Things to do at the end of the package load.
### The name zzz.R is historical.
### example taken from https://github.com/tidyverse/dplyr/blob/master/R/zzz.r

.onLoad <- function(libname, pkgname) {
  add_plateinfo("mcs-8x8-100um", mcs100um_8x8_platelayout)
  
  invisible()
}



