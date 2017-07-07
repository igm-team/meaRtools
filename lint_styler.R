library("lintr")
#lint("R/entropy_util.R")
#lint("R/zzz.R")
#lint("R/entropy_unittest.R")
#lint("R/utils.R")
lint("R/spikes.R")

lint("R/spikelist_functions.R")

library("styler")
style_pkg()