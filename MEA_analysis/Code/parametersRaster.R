# executed : 26/10/17 15:26:28
r_object_file<-'/Users/dh2744/Dropbox/Columbia/Software/github/meaRtools/test/exampleRecording/Analysis/R_Objects/exampleRecording_1012016_plate1_DIV4.RData' 
well_for_raster<- 'B2'
interval_for_raster<-c(0.0,100.0)
show_bursts<- TRUE
show_burst_number<- TRUE
show_networkspikes<- TRUE
show_ns_number<- TRUE
show_nb<- TRUE
window_size<- 20
library(meaRtools)
generate_raster_plot(r_object_file= r_object_file,
outputdir = NULL, well_for_raster= well_for_raster,
interval_for_raster=interval_for_raster,
show_bursts=show_bursts,
show_burst_number=show_burst_number,
show_nb=show_nb,
show_networkspikes=show_networkspikes,
show_ns_number =show_ns_number,
window_size = window_size )
