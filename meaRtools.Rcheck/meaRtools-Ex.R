pkgname <- "meaRtools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "meaRtools-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('meaRtools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("IGM.aggregate.features")
### * IGM.aggregate.features

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aggregate_features
### Title: Aggregate Feature Data
### Aliases: aggregate_features
### Keywords: MEA Aggregate

### ** Examples

	data("S")
	data("parameters")
	s<-list()
	s[[1]]<-S
	spike_features = suppressWarnings( aggregate_features(s, "spike", parameters))
	ns_features = suppressWarnings( aggregate_features(s, "ns", parameters) )
	burst_features = suppressWarnings( aggregate_features(s, "burst", parameters) )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IGM.aggregate.features", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IGM.plot.active.wells.network.spikes")
### * IGM.plot.active.wells.network.spikes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_active_wells_network_spikes
### Title: plot_active_wells_network_spikes
### Aliases: plot_active_wells_network_spikes
### Keywords: network spikes

### ** Examples

data("S") 
data('parameters')
nspikes <- calculate.network.spikes( S, parameters$sur ,parameters$ns.N, parameters$ns.T )
## pdf(file=NSPlotPath)
## xyplot.network.spikes(nspikes)	
## plot_active_wells_network_spikes(nspikes)
## dev.off()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IGM.plot.active.wells.network.spikes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IGM.plot.plate.summary.for.bursts")
### * IGM.plot.plate.summary.for.bursts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_plate_summary_for_bursts
### Title: Plot burst features
### Aliases: plot_plate_summary_for_bursts
### Keywords: distribution burst

### ** Examples

data("S")  
#plot_plate_summary_for_bursts(S,"/Analysis")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IGM.plot.plate.summary.for.bursts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("S")
### * S

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: S
### Title: example 'S' object
### Aliases: S
### Keywords: datasets

### ** Examples

data('S')
names(S)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("S", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc.burst.distributions")
### * calc.burst.distributions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_burst_distributions
### Title: calculate and plot burst featues distributions
### Aliases: calc_burst_distributions
### Keywords: distributions IBI frequency burst

### ** Examples

# Load exapmple of recording Robject (MEA data structure)
data("S")  
feature="IBI"; 
#calc_burst_distributions(S, min_vals = 15, xlimit = 20, bins_in_sec = 5, 
#feature = feature, per_well = 0, outputdir = "/Analysis")


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc.burst.distributions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc.burst.summary")
### * calc.burst.summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_burst_summary
### Title: Calculate average and standard deviation of the bursting
###   features.
### Aliases: calc_burst_summary
### Keywords: burst frequency duration bursts_per_min IBI isi

### ** Examples

# Load exapmple of recording Robject (MEA data structure)
data("S")  
S$bs<-calc_burst_summary(S)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc.burst.summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_isis")
### * calculate_isis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_isis
### Title: Calculate inter spike intervals
### Aliases: calculate_isis
### Keywords: isis inter spike interval

### ** Examples

data("S")  
S <- calculate_isis(S)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_isis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compute_mean_firingrate_by_well")
### * compute_mean_firingrate_by_well

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_mean_firingrate_by_well
### Title: mean.firingrate.by.well
### Aliases: compute_mean_firingrate_by_well
### Keywords: spikes mfr

### ** Examples

data("S") 

res<-compute_mean_firingrate_by_well(S)
res[1:4,]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_mean_firingrate_by_well", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dist.perm")
### * dist.perm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dist_perm
### Title: Burst distribution permutations
### Aliases: dist_perm
### Keywords: distribution permutation

### ** Examples

#    result <- dist_perm(distributionFilePath,10000,"WT","KO")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dist.perm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("filter.wells")
### * filter.wells

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: filter_wells
### Title: Filter wells
### Aliases: filter_wells
### Keywords: filter well

### ** Examples



	#data("S")
	#data("parameters")
	#s<-list(); s[[1]]<-S
	# spike.features<-aggregate_features(s, feat_type="spike", parameters )
	# nae = spike.features$nAE
  # filtered.spike.features = lapply(spike.features, function(x) filter_wells(x, nae))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("filter.wells", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("generate.raster.plot")
### * generate.raster.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: generate.raster.plot
### Title: generate.raster.plot
### Aliases: generate.raster.plot
### Keywords: utility spikes

### ** Examples

##generate.raster.plot(RobjectFile=NULL,
#                      well.for.raster=NULL, 
#                      interval.for.raster=NULL,
#                      show.bursts=F, 
#                      show.burst.number=F, 
#                      show.networkspikes=F,
#                      show.ns.number=F,
#                      show.nb=F,
#                      window.size=NULL )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("generate.raster.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.burst.info")
### * get.burst.info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_burst_info
### Title: get burst feature information
### Aliases: get_burst_info
### Keywords: burst IBI duration

### ** Examples

data("S")  
S$allb[[1]]



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.burst.info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.data")
### * get.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.data
### Title: get.data
### Aliases: get.data
### Keywords: s-object

### ** Examples

## get.data(caption="Please select a spike-list file for analysis")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.experimental.log.file")
### * get.experimental.log.file

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.experimental.log.file
### Title: get.experimental.log.file
### Aliases: get.experimental.log.file
### Keywords: experimental log

### ** Examples

##masterChemFile<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_expLog.csv" )
      
##spike.list.file<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_DIV1_spike_list.csv" )

##plate.data<-getxperimental.log.file( file=spike.list.file, masterChemFile = masterChemFile )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.experimental.log.file", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.num.AE")
### * get.num.AE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.num.AE
### Title: get.num.AE
### Aliases: get.num.AE
### Keywords: spikes

### ** Examples

data("S") # load data
b<-get.num.AE(S)
b$nAE




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.num.AE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.wt")
### * get.wt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.wt
### Title: Get WT
### Aliases: get.wt
### Keywords: wt treatment

### ** Examples

	data("S")
	s<-list()
	s[[1]]<-S
	##wt <- get.wt(s)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.wt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_file_basename")
### * get_file_basename

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_file_basename
### Title: get_file_basename
### Aliases: get_file_basename
### Keywords: spike-list

### ** Examples


data("S") # load data
get_file_basename(S$file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_file_basename", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_project_plate_name")
### * get_project_plate_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_project_plate_name
### Title: get_project_plate_name
### Aliases: get_project_plate_name
### Keywords: utility

### ** Examples

data("S") # load data
get_project_plate_name(S$file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_project_plate_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("igm_write_ui_to")
### * igm_write_ui_to

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: igm_write_ui_to_log
### Title: igm_write_ui_to_log
### Aliases: igm_write_ui_to_log
### Keywords: log

### ** Examples

# igm_write_ui_to_log(files='/Desktop/logfile.txt',parameter_list, new_file=F )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("igm_write_ui_to", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("isi")
### * isi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: isi
### Title: isi
### Aliases: isi
### Keywords: isi spikes interval

### ** Examples


data("S") # load data
b<-isi(S$spikes[[1]])
S$spikes[[1]][1:4]
b[1:3]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("isi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("load_spikelist")
### * load_spikelist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: load_spikelist
### Title: Load Robject File
### Aliases: load_spikelist
### Keywords: Rdata s

### ** Examples

#    s1 <- load_spikelist(dir to saved Rdata file)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("load_spikelist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mi.find.bursts")
### * mi.find.bursts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mi.find.bursts
### Title: Find bursts
### Aliases: mi.find.bursts
### Keywords: maximum interval burst IBI allb

### ** Examples

data("S")  
allb <- lapply(S$spikes, mi.find.bursts, S$parameters$mi.par )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mi.find.bursts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parameters")
### * parameters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parameters
### Title: A list of parameters with default values that user can
###   customize.
### Aliases: parameters
### Keywords: datasets

### ** Examples

data(parameters)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parameters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("permute.features.and.plot")
### * permute.features.and.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: permute.features.and.plot
### Title: Write PDF
### Aliases: permute.features.and.plot
### Keywords: MEA analysis PDF

### ** Examples

	data("S")
	#spike.features<-aggregate_features(S, feat_type="spike" )
	#wt <- "untreated"
	#output_dir = getwd()
	#permute.features.and.plot(S, wt, np, spike.features, "spikes", output_dir)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("permute.features.and.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_mean_firingrate_by_eletrode_by_div")
### * plot_mean_firingrate_by_eletrode_by_div

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_mean_firingrate_by_eletrode_by_div
### Title: plot.mean.firingrate.by.eletrode.by.div
### Aliases: plot_mean_firingrate_by_eletrode_by_div
### Keywords: firing rate

### ** Examples


data("S") 
s<-list()
s[[1]]<-S
#plot.mean.firingrate.by.eletrode.by.div(s)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_mean_firingrate_by_eletrode_by_div", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_mean_firingrate_by_well_by_div")
### * plot_mean_firingrate_by_well_by_div

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_mean_firingrate_by_well_by_div
### Title: plot.mean.firingrate.by.well.by.div
### Aliases: plot_mean_firingrate_by_well_by_div
### Keywords: firing rate spikes

### ** Examples

data("S") 
s<-list()
s[[1]]<-S
#plot.mean.firingrate.by.well.by.div(s)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_mean_firingrate_by_well_by_div", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_plate_summary_for_spikes")
### * plot_plate_summary_for_spikes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_plate_summary_for_spikes
### Title: plot.plate.summary.for.spikes
### Aliases: plot_plate_summary_for_spikes
### Keywords: spikes

### ** Examples


data("S") 
s<-list()
s[[1]]<-S
## plot.plate.summary.for.spikes(s, outputdir="/Desktop")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_plate_summary_for_spikes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_spikelist")
### * read_spikelist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_spikelist
### Title: Axion convert spk_list to r_object
### Aliases: read_spikelist
### Keywords: spike_list r_object Axion

### ** Examples


##masterChemFile<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_expLog.csv" )
      
##spike_list_file<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_DIV1_spike_list.csv" )

##title<-strsplit(basename(spike_list_file), ".csv")[[1]][1]
# get plate chemical info for each file in the list

##plate_chem_info<-chem_info_2( file=spike_list_file, masterChemFile = masterChemFile )

##r_object_file_name<-read_spikelist(key=title, 
#                                                  spk_list_file=plate_chem_info, 
#                                                  chem_info=plate_chem_info,r_object_dir="/") 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_spikelist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("remove_spikes")
### * remove_spikes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: remove_spikes
### Title: remove_spikes
### Aliases: remove_spikes
### Keywords: spikes

### ** Examples

data("S") # load data
r<-remove_spikes(S, c(-1, -2))

S$channels[1:2] # original 's' object first 2 channels
r$channels[1:2] # first 2 channels have been removed

S$NCells # original count of channels
r$NCells # count of channels after 2 channels removed

S$nspikes # original spike count of first 2 channels
r$nspikes # spike count of first 2 channels after 2 channels removed

# OR keep only first 2 channels
t<-remove_spikes(S, c(1, 2))
t$channels



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("remove_spikes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("si_find_bursts")
### * si_find_bursts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: si_find_bursts
### Title: Find bursts
### Aliases: si_find_bursts
### Keywords: poisson surprise burst

### ** Examples

data("S")  
allb <- lapply(S$spikes, si_find_bursts, S$parameters$s.min )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("si_find_bursts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write.features.to.files")
### * write.features.to.files

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_features_to_files
### Title: Write feature data to an output file
### Aliases: write_features_to_files
### Keywords: spikes bursts network features print

### ** Examples

	data("S")
	s<-list()
	s[[1]]<-S
  spike.features = aggregate_features(s, "spike")

#  write_features_to_files(s, spike.features, analysis$output_dir, "spikes")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write.features.to.files", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write.plate.summary.for.bursts")
### * write.plate.summary.for.bursts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_plate_summary_for_bursts
### Title: Prints bursting features
### Aliases: write_plate_summary_for_bursts
### Keywords: bursts IBI duration frequency

### ** Examples

data("S")  
d<-dir.create(paste0(getwd(),"/Analysis") )
s<-list(); s[[1]]<-S
write_plate_summary_for_bursts(s, paste0(getwd() ) )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write.plate.summary.for.bursts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_plate_summary_for_spikes")
### * write_plate_summary_for_spikes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_plate_summary_for_spikes
### Title: write_plate_summary_for_spikes
### Aliases: write_plate_summary_for_spikes
### Keywords: spikes

### ** Examples

data("S") 
s<-list()
s[[1]]<-S
## path<-system.file()
## write_plate_summary_for_spikes(s , path)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_plate_summary_for_spikes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
