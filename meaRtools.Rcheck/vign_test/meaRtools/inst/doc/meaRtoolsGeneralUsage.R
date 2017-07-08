## ------------------------------------------------------------------------
#install.packages( "meaRtools",repos="http://cran.us.r-project.org")

## ------------------------------------------------------------------------
library(meaRtools)
library(plyr)
library(ggplot2)
library(reshape2)

## ------------------------------------------------------------------------

# set path to "_spike_list.csv" files from the file path in 'filesPath'
spk_list_files<-c(system.file("extdata","exampleRecording_1012016_plate1_DIV1_spike_list.csv.gz",package = "meaRtools"),
                system.file("extdata","exampleRecording_1012016_plate1_DIV3_spike_list.csv.gz",package = "meaRtools"),
                system.file("extdata","exampleRecording_1012016_plate1_DIV4_spike_list.csv.gz",package = "meaRtools"))

# set the recording layout file "_expLog.csv"
experimental_log_file <- system.file("extdata","exampleRecording_1012016_plate1_expLog.csv.gz",package = "meaRtools")

## ------------------------------------------------------------------------
# The next command will get the directory of the csv files
data_dir<-dirname(spk_list_files[1])

# create the output directory as /Analysis under the data_dir
output_dir<-paste0( data_dir , "/Analysis" ) 
suppressWarnings(  dir.create(output_dir) )

# create the output directory for single recording analysis 
output_perDIV_dir<-paste0( data_dir , "/Analysis/outputPerDIV" ) 
suppressWarnings(  dir.create(output_perDIV_dir) )

# create the output directory for R objects of analyzed recordings 
r_object_dir<-paste0( data_dir , "/Analysis/R_Objects" )
suppressWarnings(  dir.create(r_object_dir) )

# create the output directory for log files
log.dir<-paste0( output_dir , "/LogFiles" ) 
suppressWarnings(  dir.create(log.dir) )

# For organization sake, set a list object to hold all output directories 
analysis<-list(spikeFiles = spk_list_files, output_dir = output_dir, Routput_dir = r_object_dir, output_perDIV_dir = output_perDIV_dir)


## ------------------------------------------------------------------------
# A loop to go over all three recording files
for (i in 1:length(spk_list_files)){
  #save title for output file name
  title<-strsplit(basename(spk_list_files[i]), ".csv")[[1]][1]
  #load plate design info for each file in the list
  plate_chem_info<-get.experimental.log.file(spk_list_files[i], experimental_log_file)
  
    # convert the spike list data to a 'spike.list' class Robject
  analysis$Robject[i]<-read_spikelist(key=title, spk_list_file=spk_list_files[i],          chem_info=plate_chem_info,r_object_dir=r_object_dir) 
}

## ------------------------------------------------------------------------
data("parameters")

## ------------------------------------------------------------------------

# Select burst algorithm
parameters$burst_type="ps"

# Construct the 'spike.list' object and calculate spike features
s<-calculate_spike_features(analysis$Robject, parameters)

# Detect bursts and calculate their feature statistics
s<-calculate_burst_features(s)

# Iterate through all the recordings to calculate inter-spike intervals and well level mean firing rate and add that to the 'spike.list' object

for (i in 1:length(s)) {
  s[[i]] <- calculate_isis(s[[i]])
  s[[i]]$well_stats <- compute_mean_firingrate_by_well(s[[i]])
}


## ------------------------------------------------------------------------
s[[1]]$spikes$B3_41

## ------------------------------------------------------------------------
s[[2]]$allb$E7_42

## ------------------------------------------------------------------------

# Iterate through all the recordings
for (i in 1:length(s)) {

  #Calculate Network Spikes
  nspikes_old <- calculate.network.spikes(s[[i]],parameters$sur, parameters$ns.N, parameters$ns.T)
  
  # Extract network spike features that will be printed later
  nspikes <- summarize.network.spikes(s[[i]],nspikes_old,ns.E = 1, parameters$sur)
  
  # Add network spike data to the 'spike.list' object
  s[[i]]$ns.all<-nspikes$ns.all
}

## ------------------------------------------------------------------------

s[[i]]$ns.all$B5$en.brief


## ------------------------------------------------------------------------

   nb.list <- calculate.network.bursts(s,parameters$Sigma,
                                       parameters$min_electrodes,
                                       parameters$local_region_min_nAE)
    
    nb.features <- NB.matrix.to.feature.dfs( nb.list$nb.features.merged )

    # attach data to s object
    for (i in 1:length(s) ){
      s[[i]]$nb.all<-nb.list$nb.all[[i]]
      s[[i]]$data.frame$nb.features<-nb.list$nb.features[[i]]
    }
    

## ------------------------------------------------------------------------

# print spikes graphs (pdf format) for each recording
plot_plate_summary_for_spikes(s,analysis$output_perDIV_dir)

# write spike feature tables for each recording
suppressWarnings(write_plate_summary_for_spikes(s,analysis$output_perDIV_dir))

## ------------------------------------------------------------------------
# plot burst pdfs for each recording
suppressWarnings(plot_plate_summary_for_bursts(s,analysis$output_perDIV_dir,parameters))

# write burst feature tables for each recording
write_plate_summary_for_bursts(s,analysis$output_perDIV_dir)

## ------------------------------------------------------------------------
i=1 
# Get plate name
basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]

#Use the next commands for plotting all the ns graphs. Try opening a pdf file so that all will be printed to the same file (which is automatically done for burst features):

pdf(file=paste0(analysis$output_perDIV_dir,"/ns_plot.pdf"))
IGM.xyplot.network.spikes(nspikes)	
plot_active_wells_network_spikes(nspikes)
dev.off()

# write network spike data to output file
write.network.spikes.to.csv(s[[i]],nspikes,analysis$output_perDIV_dir)

# Check the graphs and csvs printed under the analysis$output_perDIV_dir path

## ------------------------------------------------------------------------
spike.features = aggregate_features(s, "spike",parameters)
ns.features = aggregate_features(s, "ns",parameters)
burst.features = aggregate_features(s, "burst",parameters)

# printing spike features nAE
spike.features$nAE

#Feel free to explore the spike/ns/burst and nb.features for the different features they offer

## ------------------------------------------------------------------------

# All uncalculated aEs were set previously to NA, convert all those to 0 aE before the filter
nae <- spike.features$nAE
nae[is.na(nae)] <- 0

# filter spike wells
spike.features = lapply(spike.features, function(x) filter_wells( x, nae, parameters$well_min_rate, parameters$well_filter_maximum_DIV_inactive_ratio))

# filter network burst wells
nb.features <- lapply(nb.features, function(x) filter_wells(x, nae, parameters$well_min_rate, parameters$well_filter_maximum_DIV_inactive_ratio ))
# re-order features by well name
nb.features <- lapply(nb.features, function(x) x[order(x[,'well']),])

# printing spike features nAE after filter
spike.features$nAE


## ------------------------------------------------------------------------
#write csvs 
write_features_to_files(s, spike.features, analysis$output_dir, "spikes")
write_features_to_files(s, burst.features, analysis$output_dir, "bursts")
write_features_to_files(s, ns.features, analysis$output_dir, "ns")
write_features_to_files(s, nb.features, analysis$output_dir, "nb")

## ------------------------------------------------------------------------

suppressMessages(permute.features.and.plot(s, "untreated", parameters$perm.n, spike.features, "spikes", analysis$output_dir))
suppressMessages(permute.features.and.plot(s, "untreated", parameters$perm.n, burst.features, "bursts", analysis$output_dir))
suppressMessages(permute.features.and.plot(s, "untreated", parameters$perm.n, ns.features, "ns", analysis$output_dir))
suppressMessages(permute.features.and.plot(s, "untreated", parameters$perm.n, nb.features, "nb", analysis$output_dir))


## ------------------------------------------------------------------------
result <- suppressWarnings(dist_perm(paste0(dirname(spk_list_files[1]),"/Analysis/outputPerDIV/distributionFiles/exampleRecording_1012016_plate1_DATE_TIME_IBI_distributions.csv"),1000,"untreated","treatX"))

plot(result$data_wt_original,col="blue",main=basename,type="l",lwd=3,xlab="IBI")
points(result$data_ko_original,col="green",type="l",lwd=3)
par(mfrow=c(1,1))  
mtext(side = 1, at = 0, line = 4,
          text = paste("P.value EMD after 1000 permutations: ",format((1-result$perm_EMD), digits = 2),sep=""),col = "black",cex= 0.9,adj=0)    

## ------------------------------------------------------------------------
suppressWarnings(result <- dist_perm(paste0(dirname(spk_list_files[1]),"/Analysis/outputPerDIV/distributionFiles/exampleRecording_1012016_plate1_DATE_TIME_IBI_distributions.csv"),1000,"untreated","treatX"))

plot(result$data_wt,col="blue",main=basename,type="l",lwd=3,xlab="IBI")
points(result$data_ko,col="green",type="l",lwd=3)
par(mfrow=c(1,1))  
mtext(side = 1, at = 0, line = 4,
      text = paste("P.value Max distance after 1000 permutations: ",format((1-result$perm_p), digits = 3),sep=""),col = "black",cex= 0.9,adj=0)    

