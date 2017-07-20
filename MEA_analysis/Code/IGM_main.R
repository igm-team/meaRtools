options(warn=-1)
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(meaRtools)))

#   	    program  : IGM_main.R
# 								
# 			purpose: this program now takes apart spike_lists and makes them into data csv's
#

if (!interactive()){
  args <- commandArgs(trailingOnly = FALSE)
  Path=dirname(strsplit(args[4], "=")[[1]][2])
  setwd(dirname(Path))
} else { #add these just for running code in R 
  Path = paste(dirname(getwd()),"/MEA_analysis/Code",sep="")
  #Path = '/Users/qw2192/Desktop/mea/MEA_analysis/Code';
  setwd(dirname(Path))
}
# Load parameters, called 'parameters'
source(paste(Path,"/parameters.r",sep=""), echo=FALSE)

if(!exists('parameters') ){
  stop("missing parameters file in 'MEA_Analsyis/Code/parameters.R' ")
}


#detect parameter file added or add data
if (interactive()){
  spkListFiles<-get_data(caption="Choose spike list file(s)")
  ExperimentalLogFile<-get_data(caption="Choose experimental log file")

}

cat("\n")
cat("Setting directories.\n")
#determine directory for R code and load function bank 

data.dir<-dirname(spkListFiles[1])
#directory of Robjects files
output.dir<-paste0( data.dir , "/Analysis" ) 
suppressWarnings(  dir.create(output.dir) )
output.perDIV.dir<-paste0( data.dir , "/Analysis/outputPerDIV" ) 
suppressWarnings(  dir.create(output.perDIV.dir) )
Robject.dir<-paste0( data.dir , "/Analysis/R_Objects" )
suppressWarnings(  dir.create(Robject.dir) )
log.dir<-paste0( output.dir , "/LogFiles" ) 
suppressWarnings(  dir.create(log.dir) )

analysis<-list(spikeFiles = spkListFiles,output.dir = output.dir,Routput.dir = Robject.dir,
               output.perDIV.dir = output.perDIV.dir
               )

write.table(as.matrix(parameters), file = paste0(log.dir,"/parameters_",parameters$timeStamp,".txt"),quote = F,col.names = F)
##++++++++++++++++++++++++++++++  Analysis section    +++++++++++++++++++++++++++++++++
L=length(spkListFiles)
fileNum=1
for (i in 1:L){
  title<-strsplit(basename(spkListFiles[i]), ".csv")[[1]][1]
  #get plate chemical info for each file in the list
  plate.chem.info<-get_experimental_log_file(spkListFiles[i], ExperimentalLogFile)
  if (length(plate.chem.info$well) == 0){
    # No data in the plate.chem.info 
    cat(paste("No data for file",spkListFiles[i],"removing file from analysis."))
  }else{
    analysis$Robject[fileNum]<-read_spikelist(key=title, 
                                                  spk_list_file=spkListFiles[i], 
                                                  chem_info=plate.chem.info,r_object_dir=Robject.dir) 
    fileNum=fileNum+1
  }
}

cat("\n")
cat("Making R-objects....\n")
#++++++++++++++++++++++   make s object
s<-calculate_spike_features(analysis$Robject, parameters)

if (length(s)>0) {
  s<-calculate_burst_features(s)
  for (i in 1:length(s)) {
    #Calculate Network Spikes
    
    
    nspikes.old <- calculate_network_spikes(s[[i]],parameters$sur, parameters$ns_n, parameters$ns_t)
    nspikes <- summarize_network_spikes(s[[i]],nspikes.old,ns_e = 1, parameters$sur)
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste( "Calculating network spikes for ", basename ,"...\n") )
    
    # plot ns data if there are network spikes
    if (parameters$ns_plot){
      if (has_network_spikes(nspikes)) {
        NSPlotPath <- paste(analysis$output.perDIV.dir,"/",basename,"_ns.pdf",sep="")
        pdf(file=NSPlotPath)
        xyplot_network_spikes(nspikes)	
        plot_active_wells_network_spikes(nspikes)
        dev.off()
      }
    }
    
    # write network spike data to output file
    if (parameters$ns_csv){
      write_network_spikes_to_csv(s[[i]],nspikes,analysis$output.perDIV.dir)
    }

    
    s[[i]]$ns_all<-nspikes$ns_all
    
    s[[i]] <- calculate_isis(s[[i]])
    s[[i]]$well_stats <- compute_mean_firingrate_by_well(s[[i]])
    
    
  }
  
  # Bursting data - plot and print output csv
  if (parameters$burst_plot){
    batchname <- get_project_plate_name(s[[1]]$file)
    BatchPlotPath = paste(analysis$output.perDIV.dir,"/",batchname ,"_plot.pdf",sep="")
    pdf(file=BatchPlotPath)
    plot_mean_firingrate_by_well_by_div(s)
    plot_mean_firingrate_by_eletrode_by_div(s)
    dev.off()
  }
  
  
  #spiking
  if (parameters$spike_plot){
    suppressWarnings(plot_plate_summary_for_spikes(s,analysis$output.perDIV.dir))
    
  }
  if (parameters$spike_csv){
    suppressWarnings(write_plate_summary_for_spikes(s,analysis$output.perDIV.dir))
    
  }
  
  
  
  # bursting
  if (parameters$burst_plot){
    suppressWarnings(plot_plate_summary_for_bursts(s,analysis$output.perDIV.dir,parameters))
  
  }
  if (parameters$burst.csv){
    suppressWarnings(write.plate.summary.for.bursts(s,analysis$output.perDIV.dir))
    
  
  }
  
} else{
  cat("\n")
  cat("Insufficient data passed filter parameters for analysis.\n") 
}

# extract NB features, the parameters are defined in parameters.R under NB section
if (parameters$want_nb){ 
  
  if (length(s)>0) {
    cat("\n")
    cat("calculating network bursts...\n") 
    nb.list<- calculate_network_bursts(s,parameters$Sigma,
                                       parameters$min_electrodes,
                                       parameters$local_region_min_nAE)
    
    if ( !is.null( nb.list$nb_features_merged  ) ){
      nb.features <- nb_matrix_to_feature_dfs( nb.list$nb_features_merged )
    } else{
      nb.features<-c()
    }
    
    # attach data to s object
    for (i in 1:length(s) ){
      s[[i]]$nb_all<-nb.list$nb_all[[i]]
      s[[i]]$data.frame$nb.features<-nb.list$nb_features[[i]]
    }
    
    nb.file <- paste(analysis$output.perDIV.dir,"/",get_project_plate_name(s[[1]]$file),"_nb.csv",sep="")
    write.csv(nb.features, file = nb.file, row.names = FALSE)
    
  }
}


#+++++++++++++++  save S object
if (length(s)>0) {
  for (i in 1:length(s)){
    basename<- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste0( "Saving file ", basename ,"...\n") )
    rm('S')
    S=s[[i]]
    # Save each s object as S in RData for future use
    save(S, file = paste0(analysis$Routput.dir,"/",basename,".RData") )
    
  }
}



# Permutation and plotting
if (length(s)>0) {
  # aggregate
  cat("\n")
  cat("building feature table...\n") 
  
  spike.features = aggregate_features(s, "spike",parameters)
  ns.features = aggregate_features(s, "ns",parameters)
  burst.features = aggregate_features(s, "burst",parameters)
  
  #filter 
  nae <- spike.features$nae
  nae[is.na(nae)] <- 0
  
  spike.features = lapply(spike.features, function(x) filter_wells( x , nae, parameters$well_min_rate, parameters$well_max_div_inactive_ratio  ))
  ns.features = lapply(ns.features, function(x) filter_wells(x, nae, parameters$well_min_rate, parameters$well_max_div_inactive_ratio))
  burst.features = lapply(burst.features, function(x) filter_wells(x, nae, parameters$well_min_rate, parameters$well_max_div_inactive_ratio))

  if (parameters$want_nb){
    nb.features <- lapply(nb.features, function(x) filter_wells(x, nae, parameters$well_min_rate, parameters$well_max_div_inactive_ratio))		   
    nb.features <- lapply(nb.features, function(x) x[order(x[,'well']),])
  }
  
  #write csvs 
  write_features_to_files(s, spike.features, analysis$output.dir, "spikes")
  write_features_to_files(s, ns.features, analysis$output.dir, "ns")
  write_features_to_files(s, burst.features, analysis$output.dir, "bursts")
  if (parameters$want_nb){
    write_features_to_files(s, nb.features, analysis$output.dir, "nb")
  }
  
}

if (length(s) > 0) {
  cat("\n")
  cat(paste0( "calculating permutations, # permutaions ", parameters$perm_n , "...\n") )
  # mann whit/perm --> PDF
  wt <- get_wt(s)
  
  suppressMessages(permute_features_and_plot(s, wt, parameters$perm_n, spike.features, "spikes", analysis$output.dir))
  if ( length(ns.features)>0 ){
    suppressMessages(permute_features_and_plot(s, wt, parameters$perm_n, ns.features, "ns", analysis$output.dir))
    
  }
  if ( length(burst.features)>0 ){
    suppressMessages(permute_features_and_plot(s, wt, parameters$perm_n, burst.features, "bursts", analysis$output.dir))
  }
  if (parameters$want_nb){
    suppressMessages(permute_features_and_plot(s, wt, parameters$perm_n, nb.features, "nb", analysis$output.dir))
  }
}

cat("\n")
cat("Done with analysis.\n")