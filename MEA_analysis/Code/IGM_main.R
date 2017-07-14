options(warn=-1)
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(IGM.MEA)))

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
source(paste(Path,"/parameters.R",sep=""), echo=FALSE)

if(!exists('parameters') ){
  stop("missing parameters file in 'MEA_Analsyis/Code/parameters.R' ")
}


#detect parameter file added or add data
if (interactive()){
  spkListFiles<-get.data(caption="Choose spike list file(s)")
  ExperimentalLogFile<-get.data(caption="Choose experimental log file")

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
  plate.chem.info<-get.experimental.log.file(spkListFiles[i], ExperimentalLogFile)
  if (length(plate.chem.info$well) == 0){
    # No data in the plate.chem.info 
    cat(paste("No data for file",spkListFiles[i],"removing file from analysis."))
  }else{
    analysis$Robject[fileNum]<-read.spikelist(key=title, 
                                                  spkListFile=spkListFiles[i], 
                                                  chem.info=plate.chem.info,Robject.dir=Robject.dir) 
    fileNum=fileNum+1
  }
}

cat("\n")
cat("Making R-objects....\n")
#++++++++++++++++++++++   make s object
s<-calculate.spike.features(analysis$Robject, parameters)

if (length(s)>0) {
  s<-calculate.burst.features(s)
  for (i in 1:length(s)) {
    #Calculate Network Spikes
    
    
    nspikes.old <- calculate.network.spikes(s[[i]],parameters$sur, parameters$ns.N, parameters$ns.T)
    nspikes <- summarize.network.spikes(s[[i]],nspikes.old,ns.E = 1, parameters$sur)
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste( "Calculating network spikes for ", basename ,"...\n") )
    
    # plot ns data if there are network spikes
    if (parameters$ns.plot){
      if (has.network.spikes(nspikes)) {
        NSPlotPath <- paste(analysis$output.perDIV.dir,"/",basename,"_ns.pdf",sep="")
        pdf(file=NSPlotPath)
        IGM.xyplot.network.spikes(nspikes)	
        IGM.plot.active.wells.network.spikes(nspikes)
        dev.off()
      }
    }
    
    # write network spike data to output file
    if (parameters$ns.csv){
      write.network.spikes.to.csv(s[[i]],nspikes,analysis$output.perDIV.dir)
    }

    
    s[[i]]$ns.all<-nspikes$ns.all
    
    s[[i]] <- calculate.isis(s[[i]])
    s[[i]]$well.stats <- IGM.compute.mean.firingrate.by.well(s[[i]])
    
    
  }
  
  # Bursting data - plot and print output csv
  if (parameters$burst.plot){
    batchname <- get.project.plate.name(s[[1]]$file)
    BatchPlotPath = paste(analysis$output.perDIV.dir,"/",batchname ,"_plot.pdf",sep="")
    pdf(file=BatchPlotPath)
    IGM.plot.mean.firingrate.by.well.by.div(s)
    IGM.plot.mean.firingrate.by.eletrode.by.div(s)
    dev.off()
  }
  
  
  #spiking
  if (parameters$spike.plot){
    suppressWarnings(IGM.plot.plate.summary.for.spikes(s,analysis$output.perDIV.dir))
    
  }
  if (parameters$spike.csv){
    suppressWarnings(write.plate.summary.for.spikes(s,analysis$output.perDIV.dir))
    
  }
  
  
  
  # bursting
  if (parameters$burst.plot){
    suppressWarnings(IGM.plot.plate.summary.for.bursts(s,analysis$output.perDIV.dir,parameters))
  
  }
  if (parameters$burst.csv){
    suppressWarnings(write.plate.summary.for.bursts(s,analysis$output.perDIV.dir))
    
  
  }
  
} else{
  cat("\n")
  cat("Insufficient data passed filter parameters for analysis.\n") 
}

# extract NB features, the parameters are defined in parameters.R under NB section
if (parameters$want.nb){ 
  
  if (length(s)>0) {
    cat("\n")
    cat("calculating network bursts...\n") 
    nb.list<- calculate.network.bursts(s,parameters$Sigma,
                                       parameters$min_electrodes,
                                       parameters$local_region_min_nAE)
    
    if ( !is.null( nb.list$nb.features.merged  ) ){
      nb.features <- NB.matrix.to.feature.dfs( nb.list$nb.features.merged )
    } else{
      nb.features<-c()
    }
    
    # attach data to s object
    for (i in 1:length(s) ){
      s[[i]]$nb.all<-nb.list$nb.all[[i]]
      s[[i]]$data.frame$nb.features<-nb.list$nb.features[[i]]
    }
    
    nb.file <- paste(analysis$output.perDIV.dir,"/",get.project.plate.name(s[[1]]$file),"_nb.csv",sep="")
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
  
  spike.features = IGM.aggregate.features(s, "spike",parameters)
  ns.features = IGM.aggregate.features(s, "ns",parameters)
  burst.features = IGM.aggregate.features(s, "burst",parameters)
  
  #filter 
  nae <- spike.features$nAE
  nae[is.na(nae)] <- 0
  
  spike.features = lapply(spike.features, function(x) filter.wells( x , nae,parameters$well.min.rate, parameters$well.filter.maximum.DIV.inactive.ratio  ))
  ns.features = lapply(ns.features, function(x) filter.wells(x, nae,parameters$well.min.rate, parameters$well.filter.maximum.DIV.inactive.ratio  ))
  burst.features = lapply(burst.features, function(x) filter.wells(x, nae,parameters$well.min.rate, parameters$well.filter.maximum.DIV.inactive.ratio ))

  if (parameters$want.nb){
    nb.features <- lapply(nb.features, function(x) filter.wells(x, nae, parameters$well.min.rate, parameters$well.filter.maximum.DIV.inactive.ratio ))		   
    nb.features <- lapply(nb.features, function(x) x[order(x[,'well']),])
  }
  
  #write csvs 
  write.features.to.files(s, spike.features, analysis$output.dir, "spikes")
  write.features.to.files(s, ns.features, analysis$output.dir, "ns")
  write.features.to.files(s, burst.features, analysis$output.dir, "bursts")
  if (parameters$want.nb){
    write.features.to.files(s, nb.features, analysis$output.dir, "nb")
  }
  
}

if (length(s) > 0) {
  cat("\n")
  cat(paste0( "calculating permutations, # permutaions ", parameters$perm.n , "...\n") )
  # mann whit/perm --> PDF
  wt <- get.wt(s)
  
  suppressMessages(permute.features.and.plot(s, wt, parameters$perm.n, spike.features, "spikes", analysis$output.dir))
  if ( length(ns.features)>0 ){
    suppressMessages(permute.features.and.plot(s, wt, parameters$perm.n, ns.features, "ns", analysis$output.dir))
    
  }
  if ( length(burst.features)>0 ){
    suppressMessages(permute.features.and.plot(s, wt, parameters$perm.n, burst.features, "bursts", analysis$output.dir))
  }
  if (parameters$want.nb){
    suppressMessages(permute.features.and.plot(s, wt, parameters$perm.n, nb.features, "nb", analysis$output.dir))
  }
}

cat("\n")
cat("Done with analysis.\n")