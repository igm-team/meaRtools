## Functions for reading in text in a generic format.
##

##' Construct an MEA object by importing from text files.
##'
##' This function can be used to read in spike times from text files.
##' 
##' @title Construct an MEA object by importing from text files.
##' @param spike_text_file CSV with Spike time information
##' @param channel_text_file CSV with information regarding MEA layout
##' @param chem_info Optional chemical information (currently unused)
##' @param array Name of array
##' @param div Age of the recording (days in vitro)
##' @return S the MEA object
##' @author Stephen Eglen
##' @examples
##' 
read_spikelist_text <- function(spike_text_file, channel_text_file, chem_info,
                                array, div=NULL) {

  channel_data = read.csv(channel_text_file, stringsAsFactors = FALSE)
  channels = channel_data$Channel

  ## The order of the channels in the channel_text_file determines the order
  ## in which the spikes are stored.
  
  spike_data = read.csv(spike_text_file)
  spikes = split(spike_data$Time, factor(spike_data$Channel, levels=channels))
  

  ## if no well information given, assume NA.
  if (!is.element("Well", names(channel_data))) {
    wells = rep("w1", length(channel_data$x))
  } else {
    wells = as.character(channel_data$Well)
  }

  xlim = range(channel_data$x)
  ylim = range(channel_data$y)
  ##pos = cbind(channel_data$x, channel_data$y)
  pos = data.frame(x=channel_data$x, y=channel_data$y, #Well=wells,
                   stringsAsFactors=FALSE)
  rownames(pos) <- channels
  layout = list(xlim=xlim,
             ylim=ylim,
             spacing = 100,
             pos = pos,
             array = array)

             
  ## what should these all be?
  ##channels <- names(spikes)
  epos <- NULL
  array <- NULL

  well <- unique(wells)                 #sje: to check - what is this normally?
  size<-rep("NA",length(wells))
  units<-rep("NA",length(wells))
  dose<-rep("NA",length(wells))

  ## Add a place-holder for the treatment information.\
  ## TODO: this could come from cheminfo?
  treatment <- rep("untreated", length(well))
  treatment[1] <- "control"             #ensure treatment has 2 levels.
  names(treatment) <- well
                  
                   
  
  beg <- end <- NULL
  ids <- NULL
  time_interval <- 1.0
  corr_breaks <- 0

  ## adapted from .r_object_read_spikes()
  s <- .construct_s(spikes,
                    ids,
                    time_interval, beg, end,
                    corr_breaks,
                    layout,
                    filename = spike_text_file)

  s$dose <- dose
  s$treatment <- treatment
  s$size <- size
  s$units <- units
  s$well <- well
  s$div<-div

  ## add the "wc" field.
  ##s <- get_num_ae(s)
  s$wc = wells
  
  ## s = list(spikes=spikes,
  ##          scount=sapply(spikes, length),
  ##          epos=epos,
  ##          file=spike_text_file,
  ##          names=channels,
  ##          array=array,
  ##          treatment=treatment,
  ##          dose=dose,
  ##          size=size,
  ##          well=well,
  ##          units=units,
  ##          layout=layout,
  ##          rec_time=rec_time
  ##          )

  s
}






