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
##' @return S the MEA object
##' @author Stephen Eglen
##' @examples
##' 
read_spikelist_text <- function(spike_text_file, channel_text_file, chem_info,
                                array) {

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
  pos = data.frame(x=channel_data$x, y=channel_data$y, Well=wells,
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

plot.corr.index <- function(s, identify=FALSE,
                            main=NULL,
                            show.method=TRUE,
                            dot.col='red',
                            show.fit=TRUE, show.ci=FALSE,
                            show.pts=NULL,
                            ylabel="correlation",
                            xlabel=expression(paste("intercell distance (",
                                mu, "m)")),
                            ...) {
  ## SJE: update: s$NCells -> length(s$spikes)
  ## Plot the correlation indices as a function of distance.

  ## Use 'log=y' as one of the extra args if the y-axis should be
  ## drawn on a log scale.
  ## DOT.COL: colour of each dot.
  ## If SHOW.FIT is true, draw the expoential fit.
  ## If SHOW.CI is true, draw the confidence intervals estimated every
  ## 100 um or so.
  ## SHOW.PTS: if TRUE, show individual CI values.  If NULL, the value
  ## is assumed TRUE iff number of cells recorded is less than 100.
  

  nchannels = length(s$spikes)
  if (s$corr$valid) {
    dists = s$corr$corr.id[,"dist"]
    corrs = s$corr$corr.id[,"corr"]
  }

  if (!(s$corr$valid) || all(is.na(corrs))) {
    ## no correlation data to show, so just up empty plot.
    plot(NA, xlim=c(1,10), ylim=c(1,10),
         xlab=xlabel, ylab=ylabel,
         main=paste(basenamepy(s$file)$base, ': no valid corrs'))
  } else {
    ## Some of these corrs may be NA if the firing rate is low, but they
    ## should be safely ignored in the plot.
  
    if (is.null(main)) {
      main = paste(basename(s$file), "dt:", s$corr$dt)
    }
  
    if (is.null(show.pts))
      show.pts <- nchannels < 100

    if (show.pts) {
      
      plot.default(dists, corrs, xlab=xlabel, ##log=log,
                   ylab=ylabel, bty="n",
                   main=main, col=dot.col,
                   ...)
    } else {
      ## set the ylim to a sensible default.
      upper.pts <- s$corr$corr.id.means[,"mean"] + s$corr$corr.id.means[,"sd"]
      ylim <- c(0.001, max(upper.pts, na.rm=TRUE)) #sd could be NA
      
      plot.default(dists, corrs, xlab=xlabel, type='n',
                   ylab=ylabel, bty="n",
                   main=main, ylim=ylim,
                   ...)
      show.ci <- TRUE                   #better show something.
    }


    if (show.ci) 
      plotCI(s$corr$corr.id.means[,"mid"], s$corr$corr.id.means[,"mean"],
             s$corr$corr.id.means[,"sd"],
             xlab=xlabel, ylab=ylabel,
             pch=19, add=TRUE)
    if (show.fit) 
      corr.do.fit(s$corr$corr.id,plot=TRUE)

    if (!is.null(s$corr$method) && show.method) {
      title(sub = s$corr$method)
    }
  }
}




