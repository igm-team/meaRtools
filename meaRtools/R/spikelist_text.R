## Functions for reading in text in a generic format.

read_spikelist_text <- function(spike_text_file, channel_text_file, chem_info) {

  channel_data = read.csv(channel_text_file)
  channels = channel_data$Channel

  ## The order of the channels in the channel_text_file determines the order
  ## in which the spikes are stored.
  
  spike_data = read.csv(spike_text_file)
  spikes = split(spike_data$Time, factor(spike_data$Channel, levels=channels))
  


  ## if no well information given, assume NA.
  if (!is.element("Well", names(channel_data))) {
    wells = NA
    print(wells)
  }

  xlim = range(channel_data$x)
  ylim = range(channel_data$y)
  pos = cbind(channel_data$x, channel_data$y)
  rownames(pos) <- channels

  layout = list(xlim=xlim,
             ylim=ylim,
             spacing = 100,
             pos = pos)

             
  ## what should these all be?
  ##channels <- names(spikes)
  epos <- NULL
  array <- NULL
  treatment <- NULL
  dose <- NULL
  size <- NULL
  well <- NULL
  units <- NULL

  s = list(spikes=spikes,
           scount=sapply(spikes, length),
           epos=epos,
           file=spike_text_file,
           names=channels,
           array=array,
           treatment=treatment,
           dose=dose,
           size=size,
           well=well,
           units=units,
           layout=layout
           )

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




