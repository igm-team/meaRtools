### Code for working with MultiChannel Systems datafiles.
### 2019-09-24

##' Read in MCS 8x8 100um data spikes
##'
##' This uses the textreader routines for reading in MCS 8x8 arrays.
##' @title Read in MCS 8x8 100um data spikes
##' @param file File to read in
##' @param sample_rate Sample recording rate (default 25 Khz)
##' @return S object
##' @author Stephen Eglen
read_spikelist_mcs100um = function(file, sample_rate=25000) {
  f = mcs_flatten_spikes(file, sample_rate)
  posfile = system.file("extdata/textreader/mcs-8x8-100um.pos", package="meaRtools")
  s = read_spikelist_text(f, posfile, array="mcs-8x8-100um")
  s$file = file
  unlink(f)
  s
}

##' Make the MCS 8x8 100um spacing .pos file
##'
##' This creates a file in the temp directory that can then be moved.
##' @title Make the MCS 8x8 100um spacing .pos file
##' @return NULL
##' @author Stephen Eglen
make_mcs100um_pos <- function() {
  ## Create the .pos file for reading in the data.
  ## move the output file into the extdata area.
  opfile = "/tmp/mcs-8x8-100um.pos"
  r=1:8
  c=1:8
  x=rep(c, times=8)
  y=rep(r, each=8)
  channel = paste0("ch", x, y)
  separation=100 #um
  xp=x*100
  yp=(9-y)*100

  dat = data.frame(Channel=channel, x=xp, y=yp)
  write.csv(dat, opfile, row.names = FALSE)
  cat(sprintf("Position file created: %s\n", opfile))
}


.read_mcs_spikes = function(file, sample_rate=25000) {
  dt = 1.0 / sample_rate
  dat = read.table(file,skip=2,sep='\t',as.is=TRUE)
  g = grep('Spikes 1 ([0-9][0-9]) ', dat$V2, perl=TRUE)
  channels = gsub('Spikes 1 ([0-9][0-9]) ', '\\1', dat$V2[g])
  gap = diff(c(g, nrow(dat)))
  names(gap) = channels
  names(g) = channels
  last = length(gap)
  gap[last] =  gap[last] + 1
  predict_number_spikes = (gap -2)/75

  ## inhibit warnings for these conversions.
  oldwarn = options(warn=-1)
  time = as.numeric(dat[,1]) / 1e3 # convert to seconds.
  voltage = as.numeric(dat[,2])
  options(oldwarn)
  
  non_zero_spikes = names(which(predict_number_spikes> 0))
  allspikes = vector("list", length = length(non_zero_spikes))
  names(allspikes) = paste0("ch",non_zero_spikes)
  allcutouts = allspikes
  n = 1
  for (j in non_zero_spikes) {
    chan = j
    start = g[chan]
    nspikes = predict_number_spikes[chan]
    ##cat(sprintf("channel %s has %d spikes\n", chan, nspikes))
    mat = matrix(NA, nrow=nspikes, ncol=75)
    spiketime = rep(NA, nspikes)
    for (i in 1:nspikes) {
      mat[i,] = voltage[(start+2):(start+2+75-1)]
      spiketime[i] = time[start+1+25]
      start = start+75
    }
    allspikes[[n]] = spiketime
    allcutouts[[n]] = mat
    n = n + 1
  }

  allspikes

}

  


######################################################################
mcs_flatten_spikes = function(file1, sample_rate) {
  ## Read in the spikes data from mcs and then
  ## put into a two column format (c, t)
  ## where c is the channel name and t is the time of the spike.
  ## Returns the name of a new (temporary) file containing the data.
  ## This is to be deleted after use.
  spikes = .read_mcs_spikes(file1, sample_rate)
  num_spikes = sapply(spikes, length)
  flatten_spikes = unlist(spikes)
  spike_ids = rep(names(spikes), num_spikes)
  times = data.frame(Channel=spike_ids,Time=flatten_spikes)

  ## Put spike channel asnd time into temporary file.
  tempfile = tempfile()
  write.csv(times, tempfile, row.names=FALSE)

  tempfile
}

.mcs100um_8x8_platelayout = list(n_well = 1, #number of wells 
                            wells = c("w1"), #names of those wells.
                            n_well_r = 1, # number of wells / row
                            n_well_c = 1, # number of wells / col
                            layout = c(1, 1), # layout when plotting
                            n_elec_r = 8,
                            n_elec_c = 8,
                            xlim = c(50, 850), # xlimits for plotting
                            ylim = c(50, 850), # ylimits for plotting
                            spacing = 100,  # distance (um) separating electrodes
                            corr_breaks = 0 # vector of correlation distances
                        )


## The plateinformation is added to the system by the .onLoad() function.
##   meaRtools:::.plot_mealayout(s$layout, use_names=TRUE, cex=1)
