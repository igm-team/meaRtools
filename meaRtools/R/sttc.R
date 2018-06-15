##' Compute STTC for a pair of spike trains
##'
##' The Spike Time Tiling correlation (STTC) is computed for a pair
##' of spike trains.  The method is defined in Cutts and Eglen (2014).
##' We assume that the spike trains are ordered, smallest-time first.
##' 
##' @title Compute STTC for a pair of spike trains
##' @param a first spike train
##' @param b second spike train
##' @param dt bin size in seconds
##' @param rec_time 2-element vector: start and end time 
##' @return STTC a scalar bounded between -1 and +1.
##' @author Stephen J Eglen
##' @examples
##' a = c(1, 2, 3, 4, 5)
##' b = a+0.01
##' c = a+0.5
##' sttc(a, b)==1
##' sttc(a, c)==0
sttc <- function(a, b, dt = 0.05, rec_time = NULL) {
  if (is.null(rec_time)) {
    rec_time <- range(c(a, b))
  }
  run_TMcpp(dt, rec_time[1], rec_time[2], a, b)
}


##' Compute STTC profile for a pair of spike trains
##'
##' We extend the STTC to a profile (or correlogram) by shifting one
##' spike train by amount tau, where tau varies in [-tau_max, +tau_max]
##' in steps of tau_step.
##' 
##' @title Compute 
##' @param a spike train 1
##' @param b spike train 2
##' @param dt time window for STTC
##' @param tau_max maximum time shift
##' @param tau_step step size in tau
##' @param beg start of recording. When NULL use the minimum spike time from
##' the two trains.
##' @param end end of recording.  When NULL use the maximum spike time from
##' the two trains.
##' @return List containing the STTC profile.
##' @author Stephen Eglen
##' @examples
##' t1 <- -cumsum(log(runif(1000)) / 2)
##' t2 <- -cumsum(log(runif(1000)) / 2)
##' corr <- sttcp(t1, t2)
##' plot(corr, main="cross correlation")
##' autocorr <- sttcp(t1, t1)
##' plot(autocorr, main="auto correlation")
sttcp <- function(a, b, dt = 0.05, tau_max = 5, tau_step = 0.1,
                  beg = NULL, end = NULL) {
  spikes <- c(a, b)
  nspikes <- c(length(a), length(b))
  first_spike <- cumsum(c(1, length(a)))
  if (is.null(beg))
    beg <- min(spikes)
  if (is.null(end))
    end <- max(spikes)

  y = sttcp_ab(a, b, beg, end, dt, tau_step, tau_max)
  taus = seq(from=-tau_max, to=tau_max, by=tau_step)
  object = list(x=taus, y=y)
  class(object) <- "sttcp"
  object
}

plot.sttcp <- function(x, ...) {
  plot(x$x, x$y, xlab="tau (s)", ylab='STTC', type='l', ...)
}

##' Compute the mean STTC averaged across all pairwise electrodes in well
##'
##' For each pair of electrodes, we calculate the STTC.  We then take
##' the mean of these pairs, excluding autocorrelations.  If a well has
##' one (or no) electrodes, the value returned for that well is NULL.
##'
##' Warning: taking the mean over a well is useful only if you do not
##' suspect distance-dependent correlations in your firing.  (For
##' activity like retinal waves, we find that correlations are
##' strongly dependent on the distance separating electrodes.)
##' 
##' 
##' @title Compute the mean STTC averaged across all pairwise electrodes in well
##' @param s structure storing the well information
##' @param dt Time window for STTC (default = 0.05 seconds)
##' @param beg Start time in seconds (defaults to start of recording)
##' @param end End time in seconds (defaults to end of recording)
##' @return A vector giving the mean of all pairwise STTCs on each well.
##' @author Stephen Eglen
compute_mean_sttc_by_well <- function(s, dt=0.05, beg=NULL, end=NULL) {
  if (is.null(beg))
    beg <- s$rec_time[1]
  if (is.null(end))
    end <- s$rec_time[2]

  plateinfo <- .plateinfo(s$layout$array)
  wells <- plateinfo$wells
  names(wells) <- wells # keep the names valid.
  wells_layout <- plateinfo$layout

  ## For each well, we extract the electrodes on that well, and as long
  ## as there are 2+ electrodes, we compute all distinct STTCs.
  sttc_all <- lapply(wells, function(well) {
    indexes <- .names_to_indexes(names(s$spikes), well, allow_na = TRUE)
    allspikes = s$spikes[indexes]
    if (length(allspikes) >= 2) {        #need at least two spike trains in well
      sttcs_mat = sttc_allspikes1(allspikes, dt, beg, end)
      v = sttcs_mat[upper.tri(sttcs_mat)]   #excludes diagonal
      mean(v)
    } else {
      NULL                              #no recordings
    }
  })

  ## Do we want to filter out the empty wells?
  return(sttc_all)
}
  

##' Compute the STTC across all pairwise electrodes in well
##'
##' For each pair of electrodes (excluding autocorrelations), we calculate the STTC. 
##' If a well has one (or no) electrodes, no STTCs are calculated for that well.
##'
##' @title Compute the STTC across all pairwise electrodes in well
##' @param s structure storing the well information
##' @param dt Time window for STTC (default = 0.05 seconds)
##' @param beg Start time in seconds (defaults to start of recording)
##' @param end End time in seconds (defaults to end of recording)
##' @return A data frame giving all pairwise STTCs (and distance separating electrodes) on each well.
##' @author Stephen Eglen
compute_sttc_by_well <- function(s, dt=0.05, beg=NULL, end=NULL) {
  if (is.null(beg))
    beg <- s$rec_time[1]
  if (is.null(end))
    end <- s$rec_time[2]

  ## First, group electrodes per well.
  electrodes_per_well = lapply( s$well, function(w) which(s$layout$pos$Well == w))
  names(electrodes_per_well) = s$well
  nelectrodes_per_well = sapply(electrodes_per_well, length)

  ## if nelectrodes_per_well for a well is 0 or 1, the following returns zero for that well.
  nelectrode_pairs_per_well = choose(nelectrodes_per_well, 2)
  total_pairs = sum(nelectrode_pairs_per_well)

  empty_string = rep("", total_pairs)
  res = data.frame(Channela=empty_string, Channelb=empty_string, Well=empty_string,
                   Distance=rep(NA,total_pairs),
                   STTC=rep(NA, total_pairs), stringsAsFactors=FALSE)

  line = 0
  ## Return a data frame with sttc for each electrode pair in each well.
  for (well in s$well) {
    ## get all electrodes for a Well
    electrodes = electrodes_per_well[[well]]
    allspikes = s$spikes[electrodes]
    n = length(allspikes)
    if (n >= 2) {        #need at least two spike trains in well
      sttcs_mat = sttc_allspikes1(allspikes, dt, beg, end)
      ## a iterates over rows of matrix, b over columns
      for (a in 1:(n-1)) {
        a_x = s$layout$pos$x[a]
        a_y = s$layout$pos$y[a]
        for (b in (a+1):n) {
          line = line + 1
          distance = sqrt( (s$layout$pos$x[b] - a_x)^2 +
                           (s$layout$pos$y[b] - a_y)^2 )
          res[line, 1] = s$names[a]
          res[line, 2] = s$names[b]
          res[line, 3] = well
          res[line, 4] = distance
          res[line, 5] = sttcs_mat[a, b]
        }
      }
    }
  }
  stopifnot(line == total_pairs)        #check that we computed all pairs.
  res
}
