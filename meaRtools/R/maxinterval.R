## maxinterval.R --- maxinterval burst detection (from Neuroexplorer).
## Author: Stephen Eglen
## Copyright: GPL
## Fri 23 Feb 2007


mi_find_bursts <- function(spikes, mi_par) {

  ## For one spike train, find the burst using max interval method.

  no_bursts <- matrix(nrow = 0, ncol = 1) # emtpy value nrow()=length() = 0.

  par <- mi_par
  beg_isi <- par$beg_isi
  end_isi <- par$end_isi
  min_ibi <- par$min_ibi
  min_durn <- par$min_durn
  min_spikes <- par$min_spikes

  nspikes <- length(spikes)

  ## Create a temp array for the storage of the bursts.  Assume that
  ## it will not be longer than nspikes divided by 2 since we need at least two
  ## spikes to be in a burst.

  max_bursts <- floor(nspikes / 2)
  bursts <- matrix(NA, nrow = max_bursts, ncol = 3)
  colnames(bursts) <- c("beg", "end", "ibi")
  burst <- 0 # current burst number

  ## Phase 1 -- burst detection.  Here a burst is defined as starting
  ## when two consecutive spikes have an ISI less than BEG.ISI apart.
  ## The end of the burst is given when two spikes have an ISI greater
  ## than END.ISI.

  ## Find isis closer than beg_isi, and end with end isi.


  ## LAST.END is the time of the last spike in the previous burst.
  ## This is used to calculate the IBI.
  ## For the first burst, this is no previous IBI
  last_end <- NA; # for first burst, there is no IBI.

  n <- 2
  in_burst <- FALSE

  while (n <= nspikes) {

    next_isi <- spikes[n] - spikes[n - 1]
    if (in_burst) {
      if (next_isi > end_isi) {
        ## end of burst
        end <- n - 1; in_burst <- FALSE


        ibi <- spikes[beg] - last_end; last_end <- spikes[end]
        res <- c(beg, end, ibi)
        burst <- burst + 1
        if (burst > max_bursts) {
          print("too many bursts!!!")
          browser()
        }
        bursts[burst, ] <- res
      }
    } else {
      ## not yet in burst.
      if (next_isi < beg_isi) {
        ## Found the start of a new burst.
        beg <- n - 1; in_burst <- TRUE
      }
    }
    n <- n + 1
  }

  ## At the end of the burst, check if we were in a burst when the
  ## train finished.
  if (in_burst) {
    end <- nspikes
    ibi <- spikes[beg] - last_end
    res <- c(beg, end, ibi)
    burst <- burst + 1
    if (burst > max_bursts) {
      print("too many bursts!!!")
      browser()
    }
    bursts[burst, ] <- res
  }

  ## Check if any bursts were found.
  if (burst > 0) {
    ## truncate to right length, as bursts will typically be very long.
    bursts <- bursts[1:burst, , drop = FALSE]
  } else {
    ## no bursts were found, so return an empty structure.
    return(no_bursts)
  }


  ## Phase 2 -- merging of bursts.  Here we see if any pair of bursts
  ## have an IBI less than MIN.IBI; if so, we then merge the bursts.
  ## We specifically need to check when say three bursts are merged
  ## into one.


  ibis <- bursts[, "ibi"]
  merge_bursts <- which(ibis < min_ibi)

  if (any(merge_bursts)) {
    ## Merge bursts efficiently.  Work backwards through the list, and
    ## then delete the merged lines afterwards.  This works when we
    ## have say 3plus consecutive bursts that merge into one.

    for (burst in rev(merge_bursts)) {
      bursts[burst - 1, "end"] <- bursts[burst, "end"]
      bursts[burst, "end"] <- NA # not needed, but helpful.
    }
    bursts <- bursts[- merge_bursts, , drop = FALSE] # delete the unwanted info.
  }

  ## Phase 3 -- remove small bursts: less than min duration (MIN.DURN), or
  ## having too few spikes (less than MIN.SPIKES).
  ## In this phase we have the possibility of deleting all spikes.

  ## LENis the number of spikes in a burst.
  ## DURN is the duration of burst.
  len <- bursts[, "end"] - bursts[, "beg"] + 1
  durn <- spikes[bursts[, "end"]] - spikes[bursts[, "beg"]]
  bursts <- cbind(bursts, len, durn)

  rejects <- which(
    (durn < min_durn) | (len < min_spikes))

  if (any(rejects)) {
    bursts <- bursts[- rejects, , drop = FALSE]
  }

  if (nrow(bursts) == 0) {
    ## All the bursts were removed during phase 3.
    bursts <- no_bursts
  } else {
    ## Compute mean ISIS
    len <- bursts[, "end"] - bursts[, "beg"] + 1
    durn <- spikes[bursts[, "end"]] - spikes[bursts[, "beg"]]
    mean_isis <- durn / (len - 1)

    ## Recompute IBI (only needed if phase 3 deleted some cells).
    if (nrow(bursts) > 1) {
      ibi2 <- c(NA, .calc_ibi(spikes, bursts))
    } else {
      ibi2 <- NA
    }
    bursts[, "ibi"] <- ibi2

    si <- rep(1, length(mean_isis))
    bursts <- cbind(bursts, mean_isis, si)
  }

  ## End -- return burst structure.
  bursts

}
