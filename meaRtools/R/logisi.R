## Implement logisi method for burst detection.
## Author: Zhengzheng Zhang
## Copyright: GPL.

.logisi_find_burst <- function(spikes, debug=FALSE) {

  ## For one spike train, find the burst using log isi method.

  no_bursts <- NA; # value to return if no bursts found.

  logisi_par <- list(min_ibi = 0.800, min_durn = 0.05, min_spikes = 6,
    isi_low = 0.02)
  par <- logisi_par
  min_ibi <- par$min_ibi
  min_durn <- par$min_durn
  min_spikes <- par$min_spikes
#   isi low should be par isi low

  nspikes <- length(spikes)

  ## Create a temp array for the storage of the bursts.  Assume that
  ## it will not be longer than nspikes by 2 since we need at least two
  ## spikes to be in a burst.

  max_bursts <- floor(nspikes / 2)
  bursts <- matrix(NA, nrow = max_bursts, ncol = 3)
  colnames(bursts) <- c("beg", "end", "ibi")
  burst <- 0 # current burst number

  ## Phase 1 -- burst detection. Each interspike interval of the data
  ## is compared with the threshold THRE. If the interval is greater
  ## than the threshold value, it can not be part of a burst; if the
  ## interval is smaller or equal to the threhold, the interval may be
  ## part of a burst.



  ## LAST.END is the time of the last spike in the previous burst.
  ## This is used to calculate the IBI.
  ## For the first burst, this is no previous IBI
  last_end <- NA; # for first burst, there is no IBI.

  n <- 2
  in_burst <- FALSE

  while (n < nspikes) {

    next_isi <- spikes[n] - spikes[n - 1]
    if (in_burst) {
      if (next_isi > isi.low) {
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
      if (next_isi <= isi.low) {
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

  if (debug) {
    print("End of phase1\n")
    print(bursts)
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

  if (debug) {
    print("End of phase 2\n")
    print(bursts)
  }


  ## Phase 3 -- remove small bursts: less than min duration (MIN.DURN), or
  ## having too few spikes (less than MIN.SPIKES).
  ## In this phase we have the possibility of deleting all spikes.

  ## LEN is the number of spikes in a burst.
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


## Peak finding algorithm; taken from R-help mailing list.
## Author: Brian Ripley.
.locpeaks <- function(series, span = 3) {
  z <- embed(series, span)
  s <- span %/% 2
  v <- max.col(z) == 1 + s
  result <- c(rep(FALSE, s), v)
  result <- result[1:(length(result) - s)]
  which(result)
}

.logisi_compute <- function(s, min_nspikes = 10,
  breaks_max = 100,
  channel = ncells + 1,
  span = 1, span_max = 50, rat = 0.08, plot = FALSE) {

  ## min nspikes is the minimum number of spikes in a channel.
  ## brmax is the breaks_max
  ## channel.
  ##
  ## Given the spike data structure S,
  ## compute the log ISI transform and return useful information.

  ## This function should be expanded to find the peaks in the
  ## histogram either of each channel or of the grand average.

  h <- list() # hist objects
  total_isi <- NULL
  ncells <- s$NCells
  if (plot) {
    par(mfrow = c(8, 8), mar = c(3, 3, 1, 1), ask = FALSE, oma =
          c(0, 1.5, 0, 0))
  }

  for (i in 1:ncells) {
    if (s$nspikes[i] >= min_nspikes) {
      ## Channel has "enough" spikes for method.
      isi <- diff(s$spikes[[i]])
      total_isi <- c(total_isi, isi)
      brk <- sqrt(s$nspikes[i])
      if (brk > breaks_max) {
        brk <- breaks_max
      }
      if (plot){
        title <- sprintf("%d # %d", i, s$nspikes[i])
        h[[i]] <- hist(log(isi), br = brk, main = title, xlab = "logisi")
        abline(h = mean(h[[i]]$counts), col = "blue")
      } else {
        h[[i]] <- hist(log(isi), br = brk, plot = FALSE)
      }
    } else {
      ## insufficient spikes to compute ISI histogram.
      if (plot) {
        title <- sprintf("%d # %d", i, s$nspikes[i])
        plot(1, type = "n", main = title)
      }
    }
  }

  ## The log histogram for the grand average.
  brk <- sqrt(length(total_isi))
  if (brk > breaks_max) {
    brk <- breaks_max
  }
  file <- s$file
  if (plot){
    last_h <- hist(log(total_isi), br = brk, main = "All", xlab = "logisi")
    abline(h = mean(last_h$counts), col = "red")
    mtext(file, side = 2, outer = TRUE)
  } else {
    last_h <- hist(log(total_isi), br = brk, plot = FALSE)
  }
  h[[ncells + 1]] <- last_h
  counts <- h[[channel]]$counts
  breaks <- h[[channel]]$breaks

  if (plot) {
    ## Find the peaks in the histogram
    ## either of each channel or of the grand average.
    par(mfrow = c(1, 1))
    plot(counts, type = "l", xlab = "Logisi (ms)", ylab = "Frequency",
         xaxt = "n")
    axis(1, 0:length(counts), format(exp(breaks), sci = TRUE))
  }
  peaks <- .locpeaks(counts, span)

  ## Return some dummy values; these might be times of thresholds.
  lmax <- max(counts)
  if (length(peaks) == 0){
    peak_max <- - Inf
  } else {
    peak_max <- max(counts[peaks])
  }

  if (span_max >= length(counts)) {
    span_max <- length(counts) - 1
  }

  ## Find the no. of peaks no more than 6, and
  ## the golobal max is one of peaks.
  while (length(peaks) > 6 || lmax != peak_max) {
    span <- span + 1
    peaks <- .locpeaks(counts, span)
    if (length(peaks) == 0){
      peak_max <- - Inf
    } else {
      peak_max <- max(counts[peaks])
    }
    if (span > span_max) {
      peaks <- 0
      break
    }
  }


  if (length(peaks) != 1 || (length(peaks) == 1 && peaks != 0)){
    if (plot) {
      points(peaks, counts[peaks], pch = 19, col = "blue")
    }
  } else {
    browser()
  }

  ## Find the local minimums between two successive peaks, and report the
  ## lowest. If the peak finding algorithm gives some unlikely peaks
  ## between them, then the peaks will be filtered out.
  ## rat is 0.08        # a threhold for filtering unreasonable peaks

  pos <- - 1 # flag
  len <- length(peaks) # initial length
  j <- 1
  mini <- NULL
  r <- NULL

  while (pos == - 1 || j < len) {
    len <- length(peaks)
    if (len >= 2){
      loc_min <- min(counts[peaks[j]:peaks[j + 1]])
      temp <- c(peaks[j]:peaks[j + 1])
      pos <- temp[counts[peaks[j]:peaks[j + 1]] == loc_min]
      pos <- pos[length(pos)] # last local min
      pair <- c(counts[peaks[j]], counts[peaks[j + 1]])
      smallest <- c(j, j + 1)[which.min(pair)]
      cdiff <- counts[peaks[smallest]] - counts[pos]
      ## If the second peaks occurs after the first in the next 3
      ## breaks, then remove the smallest peak.
      if (diff(peaks[j:(j + 1)]) <= 3){
        peaks <- peaks[- smallest]
        pos <- - 1
        j <- 1
      } else {
        if (cdiff == 0){
          peaks <- peaks[- smallest]
          pos <- - 1
          j <- 1
        } else {
          ## define a ratio
          ratio <- cdiff / (max(counts[peaks]) - counts[pos])
          ## If the ratio is less than rat, remove the smallest peak.
          if (ratio < rat){
            peaks <- peaks[- smallest]
            pos <- - 1
            j <- 1
          } else {
            if (ratio < 0 || ratio > 1) {
              browser()
            }
            mini <- c(mini, pos)
            r <- c(r, ratio)
            j <- j + 1
          }
        }
      }

    } else {
      lowest <- - 2
      break
    }
  }
  if (length(mini) != 0){
    m <- min(counts[mini])
    lowest <- mini[counts[mini] == m][1] # choose the first
  } else {
    lowest <- - 2
  }

  if (lowest != - 2){
    if (plot) {
      points(lowest, counts[lowest], pch = 19, col = "red")
    }
    a1 <- h[[channel]]$breaks[lowest]
    a2 <- h[[channel]]$breaks[lowest + 1]
    av_a <- (a1 + a2) / 2
    loc_min <- exp(av_a)
  } else {
    loc_min <- NA
  }



  b1 <- h[[channel]]$breaks[peaks]
  b2 <- h[[channel]]$breaks[peaks + 1]
  av_b <- (b1 + b2) / 2
  isi_peaks <- exp(av_b) # time in seconds
  res <- list(max1 = isi_peaks[1], max2 = isi_peaks[2], max3 = isi_peaks[3])

  return(list(cmax = res, locmin = loc_min))

}
