## networkspikes.R --- identify and analsyse network spikes
## Author: Stephen J Eglen
## Copyright: GPL
## Sun 28 Jan 2007
## Taking ideas from Eytan & Marom J Neurosci (2006).



##' Compute network spikes
##'
##' Compute the network spikes in an MEA recording, by averaging over all the
##' electrodes in the array.
##'
##' To see the mean network spikes after they have computed, just look at the
##' mean object.
##'
##' If you wish to see the individual network spikes, try .show_ns(ns, ...)
##' where the remaining args are passed to the plot function.
##'
##' @aliases .compute_ns
##' @param s MEA data structure
##' @param ns_t Bin width (in seconds) for counting spikes.
##' @param ns_n Threshold number of active electrodes required to make network
##' spike.
##' @param sur How many bins either side of peak to retain when computing the
##' mean network spike (default 100 bins either side).
##' @param whichcells An optional vector of electrode names.
##' @param plot Set to TRUE to plot network spikes.
##' @param ns A network spike data structure, returned by
##' \code{\link{.compute_ns}}
##' @param ... Other plot arguments to pass to \code{\link{.show_ns}}
##' @return A list with the following elements: \item{counts}{vector giving the
##' number of active electrodes in each bin; this can be very long!}
##' \item{ns_n}{The value of ns_n used.} \item{ns_t}{the value of ns_t used.}
##' \item{mean}{The profile of the mean network spike (this is a time series
##' object)} \item{measures}{If N network spikes were found, this is a matrix
##' with N rows, one per network spike.} \item{brief}{A short vector
##' summarizing the network spikes.  n: number of spikes; peak_m, peak_sd: mean
##' and sd of the peak height; durn_m, durn_sd: mean and sd of the duration of
##' the network spike.}
##' @author Stephen Eglen
##' @references Eytan and Marom (2006) J Neuroscience.
##' @keywords Network spikes, MEA analysis
##'
.compute_ns <- function(s, ns_t, ns_n, sur=100, whichcells=NULL,
  plot=FALSE) {

  indexes <- .names_to_indexes(names(s$spikes), whichcells, allow_na = TRUE)
  if (length(indexes) == 0) {
    ## No electrodes were found matching "whichcells"
    ## so just return brief information summarising no network activity.
    ns <- list()
    ns$brief <- c(n = NA, peak_m = NA, peak_sd = NA, durn_m = NA, durn_sd = NA)
  } else {
    counts <- .spikes_to_count2(s$spikes[indexes], time_interval = ns_t)
    p <- .find_peaks(counts, ns_n)
    ns <- list(counts = counts, ns_n = ns_n, ns_t = ns_t)
    class(ns) <- "ns"
    m <- .mean_ns(ns, p, plot = plot, nrow = 4,
                  ncol = 4, ask = FALSE, sur = sur)
    if (is.null(m)) {
      ## No network spikes found.
      ns$brief <- c(n = 0, peak_m = NA, peak_sd = NA, durn_m = NA, durn_sd = NA)
    } else {
      ns$mean <- m$ns_mean; ns$measures <- m$measures
      peak_val <- ns$measures[, "peak_val"]
      durn <- ns$measures[, "durn"]
      ns$brief <- c(n = nrow(ns$measures),
        peak_m = mean(peak_val), peak_sd = sd(peak_val),
        durn_m = mean(durn, na.rm = TRUE), durn_sd = sd(durn, na.rm = TRUE))

    }
  }

  ns
}

.spikes_to_count2 <- function(spikes,
  time_interval=1, # time bin of 1sec.
  beg=floor(min(unlist(spikes))),
  end=ceiling(max(unlist(spikes)))
  ) {
  ## Convert the spikes for each cell into a firing rate (in Hz)
  ## We count the number of spikes within time bins of duration
  ## time_interval (measured in seconds).
  ##
  ## Currently cannot specify BEG or END as less than the
  ## range of spike times else you get an error from hist().  The
  ## default anyway is to do all the spikes within a data file.
  ##
  ## C version, which should replace spikes_to_count
  ## Returns a time series object.

  nbins <- ceiling((end - beg) / time_interval)

  nspikes <- sapply(spikes, length) # already computed elsewhere!

  nspikes <- sapply(spikes, length)     #already computed elsewhere!

  ## sjecpp
  counts = count_ns(spikes, beg, end, time_interval, nbins)
  res <- ts(data=counts, start=beg, deltat=time_interval)

  res
}

plot_network_spikes <- function(ns, ...) {
  ## Plot function for "ns" class.
  plot(ns$counts, ...)
  abline(h = ns$ns_n, col = "red")
  peak_times <- ns$measures[, "time"]
  peak_val <- ns$measures[, "peak_val"]
  points(peak_times, peak_val, col = "blue", pch = 19)

}

.summary_ns <- function(ns) {
  ## Summary function for "ns" class.
  n <- ns$brief["n"]
  cat(sprintf("%d network spikes\n", n))
  peak_m <- ns$brief["peak_m"]
  peak_sd <- ns$brief["peak_sd"]


  durn_m <- ns$brief["durn_m"]
  durn_sd <- ns$brief["durn_sd"]
  cat(sprintf("recruitment %.2f +/- %.2f\n", peak_m, peak_sd))
  cat(sprintf("FWHM %.3f +/- %.3f (s)\n", durn_m, durn_sd))
}

.mean_ns <- function(ns, p, sur=100,
  plot=TRUE, nrow=8, ncol=8, ask=FALSE) {
  ## Compute the mean network spikes, and optionally show the
  ## individual network spikes.

  ## This code does not check to worry if there is a spike right at either
  ## end of the recording.  naughty!

  if (is.null(p)) {
    if (is.null(ns$measures)) {
      # No ns found in this well
      return(NULL)
    } else {
      ## use info previously stored in measures.
      p <- ns$measures
    }
  }


  if (plot) {
    old_par <- par(mfrow = c(nrow, ncol), mar = c(2.5, 1, 1, 1), ask = ask)
  }
  ave <- rep(0, (2 * sur) + 1)
  npts <- length(ns$counts)
  times <- time(ns$counts)
  measures <- matrix(NA, nrow = nrow(p), ncol = 4)
  colnames(measures) <- c("time", "index", "peak_val", "durn")
  n_ns <- 0 # Number of valid network spikes found
  for (i in 1:nrow(p)) {
    peak_i <- p[i, "index"]
    lo <- (peak_i - sur)
    hi <- peak_i + sur

    ## Check that enough data can be found:
    if ((lo > 0) && (hi < npts)) {
      n_ns <- n_ns + 1

      dat <- ns$counts[lo:hi]
      peak_val <- dat[sur + 1]
      measures[n_ns, "time"] <- times[peak_i]
      measures[n_ns, "index"] <- peak_i
      measures[n_ns, "peak_val"] <- peak_val


      if (plot) {
        plot(dat, xaxt = "n", yaxt = "n", ylim = c(0, 60),
          bty = "n", type = "l", xlab = "", ylab = "")
        max_time <- ns$ns_t * sur
        axis(1, at = c(0, 1, 2) * sur,
          labels = c(- max_time, 0, max_time))

      }

      hm <- .find_halfmax(dat, peak_n = sur + 1, frac = 0.5, plot = plot)
      measures[n_ns, "durn"] <- hm$durn * ns$ns_t
      if (plot) {
        text <- sprintf("%d durn %.3f",
          round(peak_val), measures[n_ns, "durn"])
        legend("topleft", text, bty = "n")
      }
      ave <- ave + dat


    }
  }

  if (n_ns < nrow(p)) {
    ## Some peaks could not be averaged, since they were at either
    ## beg/end of the recording.
    ## So, in this case, truncate the matrix of results to correct
    ## number of rows.
    measures <- measures[1:n_ns, , drop = FALSE]
  }

  ## now show the average
  if (n_ns > 0) {
    ave <- ave / n_ns
    if (plot) {
      plot(ave, xaxt = "n", yaxt = "n", bty = "n", type = "l",
           xlab = "", ylab = "")
      legend("topleft", paste("m", round(max(ave))), bty = "n")
      .find_halfmax(ave)
    }

    if (plot) {
      stripchart(measures[, "durn"], ylab = "durn (s)", method = "jitter",
        vert = TRUE, pch = 19,
        main = paste("FWHM durn", round(mean(measures[, "durn"]), 3)))
    }

    if (plot) {
      par(old_par)
    }

  }


  ns_mean <- ts(ave, start = (- sur * ns$ns_t), deltat = ns$ns_t)

  list(measures = measures, ns_mean = ns_mean)
}


.find_peaks <- function(trace, ns_n) {

  ## Peaks are defined as being all elements between two zero entries
  ## (one at start, one at end) in the time series.  An alternate
  ## definiton might be to require some number N of consecutive zero
  ## entries to surround a peak.

  max_peaks <- 200000

  npts <- length(trace)

  peaks <- matrix(NA, nrow = max_peaks, ncol = 2)
  colnames(peaks) <- c("index", "peak_val")
  n <- 0

  inside <- FALSE

  for (i in 1:npts) {

    cur <- trace[i]

    if (inside) {
      ## currently in a peak.
      if (cur == 0) {
        ## no longer inside a peak, save results if peak was tall enough.
        inside <- FALSE

        if (peak > ns_n) {
          n <- n + 1
          if (n > max_peaks) {
            ## oh oh, need more room.
            browser()
          } else {
            peaks[n, ] <- c(peak_t, peak)
          }
        }

      } else {
        ## still in a peak
        if (cur > peak) {
          peak <- cur
          peak_t <- i
        }
      }
    } else {
      ## currently outside a peak.
      if (cur > 0) {
        inside <- TRUE
        peak <- cur
        peak_t <- i
      }
    }
  }

  ## tidy up result at end.

  if (n > 0) {
    peaks <- peaks[1:n, , drop = FALSE]
  } else {
    ## no peaks found.
    peaks <- NULL
  }
}

.find_halfmax <- function(y, peak_n=NULL, plot=TRUE, frac=0.5) {

  ## Given a peak somwhere with Y, find the FWHM.
  ##
  ## If peak_n is not null, it will be location of the peak -- this is helpful
  ## when there are multiple peaks within one window, and we want to find
  ## the FWHM of the non-major peak.
  ## By default, frac = 0.5, to find the half max.  Change this to some other
  ## value, e.g. 10% to find 10% onset and offset.
  ##
  ##
  ## This may fail for a few reasons, e.g. not finding half-max values within
  ## the range, running out of data...
  ## all of which should be counted for!

  n <- length(y)

  if (is.null(peak_n))
    peak_n <- which.max(y)

  peak_val <- y[peak_n]

  half_max <- peak_val * frac

  ## Break the data into three segments:

  ## llllllllllllllllllPrrrrrrrrrrrrrrrrr
  ## P is the peak; examine curve to the left (lll) and to the right (rrr) to
  ## find when the peak has decayed to half max.

  left_y <- y[1:(peak_n - 1)]
  right_y <- y[(peak_n + 1):n]

  ## When finding the halfmax value in the left and right side, we
  ## have to check that first all of the halfmax value can be found.
  ## e.g. if the peak value is 50 and all values to the left are 45,
  ## there is no value to the left which is under 25, and so the half
  ## max value cannot be computed.


  ## Assume the half max point can be found, we interpolate to find
  ## the point, see below.

  underhalf_l <- which(left_y < half_max)
  if (any(underhalf_l)) {
    xl1 <- underhalf_l[length(underhalf_l)] # get last point under halfmax.
    xl2 <- xl1 + 1

    yl1 <- y[xl1]
    yl2 <- y[xl2]
    dy <- half_max - yl1


    ## see picture.
    ## below, (xl2 - xl1) should equal 1.
    dx <- (dy * (xl2 - xl1)) / (yl2 - yl1)

    xl_half <- xl1 + dx
  } else {
    xl_half <- NA # could not find half-max to left.
  }

  ## Now work on right of curve.  find first point at which y falls below
  ## half max value.
  underhalf_r <- which(right_y < half_max)
  if (any(underhalf_r)) {
    xr2 <- underhalf_r[1] + peak_n
    xr1 <- xr2 - 1

    yr1 <- y[xr1]
    yr2 <- y[xr2]
    dy <- half_max - yr2

    dx <- dy * (xr1 - xr2) / (yr1 - yr2)

    xr_half <- xr2 + dx
  } else {
    xr_half <- NA
  }


  if (plot) {
    abline(h = peak_val * frac, col = "red")
    if (! any(is.na(c(xl_half, xr_half)))) {
      ## check first that both half-maxes are valid.
      segments(xl_half, half_max, xr_half, half_max, col = "blue")
    }
  }

  list(xl = xl_half, xr = xr_half, durn = xr_half - xl_half)
}
