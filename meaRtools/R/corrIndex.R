## Compute the correlation index, as defined by Wong et al (1993).
## Author: Stephen J Eglen
## Copyright: GPL
## Sun 04 Mar 2007

.corr_index <- function(s, distance_breaks,
  dt = getOption("meaRtools_corr_dt", default = 0.05),
  min_rate = 0,
  corr_method = getOption("meaRtools_corr_method", default = "CI")) {
  ## Make a correlation index object.
  ## MIN.RATE: if greater than zero, we analyse only spike trains whose
  ## firing rate is greater than this minimal rate.
  ## corr_method is which method to use.
  dists <- .make_distances(s$layout$pos)
  dists_bins <- .bin_distances(dists, distance_breaks)

  spikes <- s$spikes
  if (length(spikes) > 1) {
    corr_indexes <- NULL
    if (corr_method == "CI") {
      corr_indexes <- .make_corr_indexes2(spikes, dt, min_rate)
    }
    if (corr_method == "Tiling") {
      corr_indexes <- .tiling_allpairwise(s, dt)
    }
    if (is.null(corr_method)) {
      stop("Corr index not calculated")
    }


    corr_id <- cbind(dist = .my_upper(dists), corr = .my_upper(corr_indexes),
    dist_bin <- .my_upper(dists_bins))

    dist_mids <- diff(distance_breaks) / 2 +
    distance_breaks[- (length(distance_breaks))]
    corr_id_means <- .corr_get_means(corr_id, dist_mids)
  } else {
    corr_indexes <- NA
    corr_id <- NA
    corr_id_means <- NA
  }

  ## distance_breaks_strings used only by Mutual Information Code?
  distance_breaks_strings <-
  levels(cut(0, distance_breaks, right = FALSE, include.lowest = TRUE))

  res <- list(
    dt = dt,
    corr_id = corr_id,
    corr_id_means = corr_id_means,
    distance_breaks = distance_breaks,
    distance.mids = dist_mids,
    distance_breaks_strings = distance_breaks_strings,
    method = corr_method)

  res
}


.make_distances <- function(posns, rm_lower=TRUE) {
  ## POSNS should be a (N,2) array.  Returns a NxN upper triangular
  ## array of the distances between all pairs of cells.

  x <- posns[, 1]; y <- posns[, 2]
  d <- round(sqrt(outer(x, x, "-") ^ 2 + outer(y, y, "-") ^ 2))
  if (rm_lower)
    d[lower.tri(d)] <- 0
  d
}

.bin_distances <- function(dists, breaks) {
  ## DISTS is a upper NxN array.
  ## breaks is a vector of breakpoints.
  ## Return an array of the same size where each distance value is
  ## given a corresponding bin number.

  distances <- .my_upper(dists)
  ## These breaks are hardcoded.

  ## data is 0, 100, 700, 900, 400

  ## Make each bin low, high with exception that highest bin is
  ## low,high. Labels is false so that we just return numeric count
  ## of bin, rather than a factor.

  bins <- cut(distances, breaks, right = FALSE,
    include.lowest = TRUE, labels = FALSE)
  invalid <- is.na(bins)
  if (any(invalid))
  stop(paste("distances not binned:",
    paste(distances[which(invalid)], collapse = " ")))
  n <- dim(dists)[1]
  res <- matrix(0, nrow = n, ncol = n)
  res[which(upper.tri(res))] <- bins

  res
}

.make_corr_indexes2 <- function(spikes, dt, min_rate=0) {
  ## New version using the C routine for corr indexing.
  ## Return the correlation index values for each pair of spikes.
  ## The matrix returned is upper triangular.
  ## SPIKES should be a list of length N, N is the number of electrodes.
  ## "dt" is the maximum time for seeing whether two spikes are coincident.
  ## This is defined in the 1991 Meister paper.
  ## If MIN.RATE is bigger than 0, use the electrode if the firing rate is above
  ## MIN.RATE.

  n <- length(spikes)
  if (n == 1) {
    ## If only one spike train, cannot compute the cross corr indexes.
    0;
  } else {
    t_max <- max(unlist(spikes)) # time of last spike.
    t_min <- min(unlist(spikes)) # time of first spike.

    no_minimum <- isTRUE(all.equal(min_rate, 0))

    if (!no_minimum) {
      ## precompute rates, and find which electrodes are okay.
      rates <- sapply(spikes, length) / (t_max - t_min)
      rates_ok <- rates > min_rate
      cat(sprintf("Rejecting %d electrodes with firing rate below %.3f Hz\n",
        n - sum(rates_ok), min_rate))
    } else {
      rates_ok <- rep(0, n) # need to pass to C anyway...
    }

    ## create one long vector of spikes.
    all_spikes <- unlist(spikes)
    nspikes <- sapply(spikes, length)
    duration <- Tmax - Tmin
    
    first.spike <- c(0, cumsum(nspikes)[-n])
    ## sjecpp
    ## z <- .C("count_overlap_arr",
    ##     as.double(all.spikes),
    ##     as.integer(n),
    ##     as.integer(nspikes),
    ##     as.integer(first.spike),
    ##     as.integer(rates.ok),
    ##     as.integer(no.minimum),
    ##     as.double(duration),
    ##     as.double(dt),
    ##     res = double(n*n))

    ## TODO - what to do about the correlation index?!?
    ## array(z$res, dim=c(n,n))
    array(NA, dim=c(n,n))
    
  }
}

.corr_get_means <- function(id, mid) {
  ## mid contains the mid point of each bin.
  data_by_bin <- split(id[, "corr"], id[, "dist_bin"])
  bins_found <- as.integer(names(data_by_bin)) # assume sorted?
  mids <- mid[bins_found]
  means <- sapply(data_by_bin, mean)
  sds <- sapply(data_by_bin, sd)
  n <- sapply(data_by_bin, length)
  cbind(mid = mids, mean = means, sd = sds, n = n)
}

.corr_do_fit <- function(id, plot=TRUE, show_ci=FALSE, ...) {
  ## Do the fit to the exponential and optionally plot it.  Any
  ## correlation index of zero is removed, since we cannot take the
  ## log of zero.  Hopefully there won't be too many of these.
  ## If SHOW.CI is true, do the fit with 95% confidence intervals.

  y_zero <- which(id[, 2] == 0)
  if (length(y_zero) > 0) {
    id <- id[- y_zero, ]
    warning(paste("removing", length(y_zero), "zero entries"))
  }
  x <- id[, 1]
  ylog <- log(id[, 2])
  fit <- lm(ylog ~ x)
  if (show_ci) {
    ## TODO: why is 850 hard coded in here
    expt_new <- data.frame(x = seq(0, 850, 10)) # range of x for predictions.
    expt_clim <- predict(fit, expt_new, interval = "confidence")
  }
  if (plot) {
    if (show_ci) {
      ## Confidence intervals will show mean, so don't need
      ## to do both matlines and curve.
      matlines(expt_new$x, exp(expt_clim), lty = c(1, 2, 2),
        col = "black")
    } else {
      curve(exp(fit$coeff[1]) * exp(x * fit$coeff[2]), add = TRUE,
        from = 0, ...)
    }
  }
  fit
}

.my_upper <- function(x, diag=FALSE) {
  ## Return the upper triangular elements of a matrix on a
  ## column by column basis.
  ## e.g. my upper matrix 1:9, nrow 3, diag true.
  ## returns 1 4 5 7 8 9
  if (is.matrix(x)) {
    x[which(upper.tri(x, diag))]
  } else {
    stop(paste(deparse(substitute(x)), "is not a matrix"))
  }
}


##' Compute tiling coefficient for an MEA recording.
##'
##' Given an s object, we return all pairwise correlations.
##' @param s  The spike object.
##' @param dt Time window, in seconds, for coincident activity.
##' @return Upper triangular matrix of tiling coefficients.
##' @author Stephen Eglen
.tiling_allpairwise <- function(s, dt=0.05) {
  ## sjecpp
  m <- sttc_allspikes1(s$spikes,
                       dt,
                       s$rec.time[1], s$rec.time[2])
  m
}
