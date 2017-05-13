## Compute the correlation index, as defined by Wong et al (1993).
## Author: Stephen J Eglen
## Copyright: GPL
## Sun 04 Mar 2007

.corr.index <- function(s, distance.breaks,
                       dt=getOption("meaRtools.corr.dt", default=0.05),
                       min.rate=0,
                       corr.method = getOption("meaRtools.corr.method", default="CI")) {
  ## Make a correlation index object.
  ## MIN.RATE: if greater than zero, we analyse only spike trains whose
  ## firing rate is greater than this minimal rate.
  ## corr.method is which method to use.
  dists = .make.distances(s$layout$pos)
  dists.bins = .bin.distances(dists, distance.breaks)

  spikes = s$spikes
  if (length(spikes) > 1) {
    ## SJE: 2010-03-17 -- try new version of corr index.
    ##corr.indexes = .make.corr.indexes(spikes, dt, min.rate)
    corr.indexes = NULL
    if (corr.method == "CI") {
      corr.indexes = .make.corr.indexes2(spikes, dt, min.rate)
    }
    if (corr.method == "Tiling") {
      corr.indexes = .tiling.allpairwise(s, dt)
    }
    if (is.null(corr.method)) {
      stop("Corr index not calculated")
    }
    
    
    corr.id = cbind(dist=.my.upper(dists), corr=.my.upper(corr.indexes),
      dist.bin=.my.upper(dists.bins))
    ##corr.id.means = .corr.get.means(corr.id)
    dist.mids = diff(distance.breaks)/2 +
      distance.breaks[-(length(distance.breaks))]
    corr.id.means = .corr.get.means(corr.id, dist.mids)
  } else {
    corr.indexes = NA
    corr.id = NA
    corr.id.means = NA
  }

  ## distance.breaks.strings used only by Mutual Information Code?
  distance.breaks.strings =
    levels(cut(0, distance.breaks, right=FALSE, include.lowest=TRUE))

  res = list(
    ##dists=dists, dists.bins = dists.bins,
    ##corr.indexes = corr.indexes,
    dt = dt,
    corr.id = corr.id,
    corr.id.means = corr.id.means,
    distance.breaks=distance.breaks,
    distance.mids=dist.mids,
    distance.breaks.strings=distance.breaks.strings,
    method=corr.method)

  res
}


.make.distances <- function(posns, rm.lower=TRUE) {
  ## POSNS should be a (N,2) array.  Returns a NxN upper triangular
  ## array of the distances between all pairs of cells.

  x <- posns[,1]; y <- posns[,2]
  d = round(sqrt(outer(x, x, "-")^2 + outer(y, y, "-")^2))
  if (rm.lower)
    d[lower.tri(d)] = 0
  
  d
}


.bin.distances <- function(dists, breaks) {
  ## DISTS is a upper NxN array.
  ## breaks is a vector of breakpoints.
  ## Return an array of the same size where each distance value is
  ## given a corresponding bin number.

  ## e.g.
  ## dists <- matrix( c(0,0,0, 400,0,0, 80, 1000, 0), nrow=3)
  ## jay.bin.distances(dists)
  ## This binning procedure can then be checked by comparing the
  ## distances and their bins:
  ## plot(.my.upper(s$dists.bins), .my.upper(s$dists))
  ## boxplot(.my.upper(s$dists)~ .my.upper(s$dists.bins))
  
  distances <- .my.upper(dists)
  ## These breaks are hardcoded.

  ##data <- c(0, 100, 700, 900, 400)

  ## Make each bin [low, high) with exception that highest bin is
  ## [low,high]. Labels is false so that we just return numeric count
  ## of bin, rather than a factor.
  bins <- cut(distances, breaks, right=FALSE,
              include.lowest=TRUE, labels=FALSE)
  invalid <- is.na(bins)
  if (any(invalid))
    stop(paste("distances not binned:",
                  paste(distances[which(invalid)],collapse=" ")))
  n <- dim(dists)[1]
  res <- matrix(0, nrow=n, ncol=n)
  res[which(upper.tri(res))] <- bins
  
  res
}

.make.corr.indexes2 <- function(spikes, dt, min.rate=0) {
  ## New version using the C routine for corr indexing.
  ## Return the correlation index values for each pair of spikes.
  ## The matrix returned is upper triangular.
  ## SPIKES should be a list of length N, N is the number of electrodes.
  ## "dt" is the maximum time for seeing whether two spikes are coincident.
  ## This is defined in the 1991 Meister paper.
  ## If MIN.RATE is >0, use the electrode if the firing rate is above
  ## MIN.RATE.
  
  n <- length(spikes)
  if (n == 1) {
    ## If only one spike train, cannot compute the cross-corr indexes.
    0;
  } else {
    Tmax <- max(unlist(spikes))           #time of last spike.
    Tmin <- min(unlist(spikes))           #time of first spike.
    
    no.minimum <- isTRUE(all.equal(min.rate, 0))

    if (!no.minimum) {
      ## precompute rates, and find which electrodes are okay.
      rates <- sapply(spikes, length) / (Tmax - Tmin)
      rates.ok <- rates > min.rate
      cat(sprintf('Rejecting %d electrodes with firing rate below %.3f Hz\n',
             n-sum(rates.ok), min.rate))
    } else {
      rates.ok <- rep(0, n)             #need to pass to C anyway...
    }
    
    ## corrs <- array(0, dim=c(n,n))

    ## create one long vector of spikes.
    all.spikes <- unlist(spikes)
    nspikes <- sapply(spikes, length)
    duration <- Tmax - Tmin
    
    first.spike <- c(0, cumsum(nspikes)[-n])
    z <- .C("count_overlap_arr",
            as.double(all.spikes),
            as.integer(n),
            as.integer(nspikes),
            as.integer(first.spike),
            as.integer(rates.ok),
            as.integer(no.minimum),
            as.double(duration),
            as.double(dt),
            res = double(n*n))

    ## return the result.
    array(z$res, dim=c(n,n))
  }
}

.corr.get.means <- function(id, mid) {
  ## mid contains the mid-point of each bin.
  data.by.bin = split(id[,"corr"], id[,"dist.bin"])
  bins.found = as.integer(names(data.by.bin)) #assume sorted?
  mids = mid[bins.found]
  means = sapply(data.by.bin, mean)
  sds = sapply(data.by.bin, sd)
  n = sapply(data.by.bin, length)
  cbind(mid=mids, mean=means, sd=sds, n=n)
}

.corr.do.fit <- function(id, plot=TRUE, show.ci=FALSE, ...) {
  ## Do the fit to the exponential and optionally plot it.  Any
  ## correlation index of zero is removed, since we cannot take the
  ## log of zero.  Hopefully there won't be too many of these.
  ## If SHOW.CI is true, do the fit with 95% confidence intervals.

  y.zero <- which(id[,2]==0)
  if (length(y.zero)>0) {
    id <- id[-y.zero,]
    warning(paste("removing", length(y.zero),"zero entries"))
  }
  x <- id[,1]
  y.log <- log(id[,2])
  fit <- lm(y.log ~ x)
  if (show.ci) {
    ## TODO: why is 850 hard-coded in here?
    expt.new <- data.frame(x = seq(0, 850, 10))  #range of x for predictions.
    expt.clim <- predict(fit, expt.new, interval="confidence")
  }
  if (plot)  {
    if (show.ci) {
      ## Confidence intervals will show mean, so don't need
      ## to do both matlines and curve.
      matlines(expt.new$x, exp(expt.clim), lty=c(1,2,2),
               col="black")
    } else {
      curve(exp(fit$coeff[1]) * exp(x * fit$coeff[2]), add = TRUE,
            from=0, ...)
    }
  }
  fit
}

.my.upper <- function (x,diag=FALSE) {
  ## Return the upper triangular elements of a matrix on a
  ## column-by-column basis.
  ## e.g. .my.upper(matrix(1:9, nrow=3), diag=TRUE).
  ## returns >>1 4 5 7 8 9<<
  if (is.matrix(x)) {
   x[ which(upper.tri(x,diag))]
  } else {
    stop(paste(deparse(substitute(x)),"is not a matrix"))
  }
}


##' Compute tiling coefficient for an MEA recording.
##'
##' Given an s object, we return all pairwise correlations.
##' @param s  The spike object.
##' @param dt Time-window (in seconds) for coincident activity.
##' @return Upper triangular matrix of tiling coefficients.
##' @author Stephen Eglen
.tiling.allpairwise <- function(s, dt=0.05) {
  n <- length(s$spikes)
  
  all.spikes <- unlist(s$spikes)
  nspikes <- sapply(s$spikes, length)
  first.spike <- c(0, cumsum(nspikes)[-n])
  z <- .C("tiling_arr",
          as.double(all.spikes),
          as.integer(n),
          as.integer(nspikes),
          as.integer(first.spike),
          as.double(s$rec.time),
          as.double(dt),
          res = double(n*n))
  
  ## return the result.
  m <- array(z$res, dim=c(n,n))
  m[lower.tri(m)] <- NA                 #we didn't do lower triangle, so ignore those.
  m
}

