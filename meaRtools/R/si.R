## Poisson .surprise method for burst analysis -- L'egendy and Salcman (1985)
## Author: Stephen J Eglen
## Copyright: GPL

######################################################################

.burst.info <- c("beg", "len", "SI", "durn", "mean_isis")
.burst.info.len = length(.burst.info)

##' Burst detection of MEA spike trains.
##'
##' For a set of spike trains in an MEA recording, find the bursts
##' independently within each spike train.
##'
##'
##' @param s MEA data structure
##' @param method A string, either "si" (.surprise method), "mi" (maxinterval),
##' "logisi" (Log ISI histogram).
##' @return Return the "all bursts" data structure.  This is a list of
##' matrices, giving the burst information for each electrode.
##'
##' Each matrix stores basic information about each burst.  There is one row
##' for every burst, with the following columns:
##'
##' \tabular{ll}{ beg \tab index of the first spike in the burst \cr len \tab
##' number of spikes in this burst \cr SI \tab .surprise index (calculated only
##' for the .surprise method)\cr durn \tab duration (in s) of the burst\cr
##' mean_isis \tab mean of all interspike intervals.\cr }
##'
##' If no bursts could be found within a spike train, the value NA is used
##' rather than an empty matrix.
##' @keywords Burst analysis, MEA analysis
.spikes.to.bursts <- function(s, method="si") {
  ## Entry function for burst analysis.
  ## Possible methods:
  ## "mi" - maxinterval
  ## "si" - .surprise index.
  ## "logisi" - log of the ISI histogram

  ncells <- s$NCells

  if (method == "logisi") {
    isi.low <- .logisi.compute(s)$Locmin
    logisi.par$isi.low <- isi.low
  }

  allb <- list()
  for (train in 1:ncells) {
    ## cat(sprintf("** analyse train %d\n", train))
    spikes <- s$spikes[[train]]

    bursts = switch(method,
      "mi" = mi.find.bursts(spikes, s$parameters$mi.par),
      "si" = si.find.bursts(spikes, s$parameters$s_min),
      "logisi" = .logisi.find.burst(spikes),
      stop(method, " : no such method for burst analysis")
    )

    allb[[train]] <- bursts
  }

  allb
}

si.find.bursts <- function(spikes, s_min, burst.isi.max = NULL) {
  debug = FALSE
  no.bursts = matrix(nrow = 0, ncol = 1)
  nspikes = length(spikes)
  mean_isi = mean(diff(spikes))
  threshold = mean_isi / 2
  n = 1
  max.bursts <- floor(nspikes / 3)
  bursts <- matrix(NA, nrow = max.bursts, ncol = .burst.info.len)
  burst <- 0
  while (n < nspikes - 2) {
    if (debug) 
    print(n)
    if (((spikes[n + 1] - spikes[n]) < threshold) &&
      ((spikes[n + 2] - spikes[n + 1]) < threshold)) {
        res <- .si.find.burst2(n, spikes, nspikes, mean_isi,
          burst.isi.max, debug, s_min)
        if (is.na(res[1])) {
          n <- n + 1
        }
        else {
          burst <- burst + 1
          if (burst > max.bursts) {
            print("too many bursts")
            browser()
          }
          bursts[burst, ] <- res
          n <- res[1] + res[2]
          names(n) <- NULL
        }
      }
      else {
      n = n + 1
    }
  }
  if (burst > 0) {
    res <- bursts[1:burst, , drop = FALSE]
    colnames(res) <- .burst.info
  } else {
    res <- no.bursts # NA
    return(res) # return if there's no bursts
  }
  # res
  # get IBI
  end <- res[, "len"] + res[, "beg"] - 1
  IBI <- NA
  for (cur.b in 2:length(end)) {
    IBI <- c(IBI, spikes[end[cur.b]] - spikes[end[cur.b - 1] ])
  }
  # res2 <- matrix(nrow = length(end), ncol = 7)
  res2 <- cbind(res[, "beg"], end, IBI,
    res[, "len"], res[, "durn"], res[, "mean_isis"], res[, "SI"])
  colnames(res2) <- c("beg", "end", "IBI", "len", "durn", "mean_isis", "SI")

  res2
}


.si.find.burst2 <- function(n, spikes, nspikes, mean_isi, threshold=NULL,
  debug=FALSE, s_min=5) {
  ## Find a burst starting at spike N.
  ## Include a better phase 1.


  ## Determine ISI threshold.
  if (is.null(threshold))
    isi_thresh <- 2 * mean_isi
    else
    isi_thresh <- threshold

  if (debug)
      cat(sprintf("** .find.burst %d\n", n))

  i <- 3 ## First three spikes are in burst.
  s <- .surprise(n, i, spikes, nspikes, mean_isi)

  ## Phase 1 - add spikes to the train.
  phase1 <- TRUE

  ## in Phase1, check that we still have spikes to add to the train.
  while (phase1) {

    i.cur <- i

    ## CHECK controls how many spikes we can look ahead until SI is maximised.
    ## This is normally 10, but will be less at the end of the train.
    check <- min(10, nspikes - (i + n - 1))

    looking <- TRUE
    okay <- FALSE
    while (looking) {

      if (check == 0) {
        ## no more spikes left to check.
        looking <- FALSE
        break;
      }
      check <- check - 1
      i <- i + 1
      s_new <- .surprise(n, i, spikes, nspikes, mean_isi)
      if (debug)
         cat(sprintf("s_new %f s %f n %d i %d check %d\n",
                  s_new, s, n, i, check))

      if (s_new > s) {
        okay <- TRUE
        looking <- FALSE
      } else {
        ## See if we should keep adding spikes?
        if ((spikes[i] - spikes[i - 1]) > isi_thresh) {
          looking <- FALSE
        }

      }
    }
    ## No longer checking, see if we found an improvement.
    if (okay) {
      if (s > s_new) {
        ## This should not happen.
        cat(sprintf("before s %f s_new %f\n", s, s_new))
        browser()
      }
      s <- s_new
    } else {
      ## Could not add more spikes onto the end of the train.
      phase1 <- FALSE
      i <- i.cur
    }
  }


  ## start deleting spikes from the start of the burst.
  phase2 <- TRUE
  while (phase2) {
    if (i == 3) {
      ## minimum length of a burst must be 3.
      phase2 <- FALSE
    } else {
      s_new <- .surprise(n + 1, i - 1, spikes, nspikes, mean_isi)
      if (debug)
      cat(sprintf("phase 2: n %d i %d s_new %.4f\n", n, i, s_new))
      if (s_new > s) {
        if (debug)
        print("in phase 2 acceptance\n")
        n <- n + 1
        i <- i - 1
        s <- s_new
      } else {
        ## removing front spike did not improve SI.
        phase2 <- FALSE
      }
    }
  }


  ## End of burst detection; accumulate result.
  if (s > s_min) {

    ## compute the ISIs, and then the mean ISI.

    ## Fencepost issue: I is the number of spikes in the burst, so if
    ## the first spike is N, the last spike is at N+I-1, not N+I.
    isis <- diff(spikes[n + (0:(i - 1))])
    mean_isis <- mean(isis)

    durn <- spikes[n + i - 1] - spikes[n]
    res <- c(n = n, i = i, s = s, durn = durn, mean_isis = mean_isis)

    if (debug)
    print(res)

  } else {
    ## burst did not have high enough SI.
    res <- rep(NA, .burst.info.len)
  }
  res

}

.surprise <- function(n, i, spikes, nspikes, mean_isi) {
  ## Calculate .surprise index for spike train.
  dur <- spikes[n + i - 1] - spikes[n]
  lambda <- dur / mean_isi
  p <- ppois(i - 2, lambda, lower.tail = FALSE)
  s <- -log(p)
  s
}
