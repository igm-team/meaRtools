get.burst.info <- function(allb, index) {
  ## Extra some part of the Burst information, for each channel.
  ## index will be the name of one of the columns of burst info.
  ## This is a HELPER function for calc.burst.summary
  sapply(allb, function(b) {
    if (length(b) > 1) {
      b[, index]
    } else {
      0
    }
  }, simplify = FALSE)
}

calc.burst.summary <- function(s, bursty.threshold=1) {
  ## bursty.threshold: min number of  bursts/minute to count as
  ## a bursty unit.

  ## Compute the summary burst information.  Use a separate
  ## call (write.csv() for example) to write the burst information to file.

  ## The columns of the data.frame returned.
  ## channels - electrode name
  ## spikes - #spikes
  ## mean.freq - firing rate (Hz)
  ## nbursts - #bursts detected
  ## bursts.per.sec - #bursts/second.matrix(nrow=0,ncol=1)
  ## bursts.per.min - #bursts/min
  ## bursty - is bursts.per.min >bursty.threshold (defaults to 1 burst/min)
  ## mean.dur - mean burst duration
  ## sd.dur - sd
  ## mean.spikes - mean #spikes in a burst
  ## sd.spikes  - sd
  ## per.spikes.in.burst - % of spikes in a burst
  ## per.spikes.out.burst- % of spikes not in a burst
  ## mean.si - mean Surprise Index (only for poisson .surprise measure)
  ## mean.isis - mean ISI within a burst (old name: mean2.isis)
  ## sd.mean.isis - sd
  ## mean.IBIs - mean IBI
  ## sd.IBIs - sd
  ## cv.IBIs - Coefficient of variation of IBI (= mean.IBI/sd.IBI)

  allb <- s$allb

  ## Create a table of output results.

  channels <- s$channels
  spikes <- as.vector(s$nspikes)

  duration <- s$rec.time[2] - s$rec.time[1]

  mean.freq <- round(spikes / duration, 3)

  nbursts <- sapply(allb, .num.bursts)

  bursts.per.sec <- round(nbursts / duration, 3)
  bursts.per.min <- bursts.per.sec * 60


  bursty = ifelse(bursts.per.min >= bursty.threshold, 1, 0)

  durations <- get.burst.info(allb, "durn")
  mean.dur <- round(sapply(durations, mean), 3)
  sd.dur <- round(sapply(durations, sd), 3)

  ISIs = .calc.all.isi(s, allb)
  mean.ISIs = sapply(ISIs, mean)
  sd.ISIs = unlist(sapply(ISIs, sd, na.rm = TRUE))


  ns <- get.burst.info(allb, "len")
  mean.spikes <- round(sapply(ns, mean), 3)
  sd.spikes <- round(sapply(ns, sd), 3)
  total.spikes.in.burst <- sapply(ns, sum)
  per.spikes.in.burst <- round(100 * (total.spikes.in.burst / spikes), 3)

  si <- get.burst.info(allb, "SI")
  mean.si <- round(sapply(si, mean), 3)


  IBIs <- .calc.all.ibi(s, allb)
  mean.IBIs <- sapply(IBIs, mean)
  sd.IBIs <- sapply(IBIs, sd, na.rm = TRUE)
  cv.IBIs <- round(sd.IBIs / mean.IBIs, 3)
  ## round afterwards...
  mean.IBIs <- round(mean.IBIs, 3); sd.IBIs <- round(sd.IBIs, 3)

  df <- data.frame(channels = channels, spikes = spikes, mean.freq = mean.freq,
    nbursts = nbursts,
    bursts.per.sec = bursts.per.sec,
    bursts.per.min = bursts.per.min,
    bursty = bursty,
    mean.dur = mean.dur,
    sd.dur = sd.dur,
    mean.spikes = mean.spikes,
    sd.spikes = sd.spikes,
    per.spikes.in.burst = per.spikes.in.burst,
    per.spikes.out.burst = round(100.0 - per.spikes.in.burst, 3),
    mean.si = mean.si,
    mean.isis = mean.ISIs,
    sd.mean.isis = sd.ISIs,
    mean.IBIs = mean.IBIs,
    sd.IBIs = sd.IBIs,
    cv.IBIs = cv.IBIs
  )
  df

}

.plot.burst.info <- function(allb, index, ylab=NULL, max=- 1, title="") {
  ## Plot result of burst analysis in a stripchart, one col per channel.
  ## Feature plotted is given by index, e.g. "durn", "len".

  ## plot.channels <- min(length(allb), 70)
  plot.channels <- length(allb) # plot all channels.

  values <- list()
  for (i in 1:plot.channels) {
    b <- allb[[i]]
    if (.num.bursts(b) == 0) {
      res <- NULL
    } else {
      res <- b[, index]
    }

    infs <- which(res == Inf)
    if (length(infs) > 0) 
    res <- res[- infs]

    values[[i]] <- res
  }

  if (max > 0) {
    values <- sapply(values, pmin, max)
  }
  mins <- min(sapply(values, min), na.rm = TRUE)
  maxs <- max(sapply(values, max), na.rm = TRUE)

  if (is.null(ylab))
    ylab = index

  stripchart(values, method = "jitter", pch = 20, vert = TRUE, main = title,
    ylim = c(mins, maxs),
    xlab = "channel", ylab = ylab)

}

.num.bursts <- function(b) {
  ## Return the number of bursts found for a spike train.
  if (is.na(b[1]))
    0
    else 
  nrow(b)
}

.calc.all.isi <- function(s, allb) {
  ## Compute ISI within bursts for all spike trains.

  calc.isi = function(spikes, b) {
    ## for one spike train, get all ISIs within bursts in that train.
    if (.num.bursts(b) == 0) {
      return(NA)
    }

    ## as.vector is needed below in case each burst is of the same
    ## length (in which case an array is returned by apply).  In
    ## normal cases, bursts are of different lengths, so "apply"
    ## returns a list.

    isis = as.vector(
      unlist(apply(b, 1,
        function(x) {
          diff(spikes[ x[1]:x[2]])
        })))
  }

  nchannels <- s$NCells
  ISIs = list()
  for (i in 1:nchannels) {
    ISIs[[i]] = calc.isi(s$spikes[[i]], allb[[i]])
  }

  ISIs
}

.calc.all.ibi <- function(s, allb) {
  ## Compute IBI for all spike trains.
  nchannels <- s$NCells
  IBIs = list()
  for (i in 1:nchannels) {
    IBIs[[i]] = .calc.ibi(s$spikes[[i]], allb[[i]])
  }

  IBIs
}

.calc.ibi <- function(spikes, b) {
  ## Compute the interburst intervals (IBI) for one spike train.
  ## Only valid if more than one burst.

  nburst = .num.bursts(b)
  if (nburst == 0) {
    res = NA # no bursts
  } else {
    if (nburst == 1) {
      res = NA # cannot compute  IBI w/only 1 burst.
    } else {
      ## find end spike in each burst.
      end = b[, "beg"] + b[, "len"] - 1

      ## for NBURST bursts, there will be NBURST-1 IBIs.
      start.spikes = b[2:nburst, "beg"]
      end.spikes = end[1:(nburst - 1)]
      ## NEX uses a strange definition of IBI -- it counts the time between
      ## the first spike of burst N and the first spike of burst N+1 as the
      ## IBI.  If we want to use that definition, use the following line:
      ## end.spikes   = b[1:(nburst-1),"beg"]
      res = spikes[start.spikes] - spikes[end.spikes]
    }
  }
  res
}

.mean.burst.summary = function(allb.sum) {
  ## Summarise the burst information.  This does not handle per.spikes.in.burst
  subset = allb.sum[which(allb.sum$bursty == 1), ]

  fields = c("spikes", "mean.dur", "cv.IBIs", "bursts.per.min", "per.spikes.in.burst")
  res = rep(0, length(fields) * 2)
  names(res) = paste(rep(fields, each = 2), c("m", "sd"), sep = ".")
  n = 1
  for (field in fields) {
    dat = subset[[field]]
    if (length(dat) > 0) {
      mean = mean(dat, na.rm = TRUE); sd = sd(dat, na.rm = TRUE);
    } else {
      mean = sd = NA;
    }
    res[n] = mean; res[n + 1] = sd
    n = n + 2
  }

  res

}
