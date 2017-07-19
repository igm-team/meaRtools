get_burst_info <- function(allb, index) {
  ## Extra some part of the Burst information, for each channel.
  ## index will be the name of one of the columns of burst info.
  ## This is a HELPER function for calc_burst_summary
  sapply(allb, function(b) {
    if (length(b) > 1) {
      b[, index]
    } else {
      0
    }
  }
  , simplify = FALSE)
}

calc_burst_summary <- function(s, bursty_threshold=1) {
  ## bursty_threshold: min number of  bursts per minute to count as
  ## a bursty unit.

  ## Compute the summary burst information.

  ## The columns of the data.frame returned.
  ## channels: electrode name
  ## spikes: #spikes
  ## mean_freq: firing rate (Hz)
  ## nbursts: #bursts detected
  ## bursts_per_sec: #bursts per second.matrix, nrow is 0,ncol is 1
  ## bursts_per_min: #bursts per min
  ## bursty: is bursts_per_min bigger than bursty_threshold
  ## (defaults to 1 burst per min)
  ## mean_dur: mean burst duration
  ## sd_dur: sd
  ## mean_spikes : mean #spikes in a burst
  ## sd_spikes: sd
  ## per_spikes_in_burst : % of spikes in a burst
  ## per_spikes_out_burst: % of spikes not in a burst
  ## mean_si: mean Surprise Index (only for poisson .surprise measure)
  ## mean_isis: mean ISI within a burst (old name: mean2.isis)
  ## sd_mean_isis: sd
  ## mean_ibis: mean IBI
  ## sd_ibis: sd
  ## cv_ibis: Coefficient of variation of IBI (equals mean_ibi
  ## divided by sd_ibi)

  allb <- s$allb

  ## Create a table of output results.

  channels <- s$channels
  spikes <- as.vector(s$nspikes)

  duration <- s$rec_time[2] - s$rec_time[1]

  mean_freq <- round(spikes / duration, 3)

  nbursts <- sapply(allb, .num_bursts)

  bursts_per_sec <- round(nbursts / duration, 3)
  bursts_per_min <- bursts_per_sec * 60


  bursty <- ifelse(bursts_per_min >= bursty_threshold, 1, 0)

  durations <- get_burst_info(allb, "durn")
  mean_dur <- round(sapply(durations, mean), 3)
  sd_dur <- round(sapply(durations, sd), 3)

  isis <- meaRtools:::.calc_all_isi(s, allb)
  mean_isis <- sapply(isis, mean)
  sd_isis <- unlist(sapply(isis, sd, na.rm = TRUE))


  ns <- get_burst_info(allb, "len")
  mean_spikes <- round(sapply(ns, mean), 3)
  sd_spikes <- round(sapply(ns, sd), 3)
  total_spikes_in_burst <- sapply(ns, sum)
  per_spikes_in_burst <- round(100 * (total_spikes_in_burst / spikes), 3)

  si <- get_burst_info(allb, "si")
  mean_si <- round(sapply(si, mean), 3)


  ibis <- .calc_all_ibi(s, allb)
  mean_ibis <- sapply(ibis, mean)
  sd_ibis <- sapply(ibis, sd, na.rm = TRUE)
  cv_ibis <- round(sd_ibis / mean_ibis, 3)
  ## round afterwards...
  mean_ibis <- round(mean_ibis, 3); sd_ibis <- round(sd_ibis, 3)

  df <- data.frame(channels = channels, spikes = spikes, mean_freq = mean_freq,
    nbursts = nbursts,
    bursts_per_sec = bursts_per_sec,
    bursts_per_min = bursts_per_min,
    bursty = bursty,
    mean_dur = mean_dur,
    sd_dur = sd_dur,
    mean_spikes = mean_spikes,
    sd_spikes = sd_spikes,
    per_spikes_in_burst = per_spikes_in_burst,
    per_spikes_out_burst = round(100.0 - per_spikes_in_burst, 3),
    mean_si = mean_si,
    mean_isis = mean_isis,
    sd_mean_isis = sd_isis,
    mean_ibis = mean_ibis,
    sd_ibis = sd_ibis,
    cv_ibis = cv_ibis
  )
  df

}

.plot_burst_info <- function(allb, index, ylab=NULL, max=- 1, title="") {
  ## Plot result of burst analysis in a stripchart, one col per channel.
  ## Feature plotted is given by index, e.g. "durn", "len".

  plot_channels <- length(allb) # plot all channels.

  values <- list()
  for (i in 1:plot_channels) {
    b <- allb[[i]]
    if (.num_bursts(b) == 0) {
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
    ylab <- index

  stripchart(values, method = "jitter", pch = 20, vert = TRUE, main = title,
    ylim = c(mins, maxs),
    xlab = "channel", ylab = ylab)

}

.num_bursts <- function(b) {
  ## Return the number of bursts found for a spike train.
  if (is.na(b[1]))
    0
    else
  nrow(b)
}

.calc_all_isi <- function(s, allb) {
  ## Compute ISI within bursts for all spike trains.

  calc_isi <- function(spikes, b) {
    ## for one spike train, get all isis within bursts in that train.
    if (.num_bursts(b) == 0) {
      return(NA)
    }

    ## as.vector is needed below in case each burst is of the same
    ## length (in which case an array is returned by apply).  In
    ## normal cases, bursts are of different lengths, so "apply"
    ## returns a list.

    isis <- as.vector(
      unlist(apply(b, 1,
        function(x) {
          diff(spikes[x[1]:x[2]])
        })))
  }

  nchannels <- s$NCells
  isis <- list()
  for (i in 1:nchannels) {
    isis[[i]] <- calc_isi(s$spikes[[i]], allb[[i]])
  }

  isis
}

.calc_all_ibi <- function(s, allb) {
  ## Compute IBI for all spike trains.
  nchannels <- s$NCells
  ibis <- list()
  for (i in 1:nchannels) {
    ibis[[i]] <- .calc_ibi(s$spikes[[i]], allb[[i]])
  }

  ibis
}

.calc_ibi <- function(spikes, b) {
  ## Compute the interburst intervals (IBI) for one spike train.
  ## Only valid if more than one burst.

  nburst <- .num_bursts(b)
  if (nburst == 0) {
    res <- NA # no bursts
  } else {
    if (nburst == 1) {
      res <- NA # cannot compute  IBI w/only 1 burst.
    } else {
      ## find end spike in each burst.
      end <- b[, "beg"] + b[, "len"] - 1

      ## for n bursts, there will be n bursts minus 1 IBIs.
      start_spikes <- b[2:nburst, "beg"]
      end_spikes <- end[1:(nburst - 1)]
      ## NEX uses a strange definition of IBI: it counts the time between
      ## the first spike of n burst and the first spike of n burst
      ## plus one as the IBI.
      ## If we want to use that definition, use the following line:
      ## end.spikes equal b where 1 to nburst minus 1
      res <- spikes[start_spikes] - spikes[end_spikes]
    }
  }
  res
}

.mean_burst_summary <- function(allb_sum) {
  ## Summarise the burst information.  This does not handle per_spikes_in_burst
  subset <- allb_sum[which(allb_sum$bursty == 1), ]

  fields <- c("spikes", "mean_dur", "cv_ibis", "bursts_per_min",
             "per_spikes_in_burst")
  res <- rep(0, length(fields) * 2)
  names(res) <- paste(rep(fields, each = 2), c("m", "sd"), sep = ".")
  n <- 1
  for (field in fields) {
    dat <- subset[[field]]
    if (length(dat) > 0) {
      mean <- mean(dat, na.rm = TRUE); sd <- sd(dat, na.rm = TRUE);
    } else {
      mean <- sd <- NA;
    }
    res[n] <- mean; res[n + 1] <- sd
    n <- n + 2
  }

  res

}
