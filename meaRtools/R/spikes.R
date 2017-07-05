.construct.s <- function(spikes, ids, time.interval, beg, end, corr.breaks,
  layout, filename) {

  spikes.range <- range(unlist(spikes))

  if (is.null(end)) {
    end <- spikes.range[2]
  } else {
    spikes <- lapply(spikes,
      function(x, max) {
        ## Any times greater than MAX are removed.
        x.high <- which(x > max)
        if (any(x.high))
          x[1:x.high[1] - 1]
          else 
        x
      },
      max = end)

  }

  if (is.null(beg)) {
    beg <- spikes.range[1]
  } else {
    spikes <- lapply(spikes,
      function(x, min) {
        ## Any times less than MIN are removed.
        x.low <- which(x < min)
        if (any(x.low)) 
        x <- x[- x.low]
        x
      },
      min = beg)
  }

  ## Remove any channels that have zero spikes (which can happen if
  ## the beg, end range is too narrow, or if a datafile is empty,
  ## which happens sometime for the Feller data.
  empty.index <- which(sapply(spikes, length) == 0)
  if (any(empty.index)) {
    spikes <- spikes[- empty.index]
  }

  if (!is.null(ids)) {
    spikes <- .filter.channel.names(spikes, ids)
  }

  ## No more spike trains should be removed, so we can refilter the layout to ensure
  ## that only spikes that are left in the array are kept in the layout.
  channels <- names(spikes)
  keep <- match(channels, rownames(layout$pos))
  layout$pos <- layout$pos[keep, ]
  rec.time <- c(beg, end)
  nspikes <- sapply(spikes, length)

  if (length(nspikes) == 0) {
    meanfiringrate <- nspikes # empty list
  } else {
    meanfiringrate <- nspikes / (end - beg)
  }

  rates <- .make.spikes.to.frate(spikes, time.interval = time.interval,
    beg = beg, end = end)
  unit.offsets <- NULL
  .check.spikes.monotonic(spikes)
  res <- list(channels = names(spikes), spikes = spikes, nspikes = nspikes,
    NCells = length(spikes), meanfiringrate = meanfiringrate,
    file = filename, layout = layout, rates = rates, unit.offsets = unit.offsets,
    rec.time = rec.time)
  class(res) <- "spike.list"
  if (length(corr.breaks) == 1) {
    res$corr = NULL
  } else {
    res$corr = .corr.index(res, corr.breaks)
  }
  res
}


calculate.isis <- function(s) {
  s$isis <- lapply(s$spikes, diff)
  s$mean.isis <- lapply(s$isis, mean)
  s$sd.isis <- lapply(s$isis, sd)
  return(s)
}

.plot.isis.by.plate <- function(s) {
  isis.all <- unlist(s$isis)
  hist(isis.all, main = "Histogram of ISIs by Plate", xlab = "ISI length")
  hist(log10(isis.all), main = "Histogram of log(ISIs) by Plate", xlab = "log10(ISI length)")
}
.spike.summary.by.electrode <- function(s) {
  s <- calculate.isis(s)
  electrodes <- .get_all_electrodes(s)
  sum <- matrix(data = NA, nrow = length(electrodes), ncol = 4)
  colnames(sum) <- c("nspikes", "meanfiringrate", "meanisis", "sdisis")
  rownames(sum) <- electrodes

  df <- cbind(s$nspikes, s$meanfiringrate, s$mean.isis, s$sd.isis)
  active.electrodes <- rownames(df)
  for (i in active.electrodes) {
    sum[i, ] <- unlist(df[i, ])
  }
  sum
}

.spike.summary.by.well <- function(s) {
  plate <- .plateinfo(s$layout$array)
  wells <- sort(plate$wells)
  s$isis <- lapply(s$spikes, diff)
  startPos = 1
  sum <- matrix(data = NA, nrow = length(wells), ncol = startPos + 8)
  colnames(sum) <- c("treatment", "nAE", "nspikes_by_well", "meanfiringrate_by_well", "meanfiringrate_by_all_ectctordes", "meanfiringrate_by_active_electordes", "sdfiringrate_by_active_electordes", "meanisis", "sdisis")
  rownames(sum) <- wells
  nelectrodes <- plate$n.elec.r * plate$n.elec.c
  if (!is.null(s$goodwells)) {
    for (j in 1:length(s$goodwells)) {
      icurrentwell <- (s$goodwells[j] == s$cw)
      incurrentwell <- which((s$goodwells[j] == s$cw))

      treatment = s$treatment[s$goodwells[j]]

      if (length(incurrentwell) > 0) {
        well <- strsplit(s$channels[incurrentwell], "_")[[1]][1]
        treatment = s$treatment[well][[1]] }
      sum[s$goodwells[j], startPos] <- treatment
      sum[s$goodwells[j], startPos + 1] <- length(incurrentwell)
      sum[s$goodwells[j], startPos + 2] <- sum(s$nspikes[icurrentwell])
      sum[s$goodwells[j], startPos + 3] <- sum(s$meanfiringrate[icurrentwell])
      sum[s$goodwells[j], startPos + 4] <- sum(s$meanfiringrate[icurrentwell]) / nelectrodes

      sum[s$goodwells[j], startPos + 5] <- mean(s$meanfiringrate[icurrentwell])
      sum[s$goodwells[j], startPos + 6] <- sd(s$meanfiringrate[icurrentwell])

      isis.all <- unlist(s$isis[icurrentwell])
      sum[s$goodwells[j], startPos + 7] <- mean(isis.all)
      sum[s$goodwells[j], startPos + 8] <- sd(isis.all)
    }
  }
  sum
}

.get.div <- function(s) {
  div <- NA
  t1 <- strsplit(s$file, split = "_", fixed = TRUE)
  for (i in t1[[1]]) {
    if (nchar(i) > 2 && substr(i, 1, 3) == "DIV") {
      if (nchar(i) > 5) {
        i = unlist(strsplit(i, split = ".", fixed = T))[1]
      }
      div <- as.numeric(substr(i, 4, nchar(i)))
    }
  }
  div
}

IGM.compute.mean.firingrate.by.well <- function(s) {
  df1 <- aggregate(s$meanfiringrate, by = list(s$cw), FUN = mean, na.rm = T)
  df2 <- aggregate(s$meanfiringrate, by = list(s$cw), FUN = sum, na.rm = T)

  df <- cbind(df1, df2[, 2], .get.div(s))
  names(df) <- c("well", "meanfiringrate", "meanfiringrate_per_well", "div")
  rownames(df) <- t(df["well"])
  df
}

.plot.isis.by.electrode <- function(s) {
  wells <- unique(s$cw)
  if (length(wells) > 0) {
    for (well in wells) {
      active.electrodes <- which(s$cw == well & as.vector(unlist(lapply(s$isis, length))) > 0)
      if (length(active.electrodes) > 0) {
        df <- list()
        for (i in 1:length(active.electrodes)) {
          df[[i]] <- cbind(s$isis[[active.electrodes[i]]],
            names(s$isis)[active.electrodes[i]])
        }
        df = do.call("rbind", df)
        colnames(df) <- c("isis", "electrode")
        plateinfo <- .plateinfo(s$layout$array)
        d1 <- expand.grid(col = 1:plateinfo$n.elec.c, row = 1:plateinfo$n.elec.r)
        all.electrodes <- sort(paste(well, "_", d1[, "row"], d1[, "col"], sep = ""))
        layout.electrodes <- c(plateinfo$n.elec.r, plateinfo$n.elec.c)
        df <- data.frame(df)
        df$isis <- as.numeric(as.vector(df$isis))
        df$electrode <- as.character(as.vector(df$electrode))


        p1 <- histogram(~ isis | factor(electrode, levels = all.electrodes),
          data = df, breaks = 10,
          main = paste("ISIs histogram plot for ", well, sep = ""),
          layout = layout.electrodes,
          drop.unused.levels = FALSE)
        print(p1)

        p2 <- histogram(~ log(isis) | factor(electrode, levels = all.electrodes),
          data = df, breaks = 10,
          main = paste("log(ISIs) histogram plot for ", well, sep = ""),
          layout = layout.electrodes,
          drop.unused.levels = FALSE)
        print(p2)
      }
    }
    p2
  }

}

.plot.mean.firingrate.by.electrode <- function(s) {
  wells <- unique(s$cw)
  if (length(wells) > 0) {
    for (well in wells) {
      active.electrodes <- which(s$cw == well)
      df <- list()
      for (i in active.electrodes) {
        df[[i]] <- cbind(s$rates$times,
          s$rates$rates[, i],
          names(s$nspikes)[i])
      }
      df = do.call("rbind", df)
      maxy <- max(df[, 2])
      colnames(df) <- c("time", "meanfiringrate", "electrode")
      plateinfo <- .plateinfo(s$layout$array)
      d1 <- expand.grid(col = 1:plateinfo$n.elec.c, row = 1:plateinfo$n.elec.r)
      all.electrodes <- sort(paste(well, "_", d1[, "row"], d1[, "col"], sep = ""))
      layout.electrodes <- c(plateinfo$n.elec.r, plateinfo$n.elec.c)
      df <- data.frame(df)

      p1 <- xyplot(meanfiringrate ~ time | factor(electrode, levels = all.electrodes),
        data = df,
        main = paste("Mean Firing Rate per Second for Well ", well, ". Maximum firing rate:", maxy, " Hz", sep = ""),
        layout = layout.electrodes, type = "h",
        scales = list(
          x = list(draw = FALSE),
          y = list(draw = FALSE)),
        drop.unused.levels = FALSE)
      print(p1)
    }
    p1
  }

}

IGM.plot.mean.firingrate.by.eletrode.by.div <- function(s) {
  electrode.stats <- lapply(s, function(d) {cbind(d$meanfiringrate, d$cw, .get.div(d))})
  electrode.stats.all <- do.call("rbind", electrode.stats)
  electrode.names <- row.names(electrode.stats.all)
  electrode.stats.all <- suppressWarnings(data.frame(cbind(electrode.names, electrode.stats.all[, 1:3])))
  names(electrode.stats.all) <- c("electrode", "meanfiringrate", "well", "div")
  electrode.stats.all$div <- as.numeric(as.vector(electrode.stats.all$div))
  electrode.stats.all$meanfiringrate <- as.numeric(as.vector(electrode.stats.all$meanfiringrate))
  electrode.stats.all$electrode <- as.character(as.vector(electrode.stats.all$electrode))


  wells <- unique(electrode.stats.all$well)
  if (length(wells) > 0) {
    for (active.well in wells) {
      df <- electrode.stats.all[which(electrode.stats.all$well == active.well), ]
      layout.info <- .get_electrode_layout(s[[1]], active.well)
      maxy <- max(df$meanfiringrate)

      p1 <- xyplot(meanfiringrate ~ div | factor(electrode, levels = layout.info$electrodes),
        data = df,
        main = paste("Mean Firing Rate across DIV's for ", active.well, ". Maximum firing rate:", round(maxy, 2), " Hz", sep = ""),
        layout = layout.info$layout,
        drop.unused.levels = FALSE)
      print(p1)
    }
  }
}

IGM.plot.mean.firingrate.by.well.by.div <- function(s) {
  well.stats <- lapply(s, function(d) {d$well.stats})
  well.stats.all <- do.call("rbind", well.stats)
  plateinfo <- .plateinfo(s[[1]]$layout$array)
  wells <- plateinfo$wells
  names(wells) <- wells # keep the names valid.
  wells.layout <- plateinfo$layout

  p1 <- xyplot(meanfiringrate ~ div | factor(well, levels = wells), data = well.stats.all,
    main = "Mean Firing Rate across DIV's (Hz/electrode)", layout = wells.layout,
    drop.unused.levels = FALSE)
  print(p1)
  p2 <- xyplot(meanfiringrate_per_well ~ div | factor(well, levels = wells), data = well.stats.all,
    main = "Mean Firing Rate across DIV's (Hz/well)", layout = wells.layout,
    drop.unused.levels = FALSE)
  print(p2)
  # return(list(p1=p1,p2=p2))
}

IGM.plot.plate.summary.for.spikes <- function(s, outputdir) {
  for (i in 1:length(s)) {
    basename <- get_file_basename(s[[i]]$file)
    spikePlotPath = paste(outputdir, "/", basename, "_spike_plot.pdf", sep = "")
    pdf(file = spikePlotPath)

    # layout
    p <- .plot.mealayout(s[[i]]$layout, use.names = T, cex = 0.25)
    title(main = paste(paste("Electrode Layout"),
      paste("file= ", strsplit(basename(s[[i]]$file), ".RData")[[1]][1], sep = ""), sep = "\n"))
    # MFR
    p <- .plot.meanfiringrate(s[[i]], main = "Mean Firing Rate by Plate (Hz)")
    # p<- plot(s[[i]],main = "Raster plots by channel", label.cells = FALSE, use.names = FALSE)
    p <- .plot.isis.by.plate(s[[i]])
    p <- .channel.plot.by.well(s[[i]], resp = "meanfiringrate", resp.label = "Mean Firing Rate (Hz)")
    p <- .plot.mean.firingrate.by.electrode(s[[i]])
    p <- .plot.isis.by.electrode(s[[i]])
    dev.off()
  }
}

write.plate.summary.for.spikes <- function(s, outputdir) {
  csvwell <- paste(outputdir, "/", get_project_plate_name(s[[1]]$file), "_well_spikes.csv", sep = "")

  for (i in 1:length(s)) {
    div <- .get.div(s[[i]])
    basename <- get_file_basename(s[[i]]$file)
    csvfile <- paste(outputdir, "/", basename, "_spikes.csv", sep = "")
    df <- .spike.summary.by.electrode(s[[i]])
    df2 <- .spike.summary.by.well(s[[i]])

    # recording time
    write.table(paste("recording time (s): [", paste(s[[i]]$rec.time[1], round(s[[i]]$rec.time[2]), sep = " ,"),
      "]", sep = ""), csvfile, sep = ",", append = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    write.table("Spike statistics for wells", csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    df2 = cbind(rownames(df2), df2)
    suppressWarnings(write.table(df2,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
    suppressWarnings(write.table(cbind(df2, div),
      csvwell, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE))

    write.table(" ", csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

    write.table("Spike statistics for electrodes",
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    df = cbind(rownames(df), df)
    colnames(df)[1] <- "electrode"
    # summary write data
    suppressWarnings(write.table(df,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
  }
}

.check.spikes.monotonic <- function(spikes) {
  ## Check to see that all spike times are monotonically increasing.
  ## The counting and histogram routines assumes that spike times
  ## are sorted, earliest spikes first.
  results <- sapply(spikes, function(x) { any(diff(x) < 0)})
  if (any(results)) {
    stop(paste("Spikes are not ordered in increasing time",
      paste(which(results), collapse = " "), "\n"))
  }
}

.make.spikes.to.frate <- function(spikes,
  time.interval=1, # time bin of 1sec.
  frate.min=0,
  frate.max=20,
  clip=FALSE,
  beg=NULL,
  end=NULL
  ) {
  ## Convert the spikes for each cell into a firing rate (in Hz)
  ## We count the number of spikes within time bins of duration
  ## time.interval (measured in seconds).
  ## 
  ## Currently cannot specify BEG or END as less than the
  ## range of spike times else you get an error from hist().  The
  ## default anyway is to do all the spikes within a data file.

  ## Note, we need to check for when there are no spikes; this can
  ## happen when examining a subset of spikes, e.g. a well in a multi-well
  ## plate that was not working.
  nspikes <- lapply(spikes, length)
  nelectrodes <- length(nspikes)

  ## if clips is set to TRUE, firing rate is clipped within the
  ## values frate.min and frate.max.  This is problably not needed.

  spikes.range <- range(unlist(spikes))
  if (is.null(beg)) beg <- spikes.range[1]
  if (is.null(end)) end <- spikes.range[2]

  time.breaks <- seq(from = beg, to = end, by = time.interval)
  if (time.breaks[length(time.breaks)] < end) {
    ## extra time bin needs adding.
    ## e.g seq(1,6, by = 3) == 1 4, so we need to add 7 ourselves.
    time.breaks <- c(time.breaks,
      time.breaks[length(time.breaks)] + time.interval)
  }
  nbins <- length(time.breaks) - 1

  z <- .C("frate",
    as.double(unlist(spikes)),
    as.integer(nspikes),
    as.integer(nelectrodes),
    as.double(time.breaks[1]), as.double(time.breaks[nbins]),
    as.double(time.interval),
    as.integer(nbins),
    counts = double(nbins * nelectrodes))

  rates <- matrix(z$counts, nrow = nbins, ncol = nelectrodes)

  ## Check if there are any electrodes to process.
  if (nelectrodes > 0) {
    ## Now optionally set the upper and lower frame rates if clip is TRUE.
    if (clip) 
    rates <- pmin(pmax(rates, frate.min), frate.max)

    ## Do the average computation here.
    ## av.rate == average rate across the array.
    av.rate <- apply(rates, 1, mean)
  } else {
    av.rate <- rep(NA, nbins)
  }
  ## We can remove the last "time.break" since it does not correspond
  ## to the start of a time frame.
  res <- list(rates = rates,
    times = time.breaks[- length(time.breaks)],
    av.rate = av.rate,
    time.interval = time.interval)
  res
}

.plot.meanfiringrate <- function(s, beg, end, main=NULL, lwd=0.2, ...) {
  ## Plot the mean firing rate over all the cells at each time step.
  ## Can optionally specify the beginning (BEG) and end (END) time, in
  ## seconds.

  if (missing(beg)) beg <- s$rates$times[1]
  if (missing(end)) end <- s$rates$times[length(s$rates$times)]

  if (is.null(main))
    main = basename(s$file)

  plot(s$rates$times, s$rates$av.rate, type = "h", xlab = "time (s)",
    xlim = c(beg, end), bty = "n", lwd = lwd,
    ylab = "mean firing rate (Hz)", main = main, ...)
}

.plot.mealayout <- function(x, use.names=FALSE, ...) {

  ## Plot the MEA layout.
  pos <- x$pos
  plot(NA, asp = 1,
    xlim = x$xlim, ylim = x$ylim,
    bty = "n",
    xlab = "spacing (\u00b5m)", ylab = "", type = "n")
  if (use.names)
    text(pos[, 1], pos[, 2], rownames(pos), ...)
    else 
  text(pos[, 1], pos[, 2], ...)
}

.summary.spike.list <- function(object, ...) {
  cat(paste("Spike data:", object$file, "\n"))
  cat(paste("NCells", object$NCells, "\n"))
  cat(sprintf("Time (s) [%.3f %.3f]\n", object$rec.time[1], object$rec.time[2]))
}

.print.spike.list <- function(x) {
  ## Default print method for a SPIKES data structure.
  cat("MEA spikes\n")
  cat(basename(x$file), "\n")
  cat("nchannels ", x$NCells, "\n")
}

isi <- function(train) {
  ## Compute the ISI for one spike train.
  isi <- NA
  if (length(train) > 1) {
    isi <- diff(train)
  }
  isi
}

## read the data and have access to all the meta-data
.spike.simulation <- function(s1,
  elec.min.rate=(1 / 60),
  elec.max.rate=25,
  well.min.rate=15) {
  dt <- 1
  current.electrode.rate <- s1$meanfiringrate
  rec.start <- s1$rec.time[1]
  rec.end <- s1$rec.time[2]
  spikes <- list()
  for (electrode in 1:length(s1$spikes)) {
    rate <- current.electrode.rate[electrode] * dt / 1000.0
    spiketimes <- list()
    timepoints <- seq(rec.start, rec.end, by = 0.001)
    p <- which(rate > runif(length(timepoints)))
    spikes[[electrode]] <- timepoints[which(rate > runif(length(timepoints)))]

  }
  names(spikes) <- names(s1$spikes)
  temp.s <- .construct.s(spikes, NULL, time.interval = 1, beg = NULL, end = NULL, corr.breaks = 0,
    s1$layout, filename = s1$file)


  # indices of low and high firing rate
  low <- which(temp.s$meanfiringrate < elec.min.rate)
  high <- which(temp.s$meanfiringrate > elec.max.rate)

  ## TODO, check that low and high are non-zero length vectors.
  extremes <- c(low, high)

  bad.ids <- names(extremes)
  bad.ids <- c("-", bad.ids) # "-" needed to remove these ids!

  s2 <- remove.spikes(temp.s, bad.ids)

  s2$treatment <- s1$treatment
  s2$size <- s1$size
  s2$units <- s1$units
  s2$dose <- s1$dose
  s2$well <- s1$well

  # get.num.AE
  s2 <- get.num.AE(s2)

  # indices of low and high firing rate

  low <- which(s2$nAE < well.min.rate)

  bad.wells <- names(low)
  bad.wells <- c("-", bad.wells) # "-" needed to remove these well!
  # just these three for example
  s <- remove.spikes(s2, bad.wells)

  s$goodwells <- names(which(s2$nAE >= well.min.rate))

  # [which(s2$nAE >= well.min.rate)
  s$treatment <- s1$treatment
  names(s$treatment) <- s1$well
  s$size <- s1$size
  names(s$size) <- s1$well
  s$units <- s1$units
  names(s$units) <- s1$well
  s$dose <- s1$dose
  names(s$dose) <- s1$well
  s$well <- s1$well
  s <- get.num.AE(s)
  s$timepoint <- s1$timepoint
  if (s$nspikes[1] > 0) {
    s$allb <- lapply(s$spikes, mi.find.bursts, s$parameters$mi.par)
    s$bs <- calc.burst.summary(s)
  }

  s <- calculate.isis(s)
  s$well.stats <- IGM.compute.mean.firingrate.by.well(s)
  s
}
