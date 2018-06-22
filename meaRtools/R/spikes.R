.construct_s <- function(spikes, ids, time_interval, beg, end, corr_breaks,
  layout, filename) {

  spikes_range <- range(unlist(spikes))

  if (is.null(end)) {
    end <- spikes_range[2]
  } else {
    spikes <- lapply(spikes,
      function(x, max) {
        ## Any times greater than MAX are removed.
        x_high <- which(x > max)
        if (any(x_high))
          x[1:x_high[1] - 1]
        else
         x
        },
      max = end)
  }

  if (is.null(beg)) {
    beg <- spikes_range[1]
  } else {
    spikes <- lapply(spikes,
      function(x, min) {
        ## Any times less than MIN are removed.
        x_low <- which(x < min)
        if (any(x_low))
           x <- x[- x_low]
        x
      },
      min = beg)
  }

  ## Remove any channels that have zero spikes (which can happen if
  ## the beg, end range is too narrow, or if a datafile is empty,
  ## which happens sometime for the Feller data.
  empty_index <- which(sapply(spikes, length) == 0)
  if (any(empty_index)) {
    spikes <- spikes[- empty_index]
  }

  if (!is.null(ids)) {
    spikes <- .filter_channel_names(spikes, ids)
  }

  ## No more spike trains should be removed, so we can refilter
  ## the layout to ensure
  ## that only spikes that are left in the array are kept in the layout.
  channels <- names(spikes)
  keep <- match(channels, rownames(layout$pos))
  layout$pos <- layout$pos[keep, ]
  rec_time <- c(beg, end)
  nspikes <- sapply(spikes, length)

  if (length(nspikes) == 0) {
    meanfiringrate <- nspikes # empty list
  } else {
    meanfiringrate <- nspikes / (end - beg)
  }

  rates <- .make_spikes_to_frate(spikes, time_interval = time_interval,
    beg = beg, end = end)
  unit_offsets <- NULL
  .check_spikes_monotonic(spikes)
  res <- list(channels = names(spikes), spikes = spikes, nspikes = nspikes,
    NCells = length(spikes), meanfiringrate = meanfiringrate,
    file = filename, layout = layout, rates = rates,
    unit_offsets = unit_offsets,
    rec_time = rec_time)
  class(res) <- "spike.list"
  if (length(corr_breaks) == 1) {
    res$corr <- NULL
  } else {
    res$corr <- .corr_index(res, corr_breaks)
  }
  res
}


calculate_isis <- function(s) {
  s$isis <- lapply(s$spikes, diff)
  s$mean_isis <- lapply(s$isis, mean)
  s$sd_isis <- lapply(s$isis, sd)
  return(s)
}

.plot_isis_by_plate <- function(s) {
  isis_all <- unlist(s$isis)
  hist(isis_all, main = "Histogram of ISIs by Plate", xlab = "ISI length")
  hist(log10(isis_all), main = "Histogram of log(ISIs) by Plate",
       xlab = "log10(ISI length)")
}
.spike_summary_by_electrode <- function(s) {
  s <- calculate_isis(s)
  electrodes <- .get_all_electrodes(s)
  sum <- matrix(data = NA, nrow = length(electrodes), ncol = 4)
  colnames(sum) <- c("nspikes", "meanfiringrate", "meanisis", "sdisis")
  rownames(sum) <- electrodes

  df <- cbind(s$nspikes, s$meanfiringrate, s$mean_isis, s$sd_isis)
  active_electrodes <- rownames(df)
  for (i in active_electrodes) {
    sum[i, ] <- unlist(df[i, ])
  }
  sum
}

.spike_summary_by_well <- function(s) {
  plate <- get_plateinfo(s$layout$array)
  wells <- sort(plate$wells)
  s$isis <- lapply(s$spikes, diff)
  start_pos <- 1
  sum <- matrix(data = NA, nrow = length(wells), ncol = start_pos + 11)
  colnames(sum) <- c("treatment", "nae", "nspikes_by_well",
                     "meanfiringrate_by_well",
                     "meanfiringrate_by_all_electrodes",
                     "meanfiringrate_by_active_electordes",
                     "sdfiringrate_by_active_electordes",
                     "meanisis", "sdisis","mutual_info","entropy","STTC")
  rownames(sum) <- wells
  nelectrodes <- plate$n_elec_r * plate$n_elec_c
  if (!is.null(s$goodwells)) {
    for (j in 1:length(s$goodwells)) {
      icurrentwell <- (s$goodwells[j] == s$cw)
      incurrentwell <- which(icurrentwell)

      treatment <- s$treatment[s$goodwells[j]]

      if (length(incurrentwell) > 0) {
        well <- strsplit(s$channels[incurrentwell], "_")[[1]][1]
        treatment <- s$treatment[well][[1]]
      }
      sum[s$goodwells[j], start_pos] <- treatment
      sum[s$goodwells[j], start_pos + 1] <- length(incurrentwell)
      sum[s$goodwells[j], start_pos + 2] <- sum(s$nspikes[icurrentwell])
      sum[s$goodwells[j], start_pos + 3] <- sum(s$meanfiringrate[icurrentwell])
      sum[s$goodwells[j], start_pos + 4] <-
        sum(s$meanfiringrate[icurrentwell]) / nelectrodes

      sum[s$goodwells[j], start_pos + 5] <- mean(s$meanfiringrate[icurrentwell])
      sum[s$goodwells[j], start_pos + 6] <- sd(s$meanfiringrate[icurrentwell])

      isis_all <- unlist(s$isis[icurrentwell])
      sum[s$goodwells[j], start_pos + 7] <- mean(isis_all)
      sum[s$goodwells[j], start_pos + 8] <- sd(isis_all)
      sum[s$goodwells[j], start_pos + 9] <- Reduce(c,s$mutual_inf)[well]
      sum[s$goodwells[j], start_pos + 10] <- Reduce(c,s$entropy)[well]
      sum[s$goodwells[j], start_pos + 11] <- unlist(s$mean_sttc)[well]
    }
  }
  sum
}

.get_div <- function(s) {
  div <- NA
  t1 <- strsplit(s$file, split = "_", fixed = TRUE)
  for (i in t1[[1]]) {
    i <- toupper(i)
    if (nchar(i) > 2 && substr(i, 1, 3) == "DIV") {
      if (nchar(i) > 5) {
        i <- unlist(strsplit(i, split = ".", fixed = T))[1]
      }
      div <- as.numeric(substr(i, 4, nchar(i)))
    }
  }
  div
}

compute_mean_firingrate_by_well <- function(s) {
  df1 <- aggregate(s$meanfiringrate, by = list(s$cw), FUN = mean, na.rm = T)
  df2 <- aggregate(s$meanfiringrate, by = list(s$cw), FUN = sum, na.rm = T)

  df <- cbind(df1, df2[, 2], .get_div(s))
  names(df) <- c("well", "meanfiringrate", "meanfiringrate_per_well", "div")
  rownames(df) <- t(df["well"])
  df
}

.plot_isis_by_electrode <- function(s) {
  wells <- unique(s$cw)
  if (length(wells) > 0) {
    for (well in wells) {
      active_electrodes <- which(s$cw == well &
                           as.vector(unlist(lapply(s$isis, length))) > 0)
      if (length(active_electrodes) > 0) {
        df <- list()
        for (i in 1:length(active_electrodes)) {
          df[[i]] <- cbind(s$isis[[active_electrodes[i]]],
            names(s$isis)[active_electrodes[i]])
        }
        df <- do.call("rbind", df)
        colnames(df) <- c("isis", "electrode")
        plateinfo <- get_plateinfo(s$layout$array)
        d1 <- expand.grid(col = 1:plateinfo$n_elec_c,
                          row = 1:plateinfo$n_elec_r)
        all_electrodes <- sort(paste(well, "_",
                              d1[, "row"], d1[, "col"],
                              sep = ""))
        layout_electrodes <- c(plateinfo$n_elec_r, plateinfo$n_elec_c)
        df <- data.frame(df)
        df$isis <- as.numeric(as.vector(df$isis))
        df$electrode <- as.character(as.vector(df$electrode))


        p1 <- histogram(~ isis | factor(electrode, levels = all_electrodes),
          data = df, breaks = 10,
          main = paste("ISIs histogram plot for ", well, sep = ""),
          layout = layout_electrodes,
          drop.unused.levels = FALSE)
        print(p1)

        p2 <- histogram(~ log(isis) | factor(electrode,
                        levels = all_electrodes),
          data = df, breaks = 10,
          main = paste("log(ISIs) histogram plot for ", well, sep = ""),
          layout = layout_electrodes,
          drop.unused.levels = FALSE)
        print(p2)
      }
    }
    p2
  }

}

.plot_mean_firingrate_by_electrode <- function(s) {
  wells <- unique(s$cw)
  if (length(wells) > 0) {
    for (well in wells) {
      active_electrodes <- which(s$cw == well)
      df <- list()
      for (i in active_electrodes) {
        df[[i]] <- cbind(s$rates$times,
          s$rates$rates[, i],
          names(s$nspikes)[i])
      }
      df <- do.call("rbind", df)
      maxy <- max(df[, 2])
      colnames(df) <- c("time", "meanfiringrate", "electrode")
      plateinfo <- get_plateinfo(s$layout$array)
      if (any(grep("^Axion", s$layout$array))) {
        ## TODO: Axion-specific layout of grid of electrodes
        ## This assumes we know the format of the electrode name.
        d1 <- expand.grid(col = 1:plateinfo$n_elec_c,
                          row = 1:plateinfo$n_elec_r)
        all_electrodes <- sort(paste(well, "_", d1[, "row"],
                                     d1[, "col"], sep = ""))
        layout_electrodes <- c(plateinfo$n_elec_r, plateinfo$n_elec_c)
      } else {
        ## For other plates, simply show all active electrodes
        ## without assuming any spatial position of the electrode
        ## within a well.
        all_electrodes <- as.factor(names(s$spikes)[active_electrodes])
        ## The following layout tells lattice to generate the N plots
        ## as best as it can.
        layout_electrodes <- c(0, length(all_electrodes))
      }

      df <- data.frame(df)
      p1 <- xyplot(meanfiringrate ~ time | factor(electrode,
                      levels = all_electrodes),
        data = df,
        main = paste("Mean Firing Rate per Second for Well ",
                well, ". Maximum firing rate:", maxy, " Hz", sep = ""),
        layout = layout_electrodes, type = "h",
        scales = list(
          x = list(draw = FALSE),
          y = list(draw = FALSE)),
        drop.unused.levels = FALSE)
      print(p1)
    }
    p1
  }

}

plot_mean_firingrate_by_eletrode_by_div <- function(s) {
  electrode_stats <- lapply(s, function(d) {
    cbind(d$meanfiringrate, d$cw, .get_div(d))
  })
  electrode_stats_all <- do.call("rbind", electrode_stats)
  electrode_names <- row.names(electrode_stats_all)
  electrode_stats_all <- suppressWarnings(data.frame(cbind(electrode_names,
                                          electrode_stats_all[, 1:3])))
  names(electrode_stats_all) <- c("electrode", "meanfiringrate", "well", "div")
  electrode_stats_all$div <- as.numeric(as.vector(electrode_stats_all$div))
  electrode_stats_all$meanfiringrate <- as.numeric(
             as.vector(electrode_stats_all$meanfiringrate))
  electrode_stats_all$electrode <- as.character(
             as.vector(electrode_stats_all$electrode))


  wells <- unique(electrode_stats_all$well)
  if (length(wells) > 0) {
    for (active_well in wells) {
      df <- electrode_stats_all[which(electrode_stats_all$well ==
                                        active_well), ]
      layout_info <- .get_electrode_layout(s[[1]], active_well)
      maxy <- max(df$meanfiringrate)

      p1 <- xyplot(meanfiringrate ~ div | factor(electrode,
                  levels = layout_info$electrodes),
        data = df,
        main = paste("Mean Firing Rate across DIV's for ",
        active_well, ". Maximum firing rate:", round(maxy, 2), " Hz", sep = ""),
        layout = layout_info$layout,
        drop.unused.levels = FALSE)
      print(p1)
    }
  }
}

plot_mean_firingrate_by_well_by_div <- function(s) {
  well_stats <- lapply(s, function(d) {
    d$well_stats
  })
  well_stats_all <- do.call("rbind", well_stats)
  plateinfo <- get_plateinfo(s[[1]]$layout$array)
  wells <- plateinfo$wells
  names(wells) <- wells # keep the names valid.
  wells_layout <- plateinfo$layout

  p1 <- xyplot(meanfiringrate ~ div | factor(well, levels = wells),
        data = well_stats_all,
        main = "Mean Firing Rate across DIV's (Hz/electrode)",
        layout = wells_layout,
        drop.unused.levels = FALSE)
  print(p1)
  p2 <- xyplot(meanfiringrate_per_well ~ div | factor(well, levels = wells),
        data = well_stats_all,
        main = "Mean Firing Rate across DIV's (Hz/well)",
        layout = wells_layout,
        drop.unused.levels = FALSE)
  print(p2)
}

plot_plate_summary_for_spikes <- function(s, outputdir) {
  for (i in 1:length(s)) {
    basename <- get_file_basename(s[[i]]$file)
    spike_plot_path <- paste(outputdir, "/", basename,
                             "_spike_plot.pdf", sep = "")
    pdf(file = spike_plot_path)

    # layout
    p <- .plot_mealayout(s[[i]]$layout, use_names = T, cex = 0.48)
    title(main = paste(paste("Electrode Layout"),
      paste("file= ", strsplit(basename(s[[i]]$file),
            ".RData")[[1]][1], sep = ""), sep = "\n"))
    # MFR
    p <- .plot_meanfiringrate(s[[i]], main = "Mean Firing Rate by Plate (Hz)")
    p <- .plot_isis_by_plate(s[[i]])
    p <- .channel_plot_by_well(s[[i]], resp = "meanfiringrate",
            resp_label = "Mean Firing Rate (Hz)")
    p <- .plot_mean_firingrate_by_electrode(s[[i]])
    p <- .plot_isis_by_electrode(s[[i]])
    dev.off()
  }
}

write_plate_summary_for_spikes <- function(s, outputdir) {
  csvwell <- paste(outputdir, "/",
            get_project_plate_name(s[[1]]$file),
            "_well_spikes.csv", sep = "")

  for (i in 1:length(s)) {
    div <- .get_div(s[[i]])
    basename <- get_file_basename(s[[i]]$file)
    csvfile <- paste(outputdir, "/", basename, "_spikes.csv", sep = "")
    df <- .spike_summary_by_electrode(s[[i]])
    df2 <- .spike_summary_by_well(s[[i]])

    # recording time
    write.table(paste("recording time (s): [",
      paste(s[[i]]$rec_time[1],
      round(s[[i]]$rec_time[2]), sep = " ,"),
      "]", sep = ""), csvfile, sep = ",", append = FALSE,
      row.names = FALSE, col.names = FALSE)
    write.table(" ", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)
    write.table("Spike statistics for wells", csvfile, sep = ",",
                append = TRUE, row.names = FALSE, col.names = FALSE)
    df2 <- cbind(rownames(df2), df2)
    suppressWarnings(write.table(df2,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
    suppressWarnings(write.table(cbind(df2, div),
      csvwell, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE))

    write.table(" ", csvfile, sep = ",", append = TRUE,
                row.names = FALSE, col.names = FALSE)

    write.table("Spike statistics for electrodes",
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    df <- cbind(rownames(df), df)
    colnames(df)[1] <- "electrode"
    # summary write data
    suppressWarnings(write.table(df,
      csvfile, sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE))
  }
}

.check_spikes_monotonic <- function(spikes) {
  ## Check to see that all spike times are monotonically increasing.
  ## The counting and histogram routines assumes that spike times
  ## are sorted, earliest spikes first.
  results <- sapply(spikes, function(x) {
    any(diff(x) < 0)
  })
  if (any(results)) {
    stop(paste("Spikes are not ordered in increasing time",
      paste(which(results), collapse = " "), "\n"))
  }
}

.make_spikes_to_frate <- function(spikes,
  time_interval=1, # time bin of 1sec.
  frate_min=0,
  frate_max=20,
  clip=FALSE,
  beg=NULL,
  end=NULL
  ) {
  ## Convert the spikes for each cell into a firing rate (in Hz)
  ## We count the number of spikes within time bins of duration
  ## time_interval (measured in seconds).
  ##
  ## Currently cannot specify BEG or END as less than the
  ## range of spike times else you get an error from hist.  The
  ## default anyway is to do all the spikes within a data file.

  ## Note, we need to check for when there are no spikes; this can
  ## happen when examining a subset of spikes, e.g. a well in a multi-well
  ## plate that was not working.
  nspikes <- lapply(spikes, length)
  nelectrodes <- length(nspikes)

  ## if clips is set to TRUE, firing rate is clipped within the
  ## values frate_min and frate_max.  This is problably not needed.

  spikes_range <- range(unlist(spikes))
  if (is.null(beg)) beg <- spikes_range[1]
  if (is.null(end)) end <- spikes_range[2]

  time_breaks <- seq(from = beg, to = end, by = time_interval)
  if (time_breaks[length(time_breaks)] <= end) {
    ## extra time bin needs adding.
    time_breaks <- c(time_breaks,
      time_breaks[length(time_breaks)] + time_interval)
  }
  nbins <- length(time_breaks) - 1
  
  # sjecpp
  rates = frate_counts(spikes, time_breaks[1], time_breaks[nbins], time_interval, nbins)
  ##rates <- matrix(counts, nrow=nbins, ncol=nelectrodes)
  
  ## Check if there are any electrodes to process.
  if (nelectrodes > 0) {
    ## Now optionally set the upper and lower frame rates if clip is TRUE.
    if (clip)
    rates <- pmin(pmax(rates, frate_min), frate_max)

    ## Do the average computation here.
    ## av_rate is average rate across the array.
    av_rate <- apply(rates, 1, mean)
  } else {
    av_rate <- rep(NA, nbins)
  }
  ## We can remove the last "time.break" since it does not correspond
  ## to the start of a time frame.
  res <- list(rates = rates,
    times = time_breaks[- length(time_breaks)],
    av_rate = av_rate,
    time_interval = time_interval)
  res
}

.plot_meanfiringrate <- function(s, beg, end, main=NULL, lwd=0.2, ...) {
  ## Plot the mean firing rate over all the cells at each time step.
  ## Can optionally specify the beginning (BEG) and end (END) time, in
  ## seconds.

  if (missing(beg)) beg <- s$rates$times[1]
  if (missing(end)) end <- s$rates$times[length(s$rates$times)]

  if (is.null(main))
    main <- basename(s$file)

  plot(s$rates$times, s$rates$av_rate, type = "h", xlab = "time (s)",
    xlim = c(beg, end), bty = "n", lwd = lwd,
    ylab = "mean firing rate (Hz)", main = main, ...)
}

.plot_mealayout <- function(x, use_names=TRUE, ...) {
  plateinfo = get_plateinfo(x$array)
  rows <- plateinfo$n_well_r
  columns <- plateinfo$n_well_c
  row_names <- chartr("123456789", "ABCDEFGHI", 1:rows)
  ## Plot the MEA layout.
  pos <- x$pos
  ## remove everything up to and including first underscore
  electrodes_only <- sub('^[^_]+_', '', rownames(x$pos))
  p<-plot(NA, xaxs="i",#asp = 1,xaxs="i",
          #xlim = c(x$xlim[1]-100,x$xlim[2]+200), ylim = c(x$ylim[1],x$ylim[2]+200),
          xlim = x$xlim, ylim = x$ylim,
          bty = "n",
    xlab = "Plate layout", ylab = "", type = "n",xaxt="n",yaxt="n",cex.lab=1.4)
  if (use_names)
    text(pos[, 1], pos[, 2], electrodes_only, ...)
  else
    text(pos[, 1], pos[, 2], ...)
  axis(3,at=seq( x$xlim[1]+x$xlim[2]/(columns*4), x$xlim[2]-x$xlim[2]/(columns*1.5),length.out = columns),labels=c(1:columns),cex.axis=1.4,line=-2,tick = F)
  axis(2,at=seq( x$ylim[2]-x$ylim[2]/(rows*1.5), x$ylim[1]+x$ylim[2]/(rows*3),length.out = rows),labels=chartr("123456789", "ABCDEFGHI", 1:rows),las=1,cex.axis=1.4,tick = F)
  ## Following works only for Axion arrays.
  ## TODO
  ## will be difficult to generalise, so leave it for now.
  ## could solve by adding it to the plateinfo list?
  if (any(grep('^Axion', x$array))) {
    abline(h=seq( max(x$pos[,"y"])+200, x$ylim[1]-200,length.out = rows+1),
           v=seq( x$xlim[2]-100, x$xlim[1]-100,length.out = columns+1),
           col=c("grey"))
  }
}

.summary_spike_list <- function(object, ...) {
  cat(paste("Spike data:", object$file, "\n"))
  cat(paste("NCells", object$NCells, "\n"))
  cat(sprintf("Time (s) [%.3f %.3f]\n", object$rec_time[1], object$rec_time[2]))
}

.print_spike_list <- function(x) {
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
.spike_simulation <- function(s1,
  elec_min_rate= (1 / 60),
  elec_max_rate=25,
  well_min_rate=15) {
  dt <- 1
  current_electrode_rate <- s1$meanfiringrate
  rec_start <- s1$rec_time[1]
  rec_end <- s1$rec_time[2]
  spikes <- list()
  for (electrode in 1:length(s1$spikes)) {
    rate <- current_electrode_rate[electrode] * dt / 1000.0
    timepoints <- seq(rec_start, rec_end, by = 0.001)
    spikes[[electrode]] <- timepoints[which(rate > runif(length(timepoints)))]

  }
  names(spikes) <- names(s1$spikes)
  temp_s <- .construct_s(spikes, NULL, time_interval = 1, beg = NULL,
              end = NULL, corr_breaks = 0,
              s1$layout, filename = s1$file)


  # indices of low and high firing rate
  low <- which(temp_s$meanfiringrate < elec_min_rate)
  high <- which(temp_s$meanfiringrate > elec_max_rate)

  ## TODO, check that low and high are non-zero length vectors.
  extremes <- c(low, high)

  bad_ids <- names(extremes)
  bad_ids <- c("-", bad_ids) # "-" needed to remove these ids!

  s2 <- remove_spikes(temp_s, bad_ids)

  s2$treatment <- s1$treatment
  s2$size <- s1$size
  s2$units <- s1$units
  s2$dose <- s1$dose
  s2$well <- s1$well

  # get_num_ae
  s2 <- get_num_ae(s2)

  # indices of low and high firing rate

  low <- which(s2$nae < well_min_rate)

  bad_wells <- names(low)
  bad_wells <- c("-", bad_wells) # "-" needed to remove these well!
  # just these three for example
  s <- remove_spikes(s2, bad_wells)

  s$goodwells <- names(which(s2$nae >= well_min_rate))

  # [which(s2$nae >= well_min_rate)
  s$treatment <- s1$treatment
  names(s$treatment) <- s1$well
  s$size <- s1$size
  names(s$size) <- s1$well
  s$units <- s1$units
  names(s$units) <- s1$well
  s$dose <- s1$dose
  names(s$dose) <- s1$well
  s$well <- s1$well
  s <- get_num_ae(s)
  s$timepoint <- s1$timepoint
  if (s$nspikes[1] > 0) {
    s$allb <- lapply(s$spikes, mi_find_bursts, s$parameters$mi_par)
    s$bs <- calc_burst_summary(s)
  }

  s <- calculate_isis(s)
  s$well_stats <- compute_mean_firingrate_by_well(s)
  s
}
