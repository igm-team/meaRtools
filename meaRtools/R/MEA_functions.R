
get_data <- function(caption="") {

  # get the directory containing the .spikelist files
  spike_files <- sort(tk_choose.files(caption = caption))

  return(spike_files)
}

get_num_ae <- function(s2) {
  # add number of active electrodes
  s2$nae <- rep(0, length(s2$well))
  names(s2$nae) <- s2$well
  for (i in 1:s2$NCells) {
    s2$nae[which(substr(s2$channels[i], 1, 2) == (s2$well))] <-
    s2$nae[which(substr(s2$channels[i], 1, 2) == (s2$well))] + 1

    s2$cw[i] <- substr(s2$channels[i], 1, 2)
  }
  s2
}

# this function removes bad channels
remove_spikes <- function(s, ids) {
  ## ids=vector of indicies eg c(1,2,4,5)
  ## Remove spikes listed in IDS from S data structure, and return
  ## new structure.

  beg <- s$rec_time[1]
  end <- s$rec_time[2]
  corr_breaks <- 0 # TODO: hardcoded for axion!
  layout <- s$layout
  filename <- s$file
  s2 <- .construct_s(s$spikes, ids, s$rates$time_interval, beg, end,
    corr_breaks, layout, filename)
  s2
}

# this function returns a list with the chemical names for corresponding
# date, plate number and wells found in "file"
get_experimental_log_file <- function(file, master_chem_file=master_chem_file) {
  master_chem <- read.csv(master_chem_file)
  master_cd <- as.data.frame(master_chem)
  # remove extraneous NA columns
  master_cd <- master_cd[, 1:9]

  # reconstruct file names
  temp1 <-
    paste(master_cd$Project, master_cd$Experiment.Date,
          master_cd$Plate.SN, sep = "_")

  # add a new column called master_cd$filenames
  master_cd$filenames <- temp1

  # ensure log file is ordered correctly so it can be read in correctly
  master_cd <-
    master_cd[order(master_cd$Experiment.Date,
                   master_cd$Plate.SN, master_cd$Well), ]

  # ****match wells to chemicals *****
  short_file_name <- paste(strsplit(basename(file), "_")[[1]][1],
    strsplit(basename(file), "_")[[1]][2],
    strsplit(basename(file), "_")[[1]][3], sep = "_")

  plate_chem_info <- list()
  count <- 1
  matched_file_name <- 0
  for (i in which(short_file_name == master_cd$filename)) {
    matched_file_name <- 1
    # get all info from chem list
    plate_chem_info$well[count] <- paste(master_cd$Well[i])
    plate_chem_info$treatment[count] <- paste(master_cd$Treatment[i])
    plate_chem_info$size[count] <- paste(master_cd$Size[i])
    plate_chem_info$dose[count] <- paste(master_cd$Dose[i])
    plate_chem_info$units[count] <- paste(master_cd$Units[i])
    count <- count + 1

  } # end of for loop through master_cd$file
  if (matched_file_name == 0) {
    print(paste("File ", short_file_name,
                  " was not found in the possible file names
                  constructed from exp log file:",
                unique(master_cd$filename), sep = ""))
  }
  if (!is.element(length(plate_chem_info$well), c(12, 48))) {
    print(paste("Info exists for ", length(plate_chem_info$well),
      " wells; Some wells have no data.", sep = ""))
  }
  plate_chem_info
}

# purpose: given a list containing spikes and s$bs containing burst info,
# plots the resp=response variable, by channel,
# in a lattice grid grouped by well
# output=a plot handle, p
# input: spikes and respsonse variable
# EXAMPLE:    p<-.channel_plot_by_well(s,resp="meanfiringrate")
# EXAMPE: list nested in list: p<-.channel_plot_by_well(s,resp="bs$mean_dur")
.channel_plot_by_well <- function(s, resp, resp_label) {
  par(mfrow = c(1, 1))
  if (length(s$well) <= 12){
    well_layout <- c(4, 3)
    well_names <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    treatment_size <-
      paste(c(s$treatment[9:12], s$treatment[5:8], s$treatment[1:4]),
      c(s$size[9:12], s$size[5:8], s$size[1:4]), sep = " ")
    names(well_names) <-
      paste(paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
      treatment_size, sep = "=")
    par_strip <- list(cex = 1)

  } else {
    well_layout <- c(8, 6)
    well_names <- paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = "")
    treatment_size <-
      paste(c(s$treatment[41:48], s$treatment[33:40], s$treatment[25:32],
      s$treatment[17:24], s$treatment[9:16], s$treatment[1:8]), sep = " ")
    names(well_names) <-
      paste(paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
      treatment_size, sep = "=")
    par_strip <- list(cex = .6)

  }

  s$active_wells <- .axion_elec_to_well(s$channels)


  if (length(strsplit(resp, "$", fixed = TRUE)[[1]]) > 1){
    response <- get(strsplit(resp, "$", fixed = TRUE)[[1]][2],
                    get(strsplit(resp, "$", fixed = TRUE)[[1]][1], s))
  } else {
    response <- get(strsplit(resp, "$", fixed = TRUE)[[1]][1], s)
  }
  p <- xyplot(response ~ factor(channels) |
    factor(active_wells, labels = names(well_names), levels = well_names),
    data = s, drop.unused.levels = FALSE, layout = well_layout,
    xlab = "Channels within well",
    ylab = paste(resp_label, sep = ""), pch = 20,
    main = paste(paste(resp_label, " by Channels within Wells", sep = ""),
      paste("file= ", strsplit(basename(s$file), ".RData")[[1]][1], sep = ""),
      sep = "\n"),
    scales = list(x = list(relation = "free",
      draw = FALSE)),
    par.strip.text = par_strip)


  print(p)
  p
}

# ********average and sum burst variables across each well
# input: s is a list containing burst info, and meta data
## purpose: average across wells
## the list returned, (master_sum[[1]],master_sum[[2]]..etc
## for each spike list s[[i]])
## has meta data and has been filtered according to weather it's 48 or 12 well
# necessary: the timepoint "00" is needed to set which wells are active etc
.get_mean_burst_info_per_well <- function(s) {
  master_sum <- list() # summary over all files
  for (i in 1:length(s)) {
    sum <- list() # summary for each timepoint
    # calculate bursting variables for current data File
    nbursts <- sapply(s[[i]]$allb, nrow)
    allb <- s[[i]]$allb
    tempsum <- calc_burst_summary(s[[i]])

    # isis: gets the ISI for each channel of s[[i]]
    isis <- .calc_all_isi(s[[i]], allb)

    # IBIs get IBI's across all inter burst intervals across all data
    temp_ibis <- .calc_all_ibi(s[[i]], allb)

    # loop through goodwells
    for (j in 1:length(s[[i]]$goodwells)) {
      # indicator of current well
      icurrentwell <- (s[[i]]$goodwells[j] == s[[i]]$cw)

      # index of current well
      incurrentwell <- which(s[[i]]$goodwells[j] == s[[i]]$cw)

      if (sum(icurrentwell) != 0){
        ##### variables that need summing and averaging
        # total spikes across all AE in current well
        sum$nspikes[j] <- sum(tempsum$spikes[icurrentwell], na.rm = TRUE)
        sum$nAB[j] <- length(which(nbursts[incurrentwell] > 0))
        # Total recorded time on current well= recording time * nae
        sum$duration[j] <-
          length(incurrentwell) * (s[[i]]$rec_time[2] - s[[i]]$rec_time[1])

        # mean duration
        sum$mean_dur[j] <- mean(tempsum$mean_dur[incurrentwell], na.rm = TRUE)

        # mean spikes per second
        sum$mean_freq[j] <- mean(tempsum$mean_freq[incurrentwell], na.rm = TRUE)
        # total number of bursts
        sum$nbursts[j] <- sum(tempsum$nbursts[icurrentwell], na.rm = TRUE)
        # mean burst per second
        sum$bursts_per_sec[j] <- mean(tempsum$bursts_per_sec[incurrentwell])
        # mean burst per minute
        sum$bursts_per_min[j] <- sum$bursts_per_sec[j] * 60

        # finds the mean of duration for a particular well (across all channels)
        # get_burst_info(allb[icurrentwell],"durn")
        # takes out the column "durn" of all
        # matricies allb among the indicator set icurrentwell
        # get duration data across all channels of current well
        sum$mean_dur[j] <-
          mean(unlist(get_burst_info(allb[icurrentwell], "durn")), na.rm = TRUE)

        # sd of current well burst durations
        sum$sd_dur[j] <- sd(unlist(get_burst_info(allb[icurrentwell], "durn")))

        # mean frequency within a burst
        sum$mean_freq_in_burst[j] <-
        mean(unlist(get_burst_info(allb[incurrentwell], "len")) /
          unlist(get_burst_info(allb[incurrentwell], "durn")), na.rm = TRUE)

        # sd frequency within a burst
        sum$sd_freq_in_burst[j] <-
          sd(unlist(get_burst_info(allb[incurrentwell], "len")) /
          unlist(get_burst_info(allb[incurrentwell], "durn")), na.rm = TRUE)

        # mean of ISI across all channels in current well
        sum$mean_isis[j] <- mean(unlist(isis[incurrentwell]), na.rm = TRUE)

        # finds sd of ISI across all channels in current well
        sum$sd_isis[j] <- sd(unlist(isis[incurrentwell]), na.rm = TRUE)

        # len=#spikes in burst (length of burst in bursts)
        # mean_spikes_in_burst
        ns <- unlist(get_burst_info(allb[icurrentwell], "len"))
        sum$mean_spikes_in_burst[j] <- round(mean(ns, na.rm = TRUE), 3)

        # sd of spikes in burst
        sum$sd_spikes_in_burst[j] <- round(sd(ns, na.rm = TRUE), 3)

        # total number of spikes arcross all bursts
        sum$total_spikes_in_burst[j] <- sum(ns, na.rm = TRUE)

        # percent of spikes in bursts
        sum$per_spikes_in_burst[j] <-
        round(100 * (sum$total_spikes_in_burst[j] / sum$nspikes[j]), 3)

        # mean IBI
        sum$mean_ibis[j] <-
          round(mean(unlist(temp_ibis[incurrentwell]), na.rm = TRUE), 3)
        # sd IBI
        sum$sd_ibis[j] <-
          round(sd(unlist(temp_ibis[incurrentwell]), na.rm = TRUE), 3)
        # cv IBI
        sum$cv_ibis[j] <- round(sum$mean_ibis[j] / sum$sd_ibis[j], 3)

      } else {
        sum$nspikes[j] <- NA
        sum$nAB[j] <- NA
        sum$duration[j] <- NA
        sum$mean_dur[j] <- NA
        sum$mean_freq[j] <- NA
        sum$nbursts[j] <- NA
        sum$bursts_per_sec[j] <- NA
        sum$bursts_per_min[j] <- NA
        sum$mean_dur[j] <- NA
        sum$sd_dur[j] <- NA
        sum$mean_freq_in_burst[j] <- NA
        sum$sd_freq_in_burst[j] <- NA
        sum$mean_isis[j] <- NA
        sum$sd_isis[j] <- NA
        sum$mean_spikes_in_burst[j] <- NA
        sum$sd_spikes_in_burst[j] <- NA
        sum$total_spikes_in_burst[j] <- NA
        sum$per_spikes_in_burst[j] <- NA
        sum$mean_ibis[j] <- NA
        sum$sd_ibis[j] <- NA
        sum$cv_ibis[j] <- NA

      }

    }

    ### Set all names
    for (k in 1:length(names(sum))) {
      names(sum[[k]]) <- s[[i]]$goodwells
    }

    # make a master_sum, that is a list of all the summaries
    goodwellindex <- which(is.element(s[[i]]$well, s[[i]]$goodwells))

    master_sum[[i]] <- sum
    master_sum[[i]]$file <- strsplit(basename(s[[i]]$file), ".RData")[[1]][1]
    master_sum[[i]]$treatment <- s[[i]]$treatment[goodwellindex]
    master_sum[[i]]$size <- s[[i]]$size[goodwellindex]
    master_sum[[i]]$dose <- s[[i]]$dose[goodwellindex]
    master_sum[[i]]$well <- s[[i]]$well[goodwellindex]
    master_sum[[i]]$nae <- s[[i]]$nae[goodwellindex]
    master_sum[[i]]$timepoint <-
      rep(s[[i]]$timepoint[1], length(s[[i]]$goodwells))
    master_sum[[i]]$start_rec_time <-
      rep(s[[i]]$rec_time[1], length(s[[i]]$goodwells))
    master_sum[[i]]$end_rec_time <-
      rep(s[[i]]$rec_time[2], length(s[[i]]$goodwells))
    master_sum[[i]]$goodwells <- s[[i]]$goodwells

  }

  master_sum
}
