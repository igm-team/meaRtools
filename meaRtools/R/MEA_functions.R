
get.data <- function(caption="") {

  # get the directory containing the .spikelist files
  spikeFiles <- sort(tk_choose.files(caption = caption))

  return(spikeFiles)
}

get.num.AE <- function(s2) {
  # add number of active electrodes
  s2$nAE <- rep(0, length(s2$well))
  names(s2$nAE) <- s2$well
  for (i in 1:s2$NCells) {
    s2$nAE[which(substr(s2$channels[i], 1, 2) == (s2$well))] =
    s2$nAE[which(substr(s2$channels[i], 1, 2) == (s2$well))] + 1

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
  filename <- s$file # paste0(s$file, ".edited")
  s2 <- .construct_s(s$spikes, ids, s$rates$time_interval, beg, end,
    corr_breaks, layout, filename)
  s2
}

# this function returns a list with the chemical names for corresponding
# date, plate number and wells found in "file"
get.experimental.log.file <- function(file, masterChemFile=masterChemFile) {
  masterChem = read.csv(masterChemFile)
  masterCD <- as.data.frame(masterChem)
  # remove extraneous NA columns
  masterCD <- masterCD[, 1:9]

  # reconstruct file names
  temp1 <- paste(masterCD$Project, masterCD$Experiment.Date, masterCD$Plate.SN, sep = "_")

  # add a new column called masterCD$filenames
  masterCD$filenames <- temp1

  # ensure log file is ordered correctly so it can be read in correctly
  masterCD <- masterCD[order(masterCD$Experiment.Date, masterCD$Plate.SN, masterCD$Well), ]

  # ****match wells to chemicals *****
  shortFileName <- paste(strsplit(basename(file), "_")[[1]][1],
    strsplit(basename(file), "_")[[1]][2],
    strsplit(basename(file), "_")[[1]][3], sep = "_")

  plate_chem_info <- list()
  count = 1;
  matchedFileName = 0;
  for (i in which(shortFileName == masterCD$filename)) {
    matchedFileName = 1;
    # get all info from chem list
    plate_chem_info$well[count] <- paste(masterCD$Well[i])
    plate_chem_info$treatment[count] <- paste(masterCD$Treatment[i])
    plate_chem_info$size[count] <- paste(masterCD$Size[i])
    plate_chem_info$dose[count] <- paste(masterCD$Dose[i])
    plate_chem_info$units[count] <- paste(masterCD$Units[i])
    count = count + 1

  } # end of for loop through masterCD$file
  if (matchedFileName == 0) {
    print(paste("File ", shortFileName, " was not found in the possible file names
                  constructed from exp log file:", unique(masterCD$filename), sep = ""))
  }
  if (!is.element(length(plate_chem_info$well), c(12, 48))) {
    print(paste("Info exists for ", length(plate_chem_info$well),
      " wells; Some wells have no data.", sep = ""))
  }
  plate_chem_info
}

# purpose: given a list containing spikes and s$bs containing burst info,
# plots the resp=response variable, by channel, in a lattice grid grouped by well
# output=a plot handle, p
# input: spikes and respsonse variable
# EXAMPLE:    p<-.channel_plot_by_well(s,resp="meanfiringrate")
# EXAMPE: list nested in list: p<-.channel_plot_by_well(s,resp="bs$mean_dur")
.channel_plot_by_well <- function(s , resp, resp_label) {
  par(mfrow = c(1, 1))
  if (length(s$well) <= 12){
    well.layout = c(4, 3)
    well.names <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    treatment_size <- paste(c(s$treatment[9:12], s$treatment[5:8], s$treatment[1:4]),
      c(s$size[9:12], s$size[5:8], s$size[1:4]), sep = " ")
    names(well.names) <- paste(paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
      treatment_size, sep = "=")
    # names(well.names) <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    par.strip = list(cex = 1)

  } else {
    well.layout = c(8, 6)
    well.names <- paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = "")
    treatment_size <- paste(c(s$treatment[41:48], s$treatment[33:40], s$treatment[25:32],
      s$treatment[17:24], s$treatment[9:16], s$treatment[1:8]),
    c(s$size[41:48], s$size[33:40], s$size[25:32],
      s$size[17:24], s$size[9:16], s$size[1:8]), sep = " ")
    names(well.names) <- paste(paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
      treatment_size, sep = "=")
    par.strip = list(cex = .6)

  }

  s$active_wells <- .axion.elec_to_well(s$channels)


  if (length(strsplit(resp, "$", fixed = TRUE)[[1]]) > 1){
    response <- get(strsplit(resp, "$", fixed = TRUE)[[1]][2] , get(strsplit(resp, "$", fixed = TRUE)[[1]][1], s))
  } else {
    response <- get(strsplit(resp, "$", fixed = TRUE)[[1]][1], s)
  }

  p <- xyplot(response ~ factor(channels) |
    factor(active_wells, labels = names(well.names), levels = well.names),
    data = s, drop.unused.levels = FALSE, layout = well.layout,
    xlab = "Channels within well",
    ylab = paste(resp_label, sep = ""), pch = 20 ,
    main = paste(paste(resp_label, " by Channels within Wells", sep = ""),
      paste("file= ", strsplit(basename(s$file), ".RData")[[1]][1], sep = ""),
      sep = "\n") ,
    scales = list(x = list(relation = "free",
      draw = FALSE)),
    par.strip.text = par.strip)


  print(p)
  p
}

# ********average and sum burst variables across each well
# input: s is a list containing burst info, and meta data
## purpose: average across wells
## the list returned, (masterSum[[1]],masterSum[[2]]..etc for each spike list s[[i]])
## has meta data and has been filtered according to weather it's 48 or 12 well
# necessary: the timepoint "00" is needed to set which wells are active etc
.get_mean_burst_info_per_well <- function(s) {
  masterSum <- list() # summary over all files
  for (i in 1:length(s)) {
    sum = list() # summary for each timepoint
    # calculate bursting variables for current data File
    nbursts <- sapply(s[[i]]$allb, nrow)
    allb <- s[[i]]$allb
    tempsum <- calc_burst_summary(s[[i]])

    # ISIs: gets the ISI for each channel of s[[i]]
    ISIs = .calc_all_isi(s[[i]], allb)

    # IBIs get IBI's across all inter burst intervals across all data
    tempIBIs <- .calc_all_ibi(s[[i]], allb)

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
        # Total recorded time on current well= recording time * nAE
        sum$duration[j] <- length(incurrentwell) * (s[[i]]$rec_time[2] - s[[i]]$rec_time[1])

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
        # get_burst_info(allb[icurrentwell],"durn") takes out the column "durn" of all
        # matricies allb among the indicator set icurrentwell
        # get duration data across all channels of current well
        sum$mean_dur[j] <- mean(unlist(get_burst_info(allb[icurrentwell], "durn")), na.rm = TRUE)

        # sd of current well burst durations
        sum$sd_dur[j] <- sd(unlist(get_burst_info(allb[icurrentwell], "durn")))

        # mean frequency within a burst
        sum$mean_freq_in_burst[j] <-
        mean(unlist(get_burst_info(allb[incurrentwell], "len")) /
          unlist(get_burst_info(allb[incurrentwell], "durn")), na.rm = TRUE)

        # sd frequency within a burst
        sum$sd_freq_in_burst[j] <- sd(unlist(get_burst_info(allb[incurrentwell], "len")) /
          unlist(get_burst_info(allb[incurrentwell], "durn")), na.rm = TRUE)

        # mean of ISI across all channels in current well
        sum$mean_ISIs[j] = mean(unlist(ISIs[incurrentwell]), na.rm = TRUE)

        # finds sd of ISI across all channels in current well
        sum$sd_ISIs[j] = sd(unlist(ISIs[incurrentwell]), na.rm = TRUE)

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
        sum$mean_IBIs[j] <- round(mean(unlist(tempIBIs[incurrentwell]), na.rm = TRUE), 3)
        # sd IBI
        sum$sd_IBIs[j] <- round(sd(unlist(tempIBIs[incurrentwell]), na.rm = TRUE), 3)
        # cv IBI
        sum$cv_IBIs[j] <- round(sum$mean_IBIs[j] / sum$sd_IBIs[j], 3)

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
        sum$mean_ISIs[j] = NA
        sum$sd_ISIs[j] = NA
        sum$mean_spikes_in_burst[j] <- NA
        sum$sd_spikes_in_burst[j] <- NA
        sum$total_spikes_in_burst[j] <- NA
        sum$per_spikes_in_burst[j] <- NA
        sum$mean_IBIs[j] <- NA
        sum$sd_IBIs[j] <- NA
        sum$cv_IBIs[j] <- NA

      }

    }

    ### Set all names
    for (k in 1:length(names(sum))) {
      names(sum[[k]]) = s[[i]]$goodwells
    }

    # make a masterSum, that is a list of all the summaries
    goodwellindex <- which(is.element(s[[i]]$well, s[[i]]$goodwells))

    masterSum[[i]] <- sum
    masterSum[[i]]$file <- strsplit(basename(s[[i]]$file), ".RData")[[1]][1]
    masterSum[[i]]$treatment <- s[[i]]$treatment[goodwellindex]
    masterSum[[i]]$size = s[[i]]$size[goodwellindex]
    masterSum[[i]]$dose = s[[i]]$dose[goodwellindex]
    masterSum[[i]]$well <- s[[i]]$well[goodwellindex]
    masterSum[[i]]$nAE <- s[[i]]$nAE[goodwellindex]
    masterSum[[i]]$timepoint = rep(s[[i]]$timepoint[1], length(s[[i]]$goodwells))
    masterSum[[i]]$start.rec_time <- rep(s[[i]]$rec_time[1], length(s[[i]]$goodwells))
    masterSum[[i]]$end.rec_time <- rep(s[[i]]$rec_time[2], length(s[[i]]$goodwells))
    masterSum[[i]]$goodwells <- s[[i]]$goodwells

  }

  masterSum
}
