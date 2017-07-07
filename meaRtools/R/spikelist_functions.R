load.spikelist <- function(spkDataFile) {
  temp <- load(spkDataFile) # temp contains objects in loaded workspace
  data <- get(temp)
}

# remake this function so that it can read .RData objects
.Robject.read.spikes <- function(spkDataFile,
  ids = NULL,
  time_interval = 1,
  beg = NULL,
  end = NULL, corr_breaks) {

  temp <- load(spkDataFile) # temp contains objects in loaded workspace
  data <- get(temp)
  spikes <- data$spikes

  arrayinfo <- .get.array.info(data)
  layout <- arrayinfo$layout
  if (missing(corr_breaks)) {
    corr_breaks <- arrayinfo$corr_breaks
  }
  s <- .construct_s(spikes, ids, time_interval, beg, end, corr_breaks,
    layout, filename = spkDataFile)
  s$dose <- data$dose
  s$treatment <- data$treatment
  s$size <- data$size
  s$units <- data$units
  s$well <- data$well
  s <- get.num.AE(s)
  s
}

calculate.spike.features <- function(r_object_files, parameters) {
  r_object_files <- sort(r_object_files)
  s <- list()
  count <- 0
  for (i in 1:length(r_object_files)) {
    timepoint <- substring(basename(r_object_files[i]),
                           nchar(basename(r_object_files[i])) - 8,
                           nchar(basename(r_object_files[i])) - 7)
    current <- .filter.spikes.Robject(r_object_files[i],
      elec_min_rate = parameters$elec_min_rate,
      elec_max_rate = parameters$elec_max_rate,
      well_min_rate = parameters$well_min_rate)
    current$parameters <- parameters
    current$timepoint <- timepoint
    if (length(current$nspikes) > 0) {
      count <- count + 1
      s[[count]] <- current
    }
  }
  s
}

calculate.burst.features <- function(s) {
  for (i in 1:length(s)) {
    current <- s[[i]]
    if (length(current$nspikes) > 0) {
      if (current$parameters$burst.type == "ps"){
        current$allb <- lapply(current$spikes, si.find.bursts,
                               s.min = current$parameters$s.min)
        current$bs <- calc.burst.summary(current)
        current$bs$burst.type <- "ps"
      } else {
        current$allb <-
          lapply(current$spikes, mi.find.bursts, current$parameters$mi.par)
        current$bs <- calc.burst.summary(current)
        current$bs$burst.type <- "mi"
      }
      s[[i]] <- current
    }
  }
  s
}

.filter.spikes.Robject <- function(r_object_files,
                                   elec_min_rate = (1 / 60),
                                   elec_max_rate = 25,
                                   well_min_rate = 4) {
  for (i in 1:length(r_object_files)) {
    if (!(i == 1)) {
      rm(s1, s2)
    }
    s1 <- load.spikelist(r_object_files[i])
    if (class(s1) != "spike.list") {
      s1 <- .Robject.read.spikes(r_object_files[i])
    }
    low <- which(s1$meanfiringrate < elec_min_rate)
    high <- which(s1$meanfiringrate > elec_max_rate)
    extremes <- c(low, high)
    bad_ids <- names(extremes)
    bad_ids <- c("-", bad_ids)
    s2 <- remove_spikes(s1, bad_ids)
    s2$treatment <- s1$treatment
    s2$size <- s1$size
    s2$units <- s1$units
    s2$dose <- s1$dose
    s2$well <- s1$well
    s2 <- get.num.AE(s2)
    low <- which(s2$nAE < well_min_rate)
    bad_wells <- names(low)
    bad_wells <- c("-", bad_wells)
    s <- remove_spikes(s2, bad_wells)
    s$treatment <- s1$treatment
    names(s$treatment) <- s1$well
    # remove from good wells analysis any wells without
    #treatment and below min required nAE
    s$goodwells <-
    names(which(s2$nAE >= well_min_rate))[names(which(s2$nAE >= well_min_rate))
    %in% names(s$treatment[!is.na(s$treatment) & (s$treatment != "")])]
    s$size <- s1$size
    names(s$size) <- s1$well
    s$units <- s1$units
    names(s$units) <- s1$well
    s$dose <- s1$dose
    names(s$dose) <- s1$well
    s$well <- s1$well
    s <- get.num.AE(s)
  }
  s
}

.spk_list_2_list <- function(file) {
  data_raw <- read.csv(file, header = T,
                       colClasses = c("NULL", "NULL", NA, NA, NA))

  # remove rows beyond end of spike data
  last_index <- which(data_raw[, 1] == "", arr.ind = TRUE)[1]
  if (!is.na(last_index)) {
    data_raw <- data_raw[1:last_index - 1, ]
  }

  data_raw$Electrode <- factor(data_raw$Electrode)
  data_raw$Time..s. <- as.numeric(as.character(data_raw$Time..s.))

  # remove NA
  ind_want <- which(!is.na(data_raw[, 1]))

  if (length(ind_want) > 0){
    data_raw2 <- data.frame(
      elect <- data_raw[ind_want, "Electrode"],
      timestamps <- data_raw[ind_want, "Time..s."]
    )

    data_raw2 <- data_raw2[order(data_raw2$elect), ]
    spikes <- split(data_raw2$timestamps, data_raw2$elect, drop = T)
  } else {
    spikes <- NULL
  }

  spikes
}

.spk_list_to_r_object <- function(spikes, chem_info, r_object_file) {
  r_object_file <- path.expand(r_object_file)
  if (file.exists(r_object_file))
  unlink(r_object_file)
  nspikes <- sapply(spikes, length)
  channels <- names(spikes)
  well <- chem_info$well
  treatment <- chem_info$treatment
  size <- chem_info$size
  dose <- chem_info$dose
  units <- chem_info$units
  wells <- .axion.guess.well.number(channels)
  array <- sprintf("Axion %d well", wells)
  plateinfo <- .plateinfo(array)
  epos <- .axion.elec.name.to.xy(channels, plateinfo)

  s <- list()
  s$spikes <- spikes
  s$sCount <- nspikes
  s$epos <- epos
  s$names <- channels
  s$array <- array
  s$treatment <- as.array(treatment)
  s$dose <- as.array(dose)
  s$size <- as.array(size)
  s$well <- as.array(well)
  s$units <- as.array(units)
  print(names(s))
  s
}

read_spikelist <- function(key, spk_list_file, chem_info, r_object_dir) {
  # function to convert spike list Robject
  # remove _spike_list
  key <- unlist(strsplit(key, split = "_spike_list"))

  r_object_file <- gsub("\\(|\\)", "_", sprintf("%s/%s", r_object_dir, key))
  r_object_file <- paste0(
    paste(strsplit(basename(r_object_file),
    split = "_")[[1]][1:4], collapse = "_"), ".RData")

  # f is a list of all files
  f <- spk_list_file

  # get spikes
  spikes_sep <- lapply(f, .spk_list_2_list)
  short_filenames <- gsub("_spike_list.csv", "", basename(f))

  summary.table <- t(sapply(spikes_sep, .axion.spikesum2))
  rownames(summary.table) <- short_filenames
  ma <- do.call("rbind", lapply(spikes_sep, .axion.spikestodf))
  # s2 is a list with all the channels and spikes under each channel
  s2 <- split(ma$time, ma$elec)
  numelec <- length(s2)
  total_spikes <- sum(sapply(s2, length))
  time_ranges <- sapply(s2, range)
  time_min <- min(time_ranges[1, ])
  time_max <- max(time_ranges[2, ])
  # printf formats the text and variables to output ready formats
  # cat contatenates files and then prints them
  cat(sprintf("Total number of spikes: %d\n", total_spikes))
  cat(sprintf("Unique number of electrodes: %d\n", numelec))
  cat(sprintf("Time range [%.3f %.3f] (seconds)\n", time_min,
    time_max))
  print(summary.table)
  S <- .spk_list_to_r_object(s2, chem_info, r_object_file)
  save_file <- paste0(r_object_dir, "/", r_object_file)
  save(S, file = save_file)
  save_file
}
