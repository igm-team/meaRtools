.graythresh <- function(I) {
  I <- as.vector(I)
  if (min(I) < 0 | max(I) > 1) {
    stop("Data needs to be between 0 and 1")
  }
  I <- I * 256

  num_bins <- 256
  counts <- hist(I, num_bins, plot = FALSE)$counts

  # Variables names are chosen to be similar to the formulas in the Otsu paper.
  p <- counts / sum(counts)
  omega <- cumsum(p)
  mu <- cumsum(p * (1:num_bins))
  mu_t <- mu[length(mu)]

  sigma_b_squared <- (mu_t * omega - mu) ^ 2 / (omega * (1 - omega))
  sigma_b_squared[is.na(sigma_b_squared)] <- 0
  maxval <- max(sigma_b_squared)
  isfinite_maxval <- is.finite(maxval)
  if (isfinite_maxval) {
    idx <- mean(which(sigma_b_squared == maxval))
    # Normalize the threshold to the range [0, 1].
    level <- (idx - 1) / (num_bins - 1)
  } else {
    level <- 0.0
  }
  level
}


# convert the spike list to a matrix, probably not necessary anymore
# but just to be consistent with matlab for now
.nb_prepare <- function(s) {
  spikes <- s$spikes
  colnames <- names(spikes)

  slength <- sapply(spikes, length)
  max_elements <- max(slength)

  data <- matrix(0, length(colnames), max_elements)
  for (i in  1:length(colnames)) {
    data[i, 1:slength[[i]]] <- spikes[[i]]
  }
  rownames(data) <- colnames
  data <- t(data)
  data
}

# generate binned data for the well
.nb_select_and_bin <- function(data, well, sbegin, send, bin) {
  well_data <- data[, grep(well, colnames(data))]
  well_data[well_data < sbegin | well_data > send] <- 0
  if (is.null(dim(well_data))) {
    dim(well_data) <- c(length(well_data), 1)
  }
  if (length(well_data) > 0){
    to_add_back <- min(well_data[well_data > 0])
  } else {
    to_add_back <- 0
  }


  well_data <- well_data[rowSums(well_data) > 0, ]
  well_data <- round(well_data * 12500)
  temp <- well_data[well_data > 0]
  if (length(temp) > 0) {
    t_min <- min(temp)
    t_max <- max(temp)
    well_data <- well_data - t_min + 1
    range <- (t_max - t_min + 1)
    bins <- floor(range / bin)
    if (range %% bin > 0) {
      bins <- bins + 1
    }
    x <- (0:bins) * bin + 1
    y <- c(- Inf, x, max(x) + bin, + Inf)

    if (is.null(dim(well_data))) {
      dim(well_data) <- c(length(well_data), 1)
    }
    well_data_new <- matrix(0, bins + 1, dim(well_data)[2])
    for (i in 1: dim(well_data)[2]) {
      temp <- hist(well_data[, i], breaks = y, plot = FALSE)$counts
      temp <- temp[2:(length(temp) - 1)]
      well_data_new[, i] <- temp
    }
    well_data_new[well_data_new > 1] <- 1
  } else {
    well_data_new <- temp
  }

  list(well_data_new, to_add_back)
}

.nb_gaussian_filter <- function(sigma, well_data, min_electrodes) {
  sigma_input <- sigma
  if (length(well_data) == 0) {
    f2 <- numeric()
  } else {
    # a very flat filter. Consider a much shapper one later
    if (sigma %% 2 == 0) {
      sigma <- sigma + 1
    }
    filter_size <- sigma
    half_size <- (filter_size - 1) / 2
    x <- seq(- half_size, half_size, length = filter_size)
    gauss_filter <- exp(- x ^ 2 / (2 * sigma ^ 2))
    gauss_filter <- gauss_filter / sum(gauss_filter)
    f <- well_data
    if (length(which(apply(well_data, 2, max) > 0)) >= min_electrodes) {
      for (i in 1:dim(f)[2]) {
        if (max(well_data[, i]) > 0) {
          f[, i] <- filter(well_data[, i], gauss_filter, circular = TRUE)
          f[, i] <- f[, i] / max(f[, i])
        }
      }
      f1 <- rowSums(f)
      f1 <- f1 / max(f1)
      f2 <- filter(f1, gauss_filter, circular = TRUE)
      f2 <- f2 / max(f2)
      dim(f2) <- c(length(f2), 1)
      colnames(f2) <- sigma_input
    } else {
      f2 <- numeric()
    }
  }
  f2
}

.nb_get_spike_stats <- function(well_data, timespan) {
  stat <- matrix(0, 1, 3)
  if (length(well_data) > 0) {
    # number of active electrodes
    stat[1] <- length(which(apply(well_data, 2, max) > 0))
    # well level mfr
    stat[2] <- sum(well_data) / timespan
    # mfr by active electrodes
    if (stat[1] > 0) {
      stat[3] <- stat[2] / stat[1]
    }
  }
  colnames(stat) <- c("n.active.electrodes",
                      "mfr.per.well", "mfr.per.active.electode")
  stat
}
.nb_get_burst_stats_intensities <-
  function(well_data, f, timespan, bin_time, min_electrodes,
  sbegin, send, offset2=0, bin_size=0.002) {
  # bin_size is set to 0.002 based on sample rate of 12500/s
  n <- length(f)
  stat <- matrix(NA, 11, n)
  rownames(stat) <- c("mean.NB.time.per.sec",
    "total.spikes.in.all.NBs",
    "percent.of.spikes.in.NBs",
    "spike.intensity",
    "spike.intensity.by.aEs",
    "total.spikes.in.all.NBs,nNB",
    "[total.spikes.in.all.NBs,nNB],nAE",
    "mean[spikes.in.NB,nAE]",
    "mean[spikes.in.NB,nAE,NB.duration]",
    "mean[spikes.in.NB]",
    "total.number.of.NBs")
  colnames(stat) <- rep("NA", n)
  for (current in 1:n) {
    # not all have names
    if (length(f[[current]] > 0)) {
      colnames(stat)[current] <- colnames(f[[current]]) # not all have names
    }
  }
  nb_times <- list()
  length(nb_times) <- length(f)
  names(nb_times) <- names(f);

  for (current  in 1:n) {
    temp <- f[[current]]
    if (length(temp) > 0) {
      n_active_electrodes <- length(which(apply(well_data, 2, max) > 0))
      temp[temp < 0] <- 0
      level <- .graythresh(temp)
      # get timestamps that are identified as part of network bursts
      indicator0 <- temp >= level

      if (min_electrodes > 0) {
        indicator <- c(0, indicator0, 0) # prepare for finding intervals
        diff_indicator <- diff(indicator)
        burst_start <- which(diff_indicator == 1)
        burst_end <- which(diff_indicator == - 1)
        if (length(burst_start) != length(burst_end)) {
          stop("Burst region no match")
        }

        ## filtered regions based on minimum number of active electrodes
        for (region in 1: length(burst_start)) {
          current_region_nae <-
            length(which(colSums(well_data[
              (burst_start[region]):(burst_end[region] - 1), ]) > 0))
          if (current_region_nae < min_electrodes) {
            indicator0[burst_start[region]:(burst_end[region] - 1)] <- 0
          }
        }
      }

      indicator <- c(0, indicator0, 0) # prepare for finding intervals
      diff_indicator <- diff(indicator)
      burst_start <- which(diff_indicator == 1)
      burst_end <- which(diff_indicator == - 1)
      if (length(burst_start) != length(burst_end)) {
        stop("Burst region no match")
      }


      nb_times_temp <- cbind.data.frame(burst_start, burst_end)
      nb_times_temp$start_t <- burst_start * 0.002 + offset2
      nb_times_temp$end_t <- burst_end * 0.002 + offset2
      nb_times[[current]] <- nb_times_temp

      # mean network burst time per second
      stat[1, current] <- sum(indicator0) * bin_time / timespan
      totalspikes <- rowSums(well_data)
      spikes_in_network_bursts <- sum(totalspikes * indicator0)
      stat[2, current] <- spikes_in_network_bursts
      # percentage of spikes in network bursts
      stat[3, current] <- stat[2, current] / sum(totalspikes)

      total_burst_regions <- sum(burst_end - burst_start)
      stat[4, current] <- spikes_in_network_bursts /
                          total_burst_regions # overall Spike Intensity
      stat[5, current] <- stat[4, current] / n_active_electrodes
      stat[6, current] <- spikes_in_network_bursts / length(burst_start)
      stat[7, current] <- stat[6, current] / n_active_electrodes

      burst_region_length <- burst_end - burst_start
      total_spikes_per_active_electrode <- totalspikes / n_active_electrodes
      spikes_in_region_per_active_electrode <- matrix(0, length(burst_start), 2)
      for (region in 1: length(burst_start)) {
        spikes_in_region_per_active_electrode[region, 1] <-
          sum(total_spikes_per_active_electrode[
             burst_start[region]:(burst_end[region] - 1)])
        # normalized to HZ per active electrode within burst region
        spikes_in_region_per_active_electrode[region, 2] <-
          spikes_in_region_per_active_electrode[region, 1] /
              (burst_region_length[region] * bin_time)
      }
      stat[8, current] <- mean(spikes_in_region_per_active_electrode[, 1])
      stat[9, current] <- mean(spikes_in_region_per_active_electrode[, 2])

      stat[10, current] <- stat[8, current] * n_active_electrodes
      stat[11, current] <- length(burst_start)
    }
  }
  list(stat, nb_times)
}
# do not change bin and sf for MEA recordings, skip is in seconds
.nb_extract_features <- function(s, sigmas,
                                 min_electrodes,
                                 local_region_min_nae,
                                 duration = 0,
                                 bin = 25,
                                 sf = 12500,
                                 skip = 10) {
  df.spikes <- .nb_prepare(s)
  wells <- unique(substr(colnames(df.spikes), 1, 2))
  output <- list()

  sbegin <- floor(min(df.spikes[df.spikes > 0] / skip)) * skip + skip
  send <- floor(max(df.spikes[df.spikes > 0] / skip)) * skip
  timespan <- send - sbegin
  # bin: 25; #2ms, this is a parameter based on our recording
  bin_time <- bin / sf
  nb_times <- list()
  length(nb_times) <- length(wells)
  names(nb_times) <- wells
  cat(paste0("calculating network bursts for recording ",
             basename(s$file), "\n"))
  for (well.index in 1: length(wells)) {
    well <- wells[well.index]
    temp_return <- .nb_select_and_bin(df.spikes, well, sbegin, send, bin)
    offset2 <- temp_return[[2]]
    well_data <- temp_return[[1]]
    f <- list()
    for (i  in 1: length(sigmas)) {
      f[[i]] <- .nb_gaussian_filter(sigmas[i], well_data, min_electrodes)

    }
    names(f) <- sigmas

    stat0 <- .nb_get_spike_stats(well_data, timespan)
    res1 <- .nb_get_burst_stats_intensities(well_data,
            f, timespan, bin_time, local_region_min_nae,
      sbegin, send, offset2)
    stat1 <- res1[[1]]
    nb_times[[well.index]] <- res1[[2]]

    output[[well.index]] <- list()
    output[[well.index]]$stat0 <- stat0
    output[[well.index]]$stat1 <- stat1
    output[[well.index]]$well <- well


  }

  list(data = output,
    div = unlist(strsplit(unlist(
      strsplit(basename(s$file), "_"))[4], "[.]"))[1],
    nb_times = nb_times)

}

.nb_merge_result <- function(s, result, sigmas) {
  wells <- character()
  divs <- character()
  phenotypes <- character()
  data_all <- numeric()

  w_fun <- function(x) {
    x$well
  }
  for (i in 1: length(result)) {
    current <- result[[i]]
    current_wells <- sapply(current$data, w_fun)
    divs <- c(divs, rep(current$div, length(current_wells)))
    wells <- c(wells, current_wells)
    phenotypes <- c(phenotypes, s[[i]]$treatment[current_wells])

    data <- matrix(0, length(current_wells),
                   length(c(as.vector(current$data[[1]]$stat1),
                            as.vector(current$data[[1]]$stat0))))
    for (j in 1:length(current_wells)) {
      data[j, ] <- c(as.vector(current$data[[j]]$stat1),
                     as.vector(current$data[[j]]$stat0))
    }
    data_all <- rbind(data_all, data)
  }


  feature_names <- character()
  for (i in 1:length(sigmas)) {
    feature_names <- c(feature_names,
        paste(rownames(current$data[[1]]$stat1), sigmas[i], sep = "_"))
  }
  feature_names <- c(feature_names, colnames(current$data[[1]]$stat0))

  df <- data.frame(divs, wells, phenotypes, data_all)
  names(df)[4:dim(df)[2]] <- feature_names

  divs <- sapply(df["divs"], as.character)
  df <- df[mixedorder(divs), ]

  result <- list() # return the result as matrix and un-altered feature names
  result$df <- df
  result$feature_names <- feature_names

  result
}

NB.matrix.to.feature.dfs <- function(Matrix_and_feature_names) {
  data <- Matrix_and_feature_names$df
  feature_names <- Matrix_and_feature_names$feature_names
  data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  n.features <- dim(data)[2] - 3 # escape the first columns
  dfs <- list()
  Well.stat <- table(data[, "wells"])

  ref.matrix <- matrix(NA, length(Well.stat), length(unique(data[, 1])))
  wells <- unique(data[, c("wells", "phenotypes")])
  rownames(ref.matrix) <- wells[, "wells"]
  colnames(ref.matrix) <- unique(data[, 1])

  # now change columnnames to match Ryan's code
  colnames(wells) <- c("well", "treatment")
  n <- dim(data)[1]
  for (index in 1:n.features) {
    data.matrix <- ref.matrix
    for (i in 1:n) {
      data.matrix[data[i, "wells"], data[i, "divs"]] <- data[i, index + 3]
    }
    dfs[[index]] <- .sort_df(cbind(wells, data.matrix))
    # [] keep it as a data.frame
    dfs[[index]][] <- lapply(dfs[[index]], as.character)
  }
  names(dfs) <- feature_names
  dfs
}

.wilcox.test.perm <- function(data, np, g1, g2, feature.index) {
  # now figure out the p from data
  d1 <- data[data[, "phenotypes"] == g1, feature.index]
  d1 <- d1[d1 >= 0]
  d2 <- data[data[, "phenotypes"] == g2, feature.index]
  d2 <- d2[d2 >= 0]
  suppressWarnings(data.p <- wilcox.test(d1, d2)$p.value)

  # subsetting data to genotypes and feature,
  #and also reformat into matrix for easy permutation
  d <- data[(data[, "phenotypes"] == g1) |
              (data[, "phenotypes"] == g2), c(2, feature.index)]
  d[, "wells"] <- factor(d[, "wells"]) # drop unused levels
  Well.stat <- table(d[, "wells"])
  data.matrix <- matrix(NA, length(Well.stat), max(Well.stat))
  rownames(data.matrix) <- names(Well.stat)
  n.cases <- length(unique(data[data[, "phenotypes"] == g1, "wells"]))
  n <- dim(data.matrix)[1]
  for (i in 1:n) {
    temp <- d[d[, "wells"] == rownames(data.matrix)[i], 2]
    data.matrix[i, 1:length(temp)] <- temp
  }

  outp <- matrix(0, np, 1)
  for (i in 1:np) {
    cases <- sample(n, n.cases)
    d1 <- as.vector(data.matrix[cases, ])
    d1 <- d1[d1 >= 0]
    d2 <- as.vector(data.matrix[- cases, ])
    d2 <- d2[d2 >= 0]
    suppressWarnings(outp[i] <- wilcox.test(d1, d2)$p.value)
  }
  outp <- sort(outp)

  perm.p <- length(which(outp < data.p)) / np
  list(perm.p = perm.p, outp = outp)

}


calculate.network.bursts <-
  function(s, sigmas, min_electrodes, local_region_min_nae) {
  # extract features and merge features
  # from different recordings into one data frame
  nb.structure <- list()
  nb.structure$summary <- list()
  nb.structure$nb_all <- list()
  nb.structure$nb_features <- list()
  if (length(s) > 0) {
    featuresExtracted.AllDIV <- list()
    for (i in 1:length(s)) {
      featuresExtracted.AllDIV[[i]] <-
        .nb_extract_features(s[[i]],
            sigmas, min_electrodes, local_region_min_nae)
      featuresExtracted.OneDIV <- list()
      featuresExtracted.OneDIV[[1]] <- featuresExtracted.AllDIV[[i]]
      nb.structure$nb_all[[i]] <- featuresExtracted.AllDIV[[i]]$nb_times
      nb.structure$result[[i]] <- featuresExtracted.AllDIV[[i]]
      nb.structure$nb_features[[i]] <-
        .nb_merge_result(s, featuresExtracted.OneDIV, sigmas)

    }
    nb.structure$nb_features_merged <-
      .nb_merge_result(s, nb.structure$result, sigmas)
  }
  nb.structure
}
