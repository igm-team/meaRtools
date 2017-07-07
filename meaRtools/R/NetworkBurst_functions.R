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
}


# convert the spike list to a matrix, probably not necessary anymore
# but just to be consistent with matlab for now
.NB.prepare <- function(S) {
  spikes <- S$spikes
  colnames <- names(spikes)
  min.values <- sapply(spikes, min)
  max.values <- sapply(spikes, max)

  slength = sapply(spikes, length)
  max.elements <- max(slength)

  data <- matrix(0, length(colnames), max.elements)
  for (i in  1:length(colnames)) {
    data[i, 1:slength[[i]]] <- spikes[[i]]
  }
  rownames(data) <- colnames
  data <- t(data)
  data
}

# generate binned data for the well
.NB.select.and.bin <- function(data, well, sbegin, send, bin) {
  well.data <- data[, grep(well, colnames(data))]
  well.data[well.data < sbegin | well.data > send] <- 0
  if (is.null(dim(well.data))) {
    dim(well.data) <- c(length(well.data), 1)
  }
  if (length(well.data) > 0){
    to.add.back <- min(well.data[well.data > 0])
  } else {
    to.add.back <- 0
  }


  well.data <- well.data[rowSums(well.data) > 0, ]
  well.data <- round(well.data * 12500)
  temp <- well.data[well.data > 0]
  if (length(temp) > 0) {
    t_min <- min(temp)
    t_max <- max(temp)
    well.data <- well.data - t_min + 1
    range <- (t_max - t_min + 1)
    bins <- floor(range / bin)
    if (range %% bin > 0) {
      bins <- bins + 1
    }
    x <- (0:bins) * bin + 1
    y <- c(- Inf, x, max(x) + bin, + Inf)

    if (is.null(dim(well.data))) {
      dim(well.data) <- c(length(well.data), 1)
    }
    well.data.new <- matrix(0, bins + 1, dim(well.data)[2])
    for (i in 1: dim(well.data)[2]) {
      temp <- hist(well.data[, i], breaks = y, plot = FALSE)$counts
      temp <- temp[2:(length(temp) - 1)]
      well.data.new[, i] <- temp
    }
    well.data.new[well.data.new > 1] <- 1
  } else {
    well.data.new <- temp
  }

  list(well.data.new, to.add.back)
}

.NB.Gaussian.Filter <- function(sigma, well.data, min_electrodes) {
  sigma.input <- sigma
  if (length(well.data) == 0) {
    f2 <- numeric()
  } else {
    # a very flat filter. Consider a much shapper one later
    if (sigma %% 2 == 0) { sigma <- sigma + 1}
    filter_size <- sigma
    half_size <- (filter_size - 1) / 2
    x <- seq(- half_size, half_size, length = filter_size)
    gaussFilter <- exp(- x ^ 2 / (2 * sigma ^ 2))
    gaussFilter <- gaussFilter / sum(gaussFilter)
    f <- well.data
    if (length(which(apply(well.data, 2, max) > 0)) >= min_electrodes) {
      for (i in 1:dim(f)[2]) {
        if (max(well.data[, i]) > 0) {
          f[, i] <- filter(well.data[, i], gaussFilter, circular = TRUE)
          f[, i] <- f[, i] / max(f[, i])
        }
      }
      f1 <- rowSums(f)
      f1 <- f1 / max(f1)
      f2 <- filter(f1, gaussFilter, circular = TRUE)
      f2 <- f2 / max(f2)
      dim(f2) <- c(length(f2), 1)
      colnames(f2) <- sigma.input
    } else {
      f2 <- numeric()
    }
  }
  f2
}

.NB.Get.Spike.Stats <- function(well.data, timespan) {
  stat <- matrix(0, 1, 3)
  if (length(well.data) > 0) {
    stat[1] = length(which(apply(well.data, 2, max) > 0)) # number of active electrodes
    stat[2] = sum(well.data) / timespan; # well level mfr
    if (stat[1] > 0) { # mfr by active electrodes
      stat[3] <- stat[2] / stat[1]
    }
  }
  colnames(stat) <- c("n.active.electrodes", "mfr.per.well", "mfr.per.active.electode")
  stat
}

.NB.Get.Burst.Stats.Intensities <- function(well.data, f, timespan, bin.time, min_electrodes,
  sbegin, send, offset2=0, binSize=0.002) {
  # binSize is set to 0.002 based on sample rate of 12500/s
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
    if (length(f[[current]] > 0)) { # not all have names
      colnames(stat)[current] <- colnames(f[[current]]) # not all have names
    }
  }
  nbTimes <- list(); length(nbTimes) <- length(f); names(nbTimes) <- names(f);

  for (current  in 1:n) {
    temp = f[[current]]
    if (length(temp) > 0) {
      nActiveElectrodes <- length(which(apply(well.data, 2, max) > 0))
      temp[temp < 0] <- 0
      level <- .graythresh(temp)
      indicator0 <- temp >= level # get timestamps that are identified as part of network bursts

      if (min_electrodes > 0) {
        indicator = c(0, indicator0, 0) # prepare for finding intervals
        diff_indicator <- diff(indicator)
        burst_start <- which(diff_indicator == 1)
        burst_end <- which(diff_indicator == - 1)
        if (length(burst_start) != length(burst_end)) {
          stop("Burst region no match")
        }

        ## filtered regions based on minimum number of active electrodes
        for (region in 1: length(burst_start)) {
          current_region_nAE <- length(which(colSums(well.data[(burst_start[region]):(burst_end[region] - 1), ]) > 0))
          if (current_region_nAE < min_electrodes) {
            indicator0[burst_start[region]:(burst_end[region] - 1)] <- 0
          }
        }
      }

      indicator = c(0, indicator0, 0) # prepare for finding intervals
      diff_indicator <- diff(indicator)
      burst_start <- which(diff_indicator == 1)
      burst_end <- which(diff_indicator == - 1)
      if (length(burst_start) != length(burst_end)) {
        stop("Burst region no match")
      }


      nbTimesTemp <- cbind.data.frame(burst_start, burst_end)
      nbTimesTemp$startT = burst_start * 0.002 + offset2
      nbTimesTemp$endT = burst_end * 0.002 + offset2
      nbTimes[[current]] <- nbTimesTemp

      stat[1, current] <- sum(indicator0) * bin.time / timespan # mean network burst time per second
      totalspikes <- rowSums(well.data)
      SpikesInNetworkBursts <- sum(totalspikes * indicator0)
      stat[2, current] <- SpikesInNetworkBursts
      stat[3, current] <- stat[2, current] / sum(totalspikes) # percentage of spikes in network bursts

      TotalBurstRegions <- sum(burst_end - burst_start)
      stat[4, current] <- SpikesInNetworkBursts / TotalBurstRegions # overall Spike Intensity
      stat[5, current] <- stat[4, current] / nActiveElectrodes
      stat[6, current] <- SpikesInNetworkBursts / length(burst_start)
      stat[7, current] = stat[6, current] / nActiveElectrodes

      burst_region_length <- burst_end - burst_start
      total_spikes_per_active_electrode <- totalspikes / nActiveElectrodes
      spikes_in_region_per_active_electrode <- matrix(0, length(burst_start), 2)
      for (region in 1: length(burst_start)) {
        spikes_in_region_per_active_electrode[region, 1] <- sum(total_spikes_per_active_electrode[burst_start[region]:(burst_end[region] - 1)])
        # normalized to HZ per active electrode within burst region
        spikes_in_region_per_active_electrode[region, 2] <- spikes_in_region_per_active_electrode[region, 1] / (burst_region_length[region] * bin.time)
      }
      stat[8, current] <- mean(spikes_in_region_per_active_electrode[, 1])
      stat[9, current] <- mean(spikes_in_region_per_active_electrode[, 2])

      stat[10, current] = stat[8, current] * nActiveElectrodes
      stat[11, current] = length(burst_start)
    }
  }
  list(stat, nbTimes)
}
# do not change bin and sf for MEA recordings, skip is in seconds
.NB.Extract.Features <- function(S, Sigma, min_electrodes, local_region_min_nAE, duration = 0, bin = 25, sf = 12500, skip = 10) {
  df.spikes <- .NB.prepare(S)
  wells <- unique(substr(colnames(df.spikes), 1, 2))
  output <- list()

  sbegin <- floor(min(df.spikes[df.spikes > 0] / skip)) * skip + skip
  send <- floor(max(df.spikes[df.spikes > 0] / skip)) * skip
  timespan <- send - sbegin
  # bin = 25; #2ms, this is a parameter based on our recording
  bin.time <- bin / sf
  nb.times <- list(); length(nb.times) = length(wells); names(nb.times) <- wells
  cat(paste0("calculating network bursts for recording ", basename(S$file), "\n"))
  for (well.index in 1: length(wells)) {
    well <- wells[well.index]

    offset <- S$rec_time[1]
    temp.return <- .NB.select.and.bin(df.spikes, well, sbegin, send, bin)
    offset2 <- temp.return[[2]]
    well.data <- temp.return[[1]]
    f <- list()
    for (i  in 1: length(Sigma)) {
      f[[i]] <- .NB.Gaussian.Filter(Sigma[i], well.data, min_electrodes)

    }
    names(f) <- Sigma

    stat0 <- .NB.Get.Spike.Stats(well.data, timespan)
    res1 <- .NB.Get.Burst.Stats.Intensities(well.data, f, timespan, bin.time, local_region_min_nAE,
      sbegin, send, offset2)
    stat1 <- res1[[1]]
    nb.times[[well.index]] <- res1[[2]]

    output[[well.index]] <- list()
    output[[well.index]]$stat0 <- stat0
    output[[well.index]]$stat1 <- stat1
    output[[well.index]]$well <- well


  }

  list(data = output,
    DIV = unlist(strsplit(unlist(strsplit(basename(S$file), "_"))[4], "[.]"))[1],
    nb.times = nb.times)

}

.NB.Merge.Result <- function(s, result, Sigma) {
  Wells <- character()
  DIVs <- character()
  Phenotypes <- character()
  Data <- numeric()

  Count <- 0
  w.fun <- function(x) {x$well}
  for (i in 1: length(result)) {
    current <- result[[i]]
    current.Wells <- sapply(current$data, w.fun)
    DIVs <- c(DIVs, rep(current$DIV, length(current.Wells)))
    Wells <- c(Wells, current.Wells)
    Phenotypes <- c(Phenotypes, s[[i]]$treatment[current.Wells])

    data <- matrix(0, length(current.Wells), length(c(as.vector(current$data[[1]]$stat1), as.vector(current$data[[1]]$stat0))))
    for (j in 1:length(current.Wells)) {
      data[j, ] <- c(as.vector(current$data[[j]]$stat1), as.vector(current$data[[j]]$stat0))
    }
    Data <- rbind(Data, data)
  }


  feature_names <- character()
  for (i in 1:length(Sigma)) {
    feature_names <- c(feature_names, paste(rownames(current$data[[1]]$stat1), Sigma[i], sep = "_"))
  }
  feature_names <- c(feature_names, colnames(current$data[[1]]$stat0))

  df = data.frame(DIVs, Wells, Phenotypes, Data)
  names(df)[4:dim(df)[2]] <- feature_names

  divs <- sapply(df["DIVs"], as.character)
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
  Well.stat <- table(data[, "Wells"])

  ref.matrix <- matrix(NA, length(Well.stat), length(unique(data[, 1])))
  Wells <- unique(data[, c("Wells", "Phenotypes")])
  rownames(ref.matrix) <- Wells[, "Wells"]
  colnames(ref.matrix) <- unique(data[, 1])

  # now change columnnames to match Ryan's code
  colnames(Wells) <- c("well", "treatment")
  n <- dim(data)[1]
  for (index in 1:n.features) {
    data.matrix <- ref.matrix
    for (i in 1:n) {
      data.matrix[data[i, "Wells"], data[i, "DIVs"]] <- data[i, index + 3]
    }
    dfs[[index]] <- .sort_df(cbind(Wells, data.matrix))
    dfs[[index]][] <- lapply(dfs[[index]], as.character) # [] keep it as a data.frame
  }
  # names(dfs) <- colnames(data)[4:(n.features+3)]
  names(dfs) <- feature_names
  dfs
}

.wilcox.test.perm <- function(data, np, g1, g2, feature.index) {
  # now figure out the p from data
  d1 <- data[data[, "Phenotypes"] == g1, feature.index]
  d1 <- d1[d1 >= 0]
  d2 <- data[data[, "Phenotypes"] == g2, feature.index]
  d2 <- d2[d2 >= 0]
  suppressWarnings(data.p <- wilcox.test(d1, d2)$p.value)

  # subsetting data to genotypes and feature, and also reformat into matrix for easy permutation
  d <- data[(data[, "Phenotypes"] == g1) | (data[, "Phenotypes"] == g2), c(2, feature.index)]
  d[, "Wells"] <- factor(d[, "Wells"]) # drop unused levels
  Well.stat <- table(d[, "Wells"])
  data.matrix <- matrix(NA, length(Well.stat), max(Well.stat))
  rownames(data.matrix) <- names(Well.stat)
  n.cases <- length(unique(data[data[, "Phenotypes"] == g1, "Wells"]))
  n <- dim(data.matrix)[1]
  for (i in 1:n) {
    temp <- d[d[, "Wells"] == rownames(data.matrix)[i], 2]
    data.matrix[i, 1:length(temp)] <- temp
  }

  outp <- matrix(0, np, 1)
  for (i in 1:np) {
    cases = sample(n, n.cases)
    d1 = as.vector(data.matrix[cases, ])
    d1 <- d1[d1 >= 0]
    d2 = as.vector(data.matrix[- cases, ])
    d2 <- d2[d2 >= 0]
    suppressWarnings(outp[i] <- wilcox.test(d1, d2)$p.value)
  }
  outp = sort(outp)

  perm.p <- length(which(outp < data.p)) / np
  list(perm.p = perm.p, outp = outp)

}


calculate.network.bursts <- function(s, Sigma, min_electrodes, local_region_min_nAE) {
  # extract features and merge features from different recordings into one data frame
  nb.structure <- list()
  nb.structure$summary <- list()
  nb.structure$nb.all <- list()
  nb.structure$nb.features <- list()
  if (length(s) > 0) {
    featuresExtracted.AllDIV <- list()
    nb.all <- list()
    for (i in 1:length(s)) {
      featuresExtracted.AllDIV[[i]] <- .NB.Extract.Features(s[[i]], Sigma, min_electrodes, local_region_min_nAE)
      featuresExtracted.OneDIV <- list(); featuresExtracted.OneDIV[[1]] <- featuresExtracted.AllDIV[[i]]
      nb.structure$nb.all[[i]] <- featuresExtracted.AllDIV[[i]]$nb.times
      nb.structure$result[[i]] <- featuresExtracted.AllDIV[[i]]
      nb.structure$nb.features[[i]] <- .NB.Merge.Result(s, featuresExtracted.OneDIV , Sigma)

    }
    nb.structure$nb.features.merged <- .NB.Merge.Result(s, nb.structure$result, Sigma)
  }
  nb.structure
}
