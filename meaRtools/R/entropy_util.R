filter_nonactive_spikes <- function(mea, spikes_per_minute_min=1) {
  # remove electrode data where spike.per.minute rate does not
  # meet or exceed user-defined value
  # (default = 1 spike/min = 1 Hz)
  mea$spikes <- sapply(names(mea$spikes), function(y) {
    filter <- (length(mea$spikes[[y]]) / 60) >= spikes_per_minute_min
    if (filter) {
      return(mea$spikes[[y]])
    }
  })
  mea$spikes <- mea$spikes[!sapply(mea$spikes, is.null)]
  return(mea)
}

calculate_entropy_and_mi <- function(mea, treatments,
  mult_factor=1.5,
  bin_size=0.1) {
  data_dists <- list("ENT" = list(), "MI" = list())
  norm_mis_per_well <- list()
  norm_ents_per_well <- .calculate_entropy_by_well(mea, mult_factor)
  for (treatment in treatments) {
    ## get wells classified as treatment and  subset well data on well
    # classification
    treatment_wells <- names(mea$treatment[mea$treatment == treatment])
    treatment_wells <- treatment_wells[treatment_wells %in%
    names(norm_ents_per_well)]
    norm_ents_per_well_treatment <- .wells_subset(norm_ents_per_well,
      treatment_wells)
    ## get mean entropy per well set for treatment
    norm_ent_means_per_well <- lapply(norm_ents_per_well_treatment,
      function(x) {
        mean(x)
      })
    ## calculate MI values
    norm_mis_per_well[[treatment]] <- .pairwise_dists_per_well(mea,
      wellnames = treatment_wells,
      dist_metric = "mutual_information",
      bin_size)
    norm_mi_means_per_well <- lapply(norm_mis_per_well[[treatment]],
      function(x) {
        mean(x)
      })
    # store summary stats to list
    data_dists[["ENT"]][[treatment]] <- .list.to.vals(norm_ent_means_per_well)
    data_dists[["MI"]][[treatment]] <- .list.to.vals(norm_mi_means_per_well)
  }
  return(list("data_dists" = data_dists,
    "norm_mis_per_well" = norm_mis_per_well))
}

.pairwise_dists_per_well <- function(mea, wellnames = c(), bin_size = NA,
  dist_max = 200,
  dist_metric = "mutual_information",
  normalize = T) {
  # iterate through all wells, get all pairwise mutual
  # information scores for each well
  dists_list <- list()
  t_0 <- mea$rec.time[1]
  t_end <- mea$rec.time[2]
  if (length(wellnames) == 0) {
    wellnames <- mea$well
  }
  for (wellname in wellnames) {
    dists_list[[wellname]] <- c()
    well_spikes <- .mea_vals_subset(mea, "spikes", wellname)
    well_elec_names <- names(well_spikes)
    elec_pairs <- .electrode_dist_pairs(mea, elec.names = well_elec_names,
      dist_max = dist_max)
    if (length(elec_pairs) <= 1) {
      next
    }
    dists_list[[wellname]] <- sapply(elec_pairs, function(x) {
      elec_pairs <- strsplit(x, ":")[[1]]
      spikes_i <- well_spikes[[elec_pairs[1]]]
      spikes_j <- well_spikes[[elec_pairs[2]]]
      spikes_i_len <- length(spikes_i)
      spikes_j_len <- length(spikes_j)
      if (min(spikes_i_len, spikes_j_len) <= 1) {
        return(NA)
      }
      dist <- .dist_electrode_pair(spikes_i,
        spikes_j,
        t_0, t_end,
        bin_size = bin_size,
        dist_metric = dist_metric,
        normalize = normalize)
      return(dist)
    })
  }
  return(dists_list)
}

.entropies_per_well <- function(mea, diff = F,
  well_names = c()) {
  # iterate through all electrodes, store entropy vals
  # for each corresponding well
  ents_list <- list()
  for (elec in names(mea$spikes)) {
    ent <- .entropy_electrode(mea, elec)
    if (is.na(ent) == T | is.nan(ent) == T) {
      next
    }
    elec_data <- strsplit(elec, "_")[[1]]
    well <- elec_data[1]
    if (length(well_names) > 0) {
      if (F == (well %in% well_names)) {
        next
      }
    }
    if (F == (well %in% names(ents_list))) {
      ents_list[[well]] <- c()
    }
    ents_list[[well]] <- c(ents_list[[well]], ent)
  }
  return(ents_list)
}

.filter_list <- function(x_list, mult_factor = 1.5) {
  for (item in names(x_list)) {
    item_vals <- x_list[[item]]
    item_vals_range <- .IQR_range(item_vals, mult_factor)
    item_vals <- item_vals[item_vals >= item_vals_range[1]
    & item_vals <= item_vals_range[2]]
    x_list[[item]] <- item_vals
  }
  return(x_list)
}

.mea_vals_subset <- function(mea, attr, wellname) {
  # get set of attr vals for electrodes corresponding to specific inputted well
  new_obj <- list()
  attr_node <- mea[[attr]]
  for (electrode in names(mea[[attr]])) {
    if (grepl(wellname, electrode) == T) {
      new_obj[[electrode]] <- attr_node[[electrode]]
    }
  }
  return(new_obj)
}

.dist_electrode_pair <- function(spikes_a, spikes_b, t_0, t_end,
  bin_size = NA,
  normalize = F,
  dist_metric = "mutual_information",
  corr_method = "pearson") {
  # use spike data and t_0+t_end to calculate mutual
  # information between spikes from electrodes a and b
  if (is.na(bin_size)) {
    a_n <- length(spikes_a)
    b_n <- length(spikes_b)
    bin_count <- min(a_n, b_n)
  } else {
    bin_count <- 1
  }
  bins <- .uniform.bins(t_0, t_end, bin_count, bin_size = bin_size)
  spikes_a_bin <- .spikes_in_bins(spikes_a, bins)
  spikes_b_bin <- .spikes_in_bins(spikes_b, bins)
  if (dist_metric == "mutual_information") {
    dist <- .mutual_information(spikes_a_bin, spikes_b_bin,
      normalize = normalize)
  } else {
    dist <- .correlation(spikes_a_bin, spikes_b_bin,
      corr_method = corr_method)
  }
  return(dist)
}

.electrode_dist_pairs <- function(mea, elec.names, dist_max = 200,
  same.well.only = T) {
  # makes all possible comparisons of electrode coordinates
  # on plate, returns list of electrode:electrode name
  # combinations where dist. btwn electrodes is <= dist_max
  elec.combos <- c()
  if (length(elec.names) <= 1) {return(c())}
  epos.subset <- mea$layout$pos[elec.names, ]
  epos.elecs <- rownames(epos.subset)
  for (i in 1:(length(epos.elecs) - 1)) {
    elec.i.name <- epos.elecs[i]
    well.i <- strsplit(elec.i.name, "_")[[1]]
    elec.i.xy <- epos.subset[elec.i.name, ]
    for (j in (i + 1):length(epos.elecs)) {
      elec.j.name <- epos.elecs[j]
      well.j <- strsplit(elec.j.name, "_")[[1]]
      if (well.i != well.j && same.well.only == T) {
        next
      }
      elec.j.xy <- epos.subset[elec.j.name, ]
      i.j.dist <- .euc.dist(elec.i.xy, elec.j.xy)
      if (i.j.dist <= dist_max) {
        elec.ij <- paste(elec.i.name, elec.j.name, sep = ":")
        elec.combos <- c(elec.combos, elec.ij)
      }
    }
  }
  return(elec.combos)
}
.euc.dist <- function(coords.i, coords.j) {
  # calculate euclidian distance between coordinates i and j
  stopifnot(length(coords.i) == length(coords.j))
  dists.squared <- (coords.i - coords.j) ^ 2
  dist <- sqrt(sum(dists.squared))
  return(dist)
}

.uniform.bins <- function(t_0, t_end, n, bin_size=NA) {
  # for a defined t_0, t_end and n.spikes, return n nonoverlapping
  # bins that are all of equal size and span [t_0, t_end]
  if (is.na(bin_size)) {
    bin_size <- (t_end - t_0) / n
  }
  bins <- seq(from = t_0, to = t_end, by = bin_size)
  if (bins[length(bins)] != t_end) {
    bins <- c(bins, t_end)
  }
  return(bins)
}

.correlation <- function(a, b, corr_method="pearson", normalize=F) {
  ## calculate the correlation btwn probability distributions a and b

  # make sure a and b have same number of bins (equal bin sizes assumed)
  stopifnot(length(a) == length(b))
  # get sum of counts across bins for a, b, a+b
  a.total <- sum(a)
  b.total <- sum(b)
  ab.total <- a.total + b.total
  # calc p(i) for a and b seperately
  p.a <- a / a.total
  p.b <- b / b.total

  if (normalize == T) {
    return(cor(p.a, p.b, method = corr_method))
  } else {
    return(cor(a, b, method = corr_method))
  }
}

.IQR_range <- function(x, mult_factor = 1) {
  x.median <- median(x)
  x.IQR <- IQR(x)
  x.IQR.min <- x.median - (mult_factor * x.IQR)
  x.IQR.max <- x.median + (mult_factor * x.IQR)
  x.IQR_range <- c(x.IQR.min, x.IQR.max)
  return(x.IQR_range)
}

.spikes_in_bins <- function(spikes, bins) {
  # count number of spikes in each bin
  hist.data <- hist(spikes, breaks = bins, plot = FALSE)
  spike.counts <- hist.data$counts
  return(spike.counts)
}

.p.bins <- function(bin_sizes) {
  # return prob of each bin assuming prob is linear
  # with bin size
  bins.totalsize <- sum(bin_sizes)
  p.bin <- bin_sizes / bins.totalsize
  return(p.bin)
}

.pdist.electrode.spikes <- function(spikes, t_0, t_end, bin.starts=c(), bin.ends=c(), bin_size=NA, probs=T) {
  # count number of spikes in each bin (uniform or user-defined),
  # if user indicates,form prob.distribution based on the spike
  # counts per bin, otherwise return spike counts per bin
  if (length(bin.starts) > 0 & length(bin.ends) > 0) {
    bin.edges <- unique(sort(c(t_0, bin.starts, bin.ends, t_end)))
  } else if (is.na(bin_size) == F) {
    bin.edges <- uniform.bins(t_0, t_end, 1, bin_size = bin_size)
  } else {
    bin_size <- (t_end - t_0) / length(spikes)
    bin.edges <- seq(t_0, t_end, by = bin_size)
    bin.starts.i <- seq(1, (length(bin.edges) - 1), by = 1)
    bin.ends.i <- seq(2, length(bin.edges), by = 1)
    bin.starts <- bin.edges[bin.starts.i]
    bin.ends <- bin.edges[bin.ends.i]
  }
  spike.counts <- .spikes_in_bins(spikes, bin.edges)
  if (probs == T) {
    p.counts.bins <- .p.bins(spike.counts)
    return(p.counts.bins)
  } else {
    return(spike.counts)
  }
}

.entropy <- function(x, normalized.uniform=F) {
  # change counts to probs for x
  x.total = sum(x)
  p.x <- x / x.total
  entropy.x <- p.x * log2(p.x)
  # normalized entropy vals by theoretical max # bits stored in seq
  if (normalized.uniform == T) {
    entropy.x = entropy.x / log2(length(x))
  }
  # remove any NaN's produced in computation
  entropy.x <- entropy.x[is.nan(entropy.x) == F & is.na(entropy.x) == F]
  # calc total entropy
  entropy.x.total = - (sum(entropy.x))
  return(entropy.x.total)
}

.KL.divergence <- function(x, y) {
  # if counts provided, ensure vals are changed to probs
  p.x <- x / sum(x)
  p.y <- y / sum(y)
  # get log2(p.x/p.y) likelihood ratio, ensure 0 division isn't encountered
  LR <- ifelse(p.x > 0, log2(p.x / p.y), 0)
  # complete KL divergence calc by summing LR product with p.x
  KL.div <- sum(p.x * LR)
  return(KL.div)
}

.mutual_information <- function(a, b, normalize=F) {
  # make sure a and b have same number of bins (equal bin sizes assumed)
  stopifnot(length(a) == length(b))
  # get sum of counts across bins for a, b, a+b
  a.total <- sum(a)
  b.total <- sum(b)
  ab.total <- a.total + b.total
  # calc p(i) for a and b seperately
  p.a <- a / a.total
  p.b <- b / b.total
  # form matrix with a,b counts, calc p(i,j) across a+b prob. space
  p.ab <- rbind(a, b) / ab.total
  # calc p(i) for a across a+b
  p.a.i.ab <- a / ab.total
  # calc p(j) for b across a+b
  p.b.j.ab <- b / ab.total
  # get total prob of an observation falling in a, given a+b
  p.a.ab <- sum(p.a.i.ab)
  # get total prob of an observation falling in b, given a+b
  p.b.ab <- sum(p.b.j.ab)
  # get total p(i) in a+b
  p.i.ab <- (p.a.i.ab + p.b.j.ab)
  # form col of [p(a|ab),p(b|ab)]
  p.x <- c(p.a.ab, p.b.ab)
  # rename p(i) in a+b
  p.y <- p.i.ab
  # form matrix of expected probs based on [p(a|ab),p(b|ab)], p(i) in a+b
  p.null <- p.x %o% p.y
  # calc KL divergence between joint prob dist (a+b) and indep dists (a)(b)
  MI <- .KL.divergence(p.ab, p.null)
  if (normalize == T) {
    ent.a <- .entropy(a)
    ent.b <- .entropy(b)
    MI <- MI / (sqrt(ent.a * ent.b))
  }
  return(MI)
}

.get.bin.fullset <- function(bin.starts, bin.ends, t_0, t_end) {
  # take bin.starts vector, bin.ends vector, t_0 and t_end,
  # and return one vector of bin edges
  bin.fullset <- c(t_0, bin.starts, bin.ends, t_end)
  bin.fullset <- unique(sort(bin.fullset))
  return(bin.fullset)
}

.get.bin_sizes <- function(bin.set, pairs.only=F) {
  # take vector of bin edges and return bin sizes
  by.iter = 1
  if (pairs.only == T) {
    by.iter = 2
  }
  stopifnot(length(bin.set) %% 2 == 0)
  bin.starts.i <- seq(1, length(bin.set) - 1, by = by.iter)
  bin.ends.i <- seq(2, length(bin.set), by = by.iter)
  bin.starts <- bin.set[bin.starts.i]
  bin.ends <- bin.set[bin.ends.i]
  return(bin.ends - bin.starts)
}

.entropy_electrode <- function(mea, elec.name, bin_size=NA) {
  # return calculated entropy value for inputted elctrode name
  spikes <- mea$spikes[[elec.name]]
  t_0 <- mea$rec.time[1]
  t_end <- mea$rec.time[2]
  spikes.pdist <- .pdist.electrode.spikes(spikes, t_0, t_end,
    bin_size = bin_size)
  ent <- .entropy(spikes.pdist, normalized.uniform = T)
  return(ent)
}

.list.to.vals <- function(list.node, numeric=T) {
  # extract and return all values stored in list node
  vals <- c()
  for (name in names(list.node)) {
    if (numeric == T) {
      vals <- c(vals, as.numeric(list.node[[name]]))
    } else {
      vals <- c(vals, listnode[[name]])
    }
  }
  return(vals)
}

.wells_subset <- function(wells.node, wells) {
  wells.node.subset = list()
  for (name in intersect(names(wells.node), wells)) {
    wells.node.subset[[name]] <- wells.node[[name]]
  }
  return(wells.node.subset)
}

.calculate_entropy_by_well <- function(mea, mult_factor = 1.5) {
  norm_ents_per_well <- .entropies_per_well(mea, diff = diff)
  norm_ents_per_well <- .filter_list(norm_ents_per_well, mult_factor = 1.5)
  return(norm_ents_per_well)
}
