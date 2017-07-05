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
    data_dists[["ENT"]][[treatment]] <- .list_to_vals(norm_ent_means_per_well)
    data_dists[["MI"]][[treatment]] <- .list_to_vals(norm_mi_means_per_well)
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
    elec_pairs <- .electrode_dist_pairs(mea, elec_names = well_elec_names,
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
    item_vals_range <- .iqr_range(item_vals, mult_factor)
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
  bins <- .uniform_bins(t_0, t_end, bin_count, bin_size = bin_size)
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

.electrode_dist_pairs <- function(mea, elec_names, dist_max = 200,
  same_well_only = T) {
  # makes all possible comparisons of electrode coordinates
  # on plate, returns list of electrode:electrode name
  # combinations where dist. btwn electrodes is <= dist_max
  elec_combos <- c()
  if (length(elec_names) <= 1) {
    return(c())
  }
  epos_subset <- mea$layout$pos[elec_names, ]
  epos_elecs <- rownames(epos_subset)
  for (i in 1:(length(epos_elecs) - 1)) {
    elec_i_name <- epos_elecs[i]
    well_i <- strsplit(elec_i_name, "_")[[1]]
    elec_i_xy <- epos_subset[elec_i_name, ]
    for (j in (i + 1):length(epos_elecs)) {
      elec_j_name <- epos_elecs[j]
      well_j <- strsplit(elec_j_name, "_")[[1]]
      if (well_i != well_j && same_well_only == T) {
        next
      }
      elec_j_xy <- epos_subset[elec_j_name, ]
      i_j_dist <- .euc_dist(elec_i_xy, elec_j_xy)
      if (i_j_dist <= dist_max) {
        elec_ij <- paste(elec_i_name, elec_j_name, sep = ":")
        elec_combos <- c(elec_combos, elec_ij)
      }
    }
  }
  return(elec_combos)
}
.euc_dist <- function(coords_i, coords_j) {
  # calculate euclidian distance between coordinates i and j
  stopifnot(length(coords_i) == length(coords_j))
  dists_squared <- (coords_i - coords_j) ^ 2
  dist <- sqrt(sum(dists_squared))
  return(dist)
}

.uniform_bins <- function(t_0, t_end, n, bin_size=NA) {
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
  a_total <- sum(a)
  b_total <- sum(b)
  # calc p(i) for a and b seperately
  p_a <- a / a_total
  p_b <- b / b_total

  if (normalize == T) {
    return(cor(p_a, p_b, method = corr_method))
  } else {
    return(cor(a, b, method = corr_method))
  }
}

.iqr_range <- function(x, mult_factor = 1) {
  x_median <- median(x)
  x_iqr <- IQR(x)
  x_iqr_min <- x_median - (mult_factor * x_iqr)
  x_iqr_max <- x_median + (mult_factor * x_iqr)
  x_iqr_range <- c(x_iqr_min, x_iqr_max)
  return(x_iqr_range)
}

.spikes_in_bins <- function(spikes, bins) {
  # count number of spikes in each bin
  hist_data <- hist(spikes, breaks = bins, plot = FALSE)
  spike_counts <- hist_data$counts
  return(spike_counts)
}

.p_bins <- function(bin_sizes) {
  # return prob of each bin assuming prob is linear
  # with bin size
  bins_totalsize <- sum(bin_sizes)
  p_bin <- bin_sizes / bins_totalsize
  return(p_bin)
}

.pdist_electrode_spikes <- function(spikes, t_0, t_end, bin_starts=c(),
                                    bin_ends=c(), bin_size=NA, probs=T) {
  # count number of spikes in each bin (uniform or user-defined),
  # if user indicates,form prob.distribution based on the spike
  # counts per bin, otherwise return spike counts per bin
  if (length(bin_starts) > 0 & length(bin_ends) > 0) {
    bin_edges <- unique(sort(c(t_0, bin_starts, bin_ends, t_end)))
  } else if (is.na(bin_size) == F) {
    bin_edges <- .uniform_bins(t_0, t_end, 1, bin_size = bin_size)
  } else {
    bin_size <- (t_end - t_0) / length(spikes)
    bin_edges <- seq(t_0, t_end, by = bin_size)
    bin_starts_i <- seq(1, (length(bin_edges) - 1), by = 1)
    bin_ends_i <- seq(2, length(bin_edges), by = 1)
    bin_starts <- bin_edges[bin_starts_i]
    bin_ends <- bin_edges[bin_ends_i]
  }
  spike_counts <- .spikes_in_bins(spikes, bin_edges)
  if (probs == T) {
    p_counts_bins <- .p_bins(spike_counts)
    return(p_counts_bins)
  } else {
    return(spike_counts)
  }
}

.entropy <- function(x, normalized_uniform=F) {
  # change counts to probs for x
  x_total <- sum(x)
  p_x <- x / x_total
  entropy_x <- p_x * log2(p_x)
  # normalized entropy vals by theoretical max # bits stored in seq
  if (normalized_uniform == T) {
    entropy_x <- entropy_x / log2(length(x))
  }
  # remove any NaN's produced in computation
  entropy_x <- entropy_x[is.nan(entropy_x) == F & is.na(entropy_x) == F]
  # calc total entropy
  entropy_x_total <- - (sum(entropy_x))
  return(entropy_x_total)
}

.kl_divergence <- function(x, y) {
  # if counts provided, ensure vals are changed to probs
  p_x <- x / sum(x)
  p_y <- y / sum(y)
  # get log2(p_x/p_y) likelihood ratio, ensure 0 division isn't encountered
  lr <- ifelse(p_x > 0, log2(p_x / p_y), 0)
  # complete KL divergence calc by summing lr product with p_x
  kl_div <- sum(p_x * lr)
  return(kl_div)
}

.mutual_information <- function(a, b, normalize=F) {
  # make sure a and b have same number of bins (equal bin sizes assumed)
  stopifnot(length(a) == length(b))
  # get sum of counts across bins for a, b, a+b
  a_total <- sum(a)
  b_total <- sum(b)
  ab_total <- a_total + b_total
  # calc p(i) for a and b seperately
  # form matrix with a,b counts, calc p(i,j) across a+b prob. space
  p_ab <- rbind(a, b) / ab_total
  # calc p(i) for a across a+b
  p_a_i_ab <- a / ab_total
  # calc p(j) for b across a+b
  p_b_j_ab <- b / ab_total
  # get total prob of an observation falling in a, given a+b
  p_a_ab <- sum(p_a_i_ab)
  # get total prob of an observation falling in b, given a+b
  p_b_ab <- sum(p_b_j_ab)
  # get total p(i) in a+b
  p_i_ab <- (p_a_i_ab + p_b_j_ab)
  # form col of [p(a|ab),p(b|ab)]
  p_x <- c(p_a_ab, p_b_ab)
  # rename p(i) in a+b
  p_y <- p_i_ab
  # form matrix of expected probs based on [p(a|ab),p(b|ab)], p(i) in a+b
  p_null <- p_x %o% p_y
  # calc KL divergence between joint prob dist (a+b) and indep dists (a)(b)
  mi <- .kl_divergence(p_ab, p_null)
  if (normalize == T) {
    ent_a <- .entropy(a)
    ent_b <- .entropy(b)
    mi <- mi / (sqrt(ent_a * ent_b))
  }
  return(mi)
}

.get_bin_fullset <- function(bin_starts, bin_ends, t_0, t_end) {
  # take bin_starts vector, bin_ends vector, t_0 and t_end,
  # and return one vector of bin edges
  bin_fullset <- c(t_0, bin_starts, bin_ends, t_end)
  bin_fullset <- unique(sort(bin_fullset))
  return(bin_fullset)
}

.get_bin_sizes <- function(bin_set, pairs_only=F) {
  # take vector of bin edges and return bin sizes
  by_iter <- 1
  if (pairs_only == T) {
    by_iter <- 2
  }
  stopifnot(length(bin_set) %% 2 == 0)
  bin_starts_i <- seq(1, length(bin_set) - 1, by = by_iter)
  bin_ends_i <- seq(2, length(bin_set), by = by_iter)
  bin_starts <- bin_set[bin_starts_i]
  bin_ends <- bin_set[bin_ends_i]
  return(bin_ends - bin_starts)
}

.entropy_electrode <- function(mea, elec_name, bin_size=NA) {
  # return calculated entropy value for inputted elctrode name
  spikes <- mea$spikes[[elec_name]]
  t_0 <- mea$rec.time[1]
  t_end <- mea$rec.time[2]
  spikes_pdist <- .pdist_electrode_spikes(spikes, t_0, t_end,
    bin_size = bin_size)
  ent <- .entropy(spikes_pdist, normalized_uniform = T)
  return(ent)
}

.list_to_vals <- function(list_node, numeric=T) {
  # extract and return all values stored in list node
  vals <- c()
  for (name in names(list_node)) {
    if (numeric == T) {
      vals <- c(vals, as.numeric(list_node[[name]]))
    } else {
      vals <- c(vals, list_node[[name]])
    }
  }
  return(vals)
}

.wells_subset <- function(wells_node, wells) {
  wells_node_subset <- list()
  for (name in intersect(names(wells_node), wells)) {
    wells_node_subset[[name]] <- wells_node[[name]]
  }
  return(wells_node_subset)
}

.calculate_entropy_by_well <- function(mea, mult_factor = 1.5) {
  norm_ents_per_well <- .entropies_per_well(mea, diff = diff)
  norm_ents_per_well <- .filter_list(norm_ents_per_well, mult_factor = 1.5)
  return(norm_ents_per_well)
}
