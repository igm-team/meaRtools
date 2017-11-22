
calc_burst_distributions <- function(s, min_vals=1, xlimit=25, bins_in_sec=5,
  feature="non", filter_values_by_min=0, min_values=0,
  per_well=0, outputdir=getwd(), min_electrodes=4,
  time_stamp="DATE_TIME") {
  # Plot distributions of selected bursting features and print csv
  # output for stats
  #
  # Args:
  # s: the recorded object
  # min_vals: minimum values number per electrode, electrodes with a smaller
  # number of values than that are discarded
  # xlimit:  max limit of values, for example: xlimit equals 25 for IBI
  # analysis means that IBIs longer than 25 seconds
  # will not be part of distribution calculations
  # bins_in_sec:  how many bins to cut each of the segments. For example:
  # IBI analysis has 25 seconds as xlimit, to analyse in a 0.1 sec resolution
  # bins_in_sec should be set to 10, for 1 sec resolution set bins_in_sec to 1
  # feature  :  what feature to analyze, options are "ibi", "isi,
  # "nspikesInBurst", "duration", "spikesDensityInBurst"
  # filter_values_by_min:  should analysis disregard values with lower then
  # filter_values_by_min number of values ? (0/1, default is 0) for example, if
  # set to 1 for duration analysis, should analysis consider also bursts
  # shorter than filter_values_by_min ?
  # min_values: disregards values with lower then filter_values_by_min , only
  # if filter_values_by_min set to 1
  # per_well: should distribution analysis be performed by testing treatment
  # differences on well level means (1) or electrode level means(0)
  # outputdir: output directory
  #
  # Output:
  # Plots the burst distributions.
  # Writes a burst distribution csv to be used for permutation test and
  # plotting

  outputdir <- paste0(outputdir, "/", "distributionFiles")
  suppressWarnings(dir.create(outputdir))

  basename <- get_file_basename(s$file)
  log_file <- paste(outputdir, "/", get_project_plate_name(s$file),
    "_distributions_log.txt", sep = "")

  stopifnot(feature != "non")
  ma5 <- c(rep(1, 5)) / 5
  duration <- s$rec_time[2] - s$rec_time[1]

  write(paste("->->-> Analysing ", s$file, sep = ""), file = log_file,
        append = TRUE)

  cat(paste("Arguments: min_vals=", min_vals, "; xlimit=", xlimit,
            "; bins_in_sec=", bins_in_sec, ";per_well=", per_well,
            "; duration=", duration, "; feature='", feature,
            "'; filter_values_by_min=", filter_values_by_min, "; min_values=",
            min_values, "\n", sep = ""))

  treatments <- unique(s$treatment)
  treatments <- treatments[!nchar(treatments) == 0]

  f_vals <- NULL
  if (feature == "ibi") {
    f_vals <- .calc_all_ibi(s, s$allb)
  } else if (feature == "isi") {
    f_vals <- .calc_all_isi(s, s$allb)
  } else if (feature == "nspikes_in_burst") {
    for (j in 1:length(s$channels)) {
      ## Number of spikes in burst !
      f_vals[j] <- get_burst_info(s$allb[j], "len")
    }} else if (feature == "duration") {
    for (j in 1:length(s$channels)) {
      ## Duration of burst
      f_vals[j] <- get_burst_info(s$allb[j], "durn")
    }} else if (feature == "spikes_density_in_burst") {
    for (j in 1:length(s$channels)) {
      if (length(get_burst_info(s$allb[j], "durn")[[1]]) > 1) {
        ## Number of spikes in burst divided by duration of burst
        f_vals[j] <- mapply("/", get_burst_info(s$allb[j], "len"),
                            get_burst_info(s$allb[j], "durn"),
                            SIMPLIFY = FALSE)
      } else {
        f_vals[j] <- 0
        if (get_burst_info(s$allb[j], "durn") == 0) {
          f_vals[j] <- 0
        } else {
          f_vals[j] <- mapply("/", get_burst_info(s$allb[j], "len"),
                              get_burst_info(s$allb[j], "durn"),
                              SIMPLIFY = FALSE)
        }
      }
    }
  }

  jump <- bins_in_sec

  # Calculate active E per well
  c_per_well <- NULL
  wells <- unique(s$cw)
  if (length(wells) > 0) {
    for (well in wells) {
      treat <- s$treatment[well][[1]]
      if (treat == "") {
        write(paste("Skipping well-", well, " that has active electrodes",
        " since treatment property is empty.",
        sep = ""), file = log_file, append = TRUE)
      }
      active_electrodes <- which(s$cw == well & as.vector(
        unlist(lapply(s$isis, length))) > 0)
      c_per_well <- rbind(c_per_well, data.frame(well, new_count =
                                                 length(active_electrodes)))
    }} else {
    write("No wells found!", file = log_file, append = TRUE)
    return(1)
  }

  old <- ""
  well_data <- NULL
  treat <- NULL
  current_enum <- 0

  # Making the full table
  post <- c(
    (1 / jump) * (min_values * jump):(xlimit * jump)
    )
  post_t <- post[1:length(post) - 1] + (post[1] + post[2]) / 2

  # Creating the electrode / well matrix
  all_pos <- rep(post_t, times = length(treatments))
  all_trt <- rep(treatments, each = length(post_t))
  all <- data.frame(pos = all_pos, treat = all_trt)
  first_data_column <- 3

  for (j in 1:length(s$channels)) {

    # indicator of current well
    icurrentwell <- (s$channels[j])
    well <- strsplit(icurrentwell, "_")[[1]][1]

    treat <- s$treatment[well][[1]]
    # Skip electrode if it does not have a valid treatment property
    # from csv file
    if (!(treat %in% treatments)) {
      next
    }
    if (treat == "") {
      next
    }

    # Check number of aE
    if (c_per_well$new_count[c_per_well$well == well] < min_electrodes){
      write(paste("Skipped well- ", well, " less than ", min_electrodes,
                  " electrodes", sep = ""), file = log_file, append = TRUE)
      next
    } else {
      data <- f_vals[[j]]
    }

    #     Filter data only if over a min
    if (filter_values_by_min == 1) {
      data <- data[data > min_values]
    }

    # Remove from analysis all values that are beyond xLimit, to make sure that
    # the output distribution will have a total on 1 (all values within range)
    data <- data[data <= xlimit]
    hist_data <- data.frame()

    # If electrode has enough values then calculate normal histogram
    if (length(data) > min_vals) {
      e <- hist(data, plot = FALSE, breaks =
                  c(
                    (1 / jump) * (min_values * jump):(xlimit * jump))
                )

      # normalize to number of values in electrode
      hist_data <- data.frame(normHist = (e$counts / length(data)),
                              pos = e$mids, treat = treat)
      names(hist_data)[1] <- icurrentwell

      if (per_well == 0) {
        # add norm hist data to the overall table
        if (treat %in% treatments) {
          all[, names(hist_data)[1]] <- NA
          all$pos <- as.character(all$pos)
          all[all$treat == treat & all$pos == as.character(hist_data$pos),
              names(hist_data)[1]] <- hist_data[, 1]
        }
      } # END per_well ==0

      # for per well analysis only
      if (per_well == 1){
        # new well
        if (well != old){
          if (old != "") {
            # save old well if there are electrodes to show
            if (current_enum != 0) {
              valid <- 1
              if (current_enum >= min_electrodes) {
                  # calculate average values per electrodes if more than
                  # minimum electrodes
                  rmeans <- apply(well_data[, 1:current_enum], 1, mean)
              } else {
                write(paste("Less than four electrodes passed filters for ",
                            old, ", it will be removed from the analysis."),
                      file = log_file, append = TRUE)
                valid <- 0
              }
              # Continue if treatment is valid
              if (unique(well_data$treat) %in% treatments && valid == 1) {
                # make a new data frame from the well data
                means_df <- data.frame(rmeans, pos = well_data$pos, treat =
                                        unique(well_data$treat))
                names(means_df)[1] <- old # change to name of the well that was
                # just analyzed

                # Fill in the matrix with well data
                all[, old] <- NA
                all$pos <- as.character(all$pos)
                all[as.character(all$treat) == as.character(unique(
                  well_data$treat)) & all$pos == as.character(means_df$pos),
                  old] <- means_df[, 1]
              }
            } # more than 0 electrodes
          }
          write(paste("Analyzing well ", well, ",  channel number is:", j,
                      sep = ""), file = log_file, append = TRUE)
          old <- well
          current_enum <- 1
          well_data <- hist_data
        } else {
          if (current_enum == 0){
            well_data <- hist_data
          } else {
            well_data <- cbind(hist_data[, 1], well_data)
          }
          current_enum <- current_enum + 1
        }
      } else {
        write(paste("Analysis per electrode, electrode-", icurrentwell,
                           "Analyzed."), file = log_file, append = TRUE)
    } # well equals old
  } #    if length of data is bigger than min_vals
} ## End for j in 1 TO s$channles


  # for per well analysis only
  if (per_well == 1) {
    if (current_enum > 1) {
      # save last well
      rmeans <- apply(well_data[, 1:current_enum], 1, mean) # median)}
      # Continue if treatment is valid
      if (unique(well_data$treat) %in% treatments) {

        means_df <- data.frame(rmeans, pos = well_data$pos, treat =
                                 unique(well_data$treat))
        names(means_df)[1] <- old
        # Fill in the matrix with last well data
        all[, old] <- NA
        all$pos <- as.character(all$pos)
        all[as.character(all$treat) == as.character(unique(well_data$treat))
            & all$pos == as.character(means_df$pos), old] <- means_df[, 1]
      }}}

  par(mfrow = c(1, 1))
  colors <- c("red", "blue", "green", "orange", "pink", "brown", "yellow",
              "black")
  first <- 1

  table <- all # NULL

  if (per_well == 1){
    write("Collapsing genotype data by well.", file = log_file, append = TRUE)
  } else {
    write("Collapsing genotype data by electrode.", file = log_file,
          append = TRUE)
  } # per_well ?

  if (is.null(table)) {
    write(paste("Based on current parameters there are no cases to plot,",
                " skipping file ", s$file, sep = ""), file = log_file,
          append = TRUE)
    return(1)
  }
  if (
    (dim(table)[2] - (first_data_column - 1)) <= 1) {
    write(paste("Not enough electrodes (<=1) to plot, skipping file ", s$file,
                sep = ""), file = log_file, append = TRUE)
    return(1)
  }
  write(paste("Number of electrodes in table : ", (dim(table)[2] -
        (first_data_column - 1)), sep = ""), file = log_file, append = TRUE)

  # set the ylimit on the top 2% values of the left most dist values
  lim <- apply(table[, (first_data_column:dim(table)[2])], 1, mean,
               na.rm = TRUE)

  # Set ylimit for graph based on first columns of data (small values)
  ylimit <- max(lim, na.rm = T)

  gmeans_table <- NULL
  good_treatments <- treatments
  # Check for empty genotypes
  for (t in treatments) {
    if (nrow(table[table$treat == t, ]) == 0) {
      good_treatments <- good_treatments[good_treatments != t]
      write(paste("Treatment", t, "has no electrodes to contribute data after",
            " filters and is excluded from analysis.", sep = " "),
            file = log_file, append = TRUE)
      next
    }
    tto_plot <- table[table$treat == t, ]
    # Check how many electrodes remain after all the analysis for the treatment
    clean_trt <- tto_plot[, colSums(is.na(tto_plot)) != nrow(tto_plot)]
    if (ncol(clean_trt) < 7) {
      good_treatments <- good_treatments[good_treatments != t]
      write(paste("Treatment", t, "has less than 4 electrodes to contribute",
                  " data after filters and is excluded from analysis.",
                  sep = " "), file = log_file, append = TRUE)
      next
    }
  }
  all_distributions <- NULL
  if (length(good_treatments) <= 1) {
    write(paste("Not enough treatments (<=1) with enough data to plot,",
          " skipping file ", s$file, sep = ""), file = log_file,
          append = TRUE)
    return(1)
  }
  # plot per genotype data
  for (tr in 1:length(good_treatments)) {
    t <- good_treatments[tr]
    #  select a treatment to plot and calculate means for all the
    # columns of each row (all electrodes of each genotype)
    tto_plot <- table[table$treat == t, ]
    # Prepare a table for permutations analysis
    clean_trt <- tto_plot[, colSums(is.na(tto_plot)) != nrow(tto_plot)]

    transposed <- as.data.frame(t(clean_trt[1:ceiling(xlimit * jump),
                                  first_data_column:length(clean_trt)]))
    names_t <- rownames(transposed)
    for (pos in 1:length(names_t)) {
      names_t[pos] <- strsplit(names_t[pos], "_")[[1]][1]
    }
    transposed <- cbind(genotype = t, well = names_t, transposed)

    names(transposed) <- c("genotype", 1:(length(transposed) - 1))
    all_distributions <- rbind(all_distributions, transposed)
    gmeans <- (apply(tto_plot[, (first_data_column:dim(tto_plot)[2])], 1, mean,
                     na.rm = TRUE))
    # smooth the values for representatoin only
    data <- gmeans
    if (xlimit > 5){
      data <- filter(gmeans, ma5)
    }
    pos <- table$pos
    if (first) {
      suppressMessages(gmeans_table <- data.frame(treat = as.character(t),
                                                  data = gmeans))
      bottom <- factorial(length(good_treatments)) / (2 *
                                (factorial(length(good_treatments) - 2))) + 3
      # limit place for ks-text to max of factorial of 6
      if (bottom > 18){
        bottom=18;
      }
      par(mar = c(bottom, 3, 3, 2))
      plot(data[1:ceiling(xlimit * jump)]~pos[1:ceiling(xlimit * jump)],
           type = "l", col = colors[tr], ylim = c(0, ylimit),
           xlim = c(0, xlimit),
        ylab = paste(feature, " normalized histogram over genotypes",
                    sep = ""), main = paste(feature, " by treatment",
                    sep = ""),
        xlab = "", lwd = 3)
      points(data[1:ceiling(xlimit * jump)]~pos[1:ceiling(xlimit * jump)],
             type = "l", col = colors[tr], lwd = 4)
      first <- 0
    } else {
      gmeans_table <- rbind(gmeans_table, data.frame(treat = t, data = gmeans))
      points(data[1:ceiling(xlimit * jump)]~pos[1:ceiling(xlimit * jump)],
             type = "l", col = colors[tr], lwd = 4)
    }
  }
  # print legend for all genotypes and treatments
  legend(xlimit * 0.6, ylimit * 0.8, legend = c(good_treatments), lty = 1,
         lwd = 5, col = colors, bg = "white", cex = 0.9, y.intersp = 0.7)
  # print values of all ks tests
  line <- 3
  if (length(good_treatments) > 1) {
    for (t2 in 1:(length(good_treatments) - 1)) {
      if (is.na(sum(gmeans_table[gmeans_table$treat == good_treatments[t2],
                                 "data"][1:(xlimit * jump)], na.rm = T))) {
        write(paste("Treatment ", t2, " is empty for this recording.",
                    sep = ""), file = log_file, append = TRUE)
        next
      }
      for (t3 in (t2 + 1):length(good_treatments)) {
        if (is.na(sum(gmeans_table[gmeans_table$treat == good_treatments[t3],
                                   "data"][1:(xlimit * jump)], na.rm = T))) {
          write(paste("Treatment ", t3, " is empty for this recording.",
                      sep = ""), file = log_file, append = TRUE)
          next
        }

        w <- ks.test(gmeans_table[gmeans_table$treat == good_treatments[t2],
                     "data"][1:(xlimit * jump)],
                     gmeans_table[gmeans_table$treat == good_treatments[t3],
                     "data"][1:(xlimit * jump)],
                     alternative = "two.sided")
        write(paste("Kolmogorov-Smirnov test p.value for treatments ",
                    good_treatments[t2], " vs. ", good_treatments[t3], " : ",
                    format(w$p.value, digits = 2), ", for: ", feature, " max ",
                    xlimit, " seconds", sep = ""), file = log_file,
              append = TRUE)
        mtext(side = 1, at = 0, line = line, text = paste("K-S test for ",
                    good_treatments[t2], " vs. ", good_treatments[t3], " : ",
                    format(w$p.value, digits = 2), ",  for: ", feature,
                    sep = ""), col = "black", cex = 0.9, adj = 0)
        line <- line + 1
      }
    }
  }
  # write all distributions for a permutation test
  table_path <- paste(outputdir, "/", basename, "_", feature,
                      "_distributions.csv", sep = "")
  csvwell <- paste(outputdir, "/", get_project_plate_name(s$file), "_",
                   time_stamp, "_", feature, "_distributions.csv", sep = "")
  write.table(all_distributions, file = csvwell, sep = ",", append = T,
              col.names = F, row.names = F)
}

# Trim trailing spaces
.trim_trailing <- function(x) sub("\\s+$", "", x)

# Perform permutations on EMD and maxdist tests of a given dat
dist_perm <- function(datafile, np, type, kotype) {
  # read in and parse data
  data <- read.csv(datafile, header = FALSE)
  data[, 1] <- .trim_trailing(as.character(data[, 1]))
  data <- data[data[, 1] == type | data[, 1] == kotype, ]
  phenotype <- data[, 1]
  wells <- data[, 2]
  value <- data[, 3:dim(data)[2]]
  value <- as.matrix(value)

  # average data by wells
  well_values <- unique(wells)

  # find out unique Kockout wells
  mt <- unique(wells[which(phenotype != type)])

  # generate well level phenotype
  phenotype <- rep(type, length(well_values))

  for (i in 1:length(mt)) {
    phenotype[which(well_values == mt[i])] <- kotype
  }

  # prepare output and sampling parameters
  n_wt <- length(which(phenotype == type))
  n <- length(phenotype)
  outp <- matrix(0, np, 1)
  out_emd <- matrix(0, np, 1)

  # permute
  for (i in 1:np) {
    if (i %% 100 == 0) {
      cat(paste(i, " permutations\n", sep = ""))
    }
    wt <- sample(n, n_wt)
    wt_e <- which(data[, 2] %in% well_values[wt])
    data_wt <- cumsum(colMeans(value[wt_e, ]))
    data_ko <- cumsum(colMeans(value[- wt_e, ]))

    outp[i] <- max(abs(data_wt - data_ko))
    data_wt_original <- colMeans(value[wt_e, ])
    data_wt_original[is.na(data_wt_original)] <- 0
    data_ko_original <- colMeans(value[- wt_e, ])
    data_ko_original[is.na(data_ko_original)] <- 0
    out_emd[i] <- emd(matrix(c(data_wt_original, 1:length(data_wt_original)),
              ncol = 2), matrix(c(data_ko_original,
                                  1:length(data_wt_original)), ncol = 2))
  }
  outp <- sort(outp)
  out_emd <- sort(out_emd)

  # now figure out the distance from data
  wt <- which(phenotype == type)
  wt_e <- which(data[, 2] %in% well_values[wt])
  data_wt <- cumsum(colMeans(value[wt_e, ]))
  data_ko <- cumsum(colMeans(value[- wt_e, ]))
  data_p <- max(abs(data_wt - data_ko))
  data_wt_original <- colMeans(value[wt_e, ])
  data_wt_original[is.na(data_wt_original)] <- 0
  data_ko_original <- colMeans(value[- wt_e, ])
  data_ko_original[is.na(data_ko_original)] <- 0
  data_emd <- emd(matrix(c(data_wt_original, 1:length(data_wt_original)),
              ncol = 2), matrix(c(data_ko_original,
                                  1:length(data_wt_original)), ncol = 2))

  # compute permutaton p-value
  perm_p <- length(which(outp < data_p)) / np
  perm_emd <- length(which(out_emd < data_emd)) / np
  result <- list(data_emd = data_emd, data_p = data_p, perm_emd = perm_emd,
              perm_p = perm_p, outp = outp, out_emd = out_emd,
              data_wt = data_wt, data_ko = data_ko, data_wt_original =
              data_wt_original, data_ko_original = data_ko_original)
  return(result)
}
