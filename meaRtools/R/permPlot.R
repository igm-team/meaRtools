###############################################################################
# Purpose:  Functions for computing p-values via mann whit and permut. test   #
#              as well as generating plots                                    #
# Author:   Ryan Dhindsa                                                      #
###############################################################################
.extract_rows <- function(data, type) {
  # Extracts rows for particular treatment
  #
  # Args:
  #   data: a dataframe
  #   type: treatment
  #
  # Returns:
  #   list of data frames containing spike data
  x <- with(data, data[treatment == type & !is.na(treatment), ])
  x$treatment <- NULL
  x$well <- NULL

  return(x)
}

.mann_whit_perm <- function(df, wt, trt, np) {
  # Calculates mann-whit p-value and permutation test for a dataframe
  #     must specify wt and the treatment you are testing
  #
  # Args:
  #   df = a dataframe with featre data
  #   wt = the treatment that will be considred wt
  #   np = number of permutations
  #
  # Returns:
  #   a df that contains a permutation p-value and a mann whit p-value
  wt_df <- .extract_rows(df, wt)
  if (nrow(wt_df) > 0) {
    wt_df[wt_df == "NaN"] <- NA
  }

  trt_df <- .extract_rows(df, trt)
  if (nrow(trt_df) > 0) {
    trt_df[trt_df == "NaN"] <- NA # convert NaN to NA
  }

  if (is.na(trt) | is.na(wt) | nrow(wt_df) == 0 | nrow(trt_df) == 0 |
      all(is.na(trt_df)) | all(is.na(wt_df))) {
    return(data.frame(perm_p = NA, data_p = NA))
  }

  # pool wt and trt data
  pool <- rbind(wt_df, trt_df)
  pool <- as.matrix(pool)

  n_wt <- nrow(wt_df)
  n <- nrow(pool)

  # randomly sample the data, compute p-values and store in outp
  outp <- matrix(0, np, 1)
  for (i in 1:np) {
    wt_smpl <- sample(n, n_wt)
    perm_wt <- as.numeric(as.vector(pool[wt_smpl, ]))
    perm_trt <- as.numeric(as.vector(pool[- wt_smpl, ]))
    outp[i] <- wilcox.test(perm_wt, perm_trt)$p.value
  }
  outp <- sort(outp)

  # calculate actual p-val from data
  data_wt <- as.numeric(unlist(wt_df))
  data_trt <- as.numeric(unlist(trt_df))
  data_p <- wilcox.test(data_wt, data_trt)$p.value
  if (data_p == "NaN"){
    perm_p <- "NaN"
  } else {
    perm_p <- length(which(outp < data_p)) / np
    if (perm_p == 0) {
      perm_p <- paste("<", 1 / np)
    }
    data_p <- signif(data_p, 3)
  }
  return(data.frame(perm_p = perm_p, data_p = data_p))
}

get_wt <- function(s) {
  # Uses tcltk user input to specify which treatment should be considered wt
  #     later on, we calculate p-value for every other treatment vs.
  #     this wt specification
  #
  # Args:
  #   s object
  #
  # Returns:
  #   wt
  choices <- as.vector(unique(s[[1]]$treatment[!is.na(s[[1]]$treatment) &
                                                 s[[1]]$treatment != ""]))
  wt <- tk_select.list(choices, preselect = NULL, multiple = FALSE,
    title = "Choose wildtype/reference for permutation test")
  return(wt)
}

.plot_feature <- function(df, feature, platename) {
  # Uses ggplot to plot feature data
  #
  # Args:
  #   df = a datafraem containing feature data
  #   feature = name of feature
  #   platename
  #
  # Returns:
  #   plot


  df$well <- NULL
  df[, - 1] <- sapply(df[, - 1], as.numeric)

  melted_df <- melt(df, id.vars = "treatment")
  melted_df <- melted_df[!is.na(melted_df[, "value"]), ]
  # melted_df = ddply(melted_df, .(treatment, variable), summarize,
  #                  mean=mean(value),
  #                  sd = sd(value))
  melted_df <- with(melted_df, {
    ddply(melted_df, c("treatment", "variable"), summarize,
    mean = mean(value),
    sem = sqrt(var(value) / length(value)))
  })


  pd <- position_dodge(width = 0.1)
  title <- paste(platename, "_", feature, sep = "")

  # no values for feature, return empty plot
  if (all(is.na(melted_df$mean))){
    x <- with(melted_df, {
      ggplot() +
      ggtitle(paste0("\n", title, "\n")) +
      xlab("") +
      ylab(paste0("\n", feature, "\n"))
    })
  } else {
    x <- with(melted_df, {
             ggplot(melted_df, aes(x = variable, y = mean, group = treatment)) +
      geom_point(aes(color = factor(treatment)), position = pd) +
      geom_line(aes(color = factor(treatment)), position = pd) +
      geom_errorbar(aes(x = variable, ymin = mean - sem, ymax = mean + sem,
        color = factor(treatment)), width = 0.1, position = pd) +
      ggtitle(paste0("\n", title, "\n")) +
      xlab("") +
      ylab(paste0("\n", feature, "\n"))
    })
  }

  x + labs(color = "Treatment")

  return(x)
}

.apply_perm_and_plot <- function(wt, df, np, feature, platename) {
  # Calls .mann_whit_perm() and .plot_feature() to create a
  # single gtable containing the plot and table
  #
  # Args:
  #   wt, df, feature, and platename
  #
  # Returns:
  #   gtable

  all_treatments <- as.vector(unique(df$treatment))
  max(all_treatments, na.rm = TRUE) # remove non-existent treatments

  # extract all treatments that aren't wt
  test_treatments <- all_treatments[!all_treatments == wt]

  # calculate mann whit p values for each combination
  num_of_trts <- length(test_treatments)

  all_p_values <- data.frame(Treatment = character(),
    perm_pval = character(),
    MW.pval = character(),
    stringsAsFactors = FALSE)

  if (num_of_trts == 0) {
    all_p_values[1, ] <- c(wt, - 1, - 1)
  } else {
    for (i in 1:num_of_trts) {
      trt <- test_treatments[i]
      # use suppress to prevent the "not exact p-value" in wilcox-test
      vals <- suppressWarnings(.mann_whit_perm(df, wt, trt, np))
      all_p_values[i, "Treatment"] <- paste(wt, " vs. ", trt)
      all_p_values[i, "perm_pval"] <- as.character(vals[, "perm_p"])
      all_p_values[i, "MW.pval"] <- vals[, "data_p"]
    }}

  names(all_p_values)[1] <- paste("Treatment/Genotype")

  p_value_table <- gridExtra::tableGrob(all_p_values)
  feature_plot <- meaRtools:::.plot_feature(df, feature, platename)

  table_and_plot <- gridExtra::arrangeGrob(feature_plot,
                                           p_value_table, nrow = 2)
  return(table_and_plot)
}

permute_features_and_plot <- function(s, wt, np, features_list, type, output_dir) {
  # Calls .apply.perm.and.plot() and writes PDF--each page contains a plot and
  #       table of p-values
  #
  # Args:
  #   s object
  #   wt
  #   features_list = list of dataframes containing feature data
  #   type = spikes, ns, or bursts
  #
  #
  # Returns:
  #   Writes a PDF

  out_folder <- paste0(output_dir, "/", type)
  dir.create(out_folder, showWarnings = FALSE)

  platename <- get_project_plate_name(s[[1]]$file)
  fname <- paste(platename, "_", type, "_", "analysis", ".pdf", sep = "")
  fpath <- paste(out_folder, "/", fname, sep = "")

  if ((0 == nrow(features_list[[1]]))) {
    # empty wells
    return(NULL)
  }
  if (length(unique(na.omit(features_list[[1]]$treatment))) == 0) {
    # No treatments
    return(NULL)
  }

  x <- list()
  for (i in 1:length(features_list)) {
    feature <- names(features_list[i])
    perm_and_plot <- .apply_perm_and_plot(wt,
                       features_list[[i]], np, feature, platename)
    x[[feature]] <- perm_and_plot
  }


  all_plots <- gridExtra::marrangeGrob(x, nrow = 1, ncol = 1)

  # create pdf
  ggsave(fpath, all_plots, width = 8.5, height = 11, units = "in")
}
