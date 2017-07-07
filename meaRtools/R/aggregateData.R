############################################################################### 
# Purpose:  Functions for aggregating data from s objects into single         #
#                 dataframes per feature                                      #
# Author:   Ryan Dhindsa                                                      #
############################################################################### 

# These functions take data from S object to make dataframes
.write_spike_summary <- function(s) {
  # Creates a list of dataframes for spike features. Each df corresponds to
  #     a single DIV and contains values for each feature
  # 
  # Args:
  #   s object
  # 
  # Returns:
  #   list of data frames containing spike data

  # initiate empty list to store dataframes
  divs_df = list()

  # loop through DIVs in s object and create 1 df per DIV. Each df gets stored in divs_df
  for (i in 1:length(s)) {
    div <- paste("DIV", .get_div(s[[i]]), sep = "")

    df <- .spike_summary_by_well(s[[i]])
    df = cbind(rownames(df), df)
    df = as.data.frame(unclass(df)) # convert strings to factors

    colnames(df)[1] <- "well"
    divs_df[[div]] = df
  }

  divs_df <- do.call(rbind, lapply(names(divs_df), function(x) cbind(div = x, divs_df[[x]])))
  return(divs_df)
}

.compile_ns <- function(s, nspikes) {
  # Calculates network spikes
  # Called from .write_network_spike_summary

  active_wells <- active_wells_network_spikes(nspikes)$ns.all
  if (length(active_wells) > 0) {
    newcol <- 3
    # 2 to peak.min and peak.max
    p <- length(active_wells[[1]]$brief) + length(active_wells[[1]]$mean) + newcol
    nsdata <- matrix(0, length(s$well), p)
    temp <- c()
    length_temp_mean <- length(active_wells[[1]]$mean)
    for (j in 1:length(s$well)) {
      cur_well <- s$well[j]
      if (is.element(cur_well, names(active_wells))){
        temp <- active_wells[[cur_well]]
        nsdata[j, 1:length(temp$brief)] <- temp$brief
        nsdata[j, length(temp$brief) + 1] <- min(temp$measures[, "peak.val"])
        nsdata[j, length(temp$brief) + 2] <- max(temp$measures[, "peak.val"])
        nsdata[j, length(temp$brief) + 3] <- s$treatment[cur_well]
        nsdata[j, (length(temp$brief) + newcol + 1):p] <- as.double(temp$mean)

      } else {
        temp$brief <- c(0, rep(NA, 4), 0, NA, NA)
        nsdata[j, 1:length(temp$brief)] <- temp$brief
        nsdata[j, length(temp$brief) + 1] <- NA
        nsdata[j, length(temp$brief) + 2] <- NA
        nsdata[j, length(temp$brief) + 3] <- NA
        nsdata[j, (length(temp$brief) + newcol + 1):p] <- rep(0, length_temp_mean)
      }
    }

    nsdata <- data.frame(nsdata)
    names(nsdata)[1:length(temp$brief)] <- names(active_wells[[1]]$brief)
    names(nsdata)[(length(temp$brief) + 1):(length(temp$brief) + newcol)] <- c("peak.min", "peak.max", "treatment")

    for (j in 1:(p - length(temp$brief) - newcol)) {
      names(nsdata)[j + newcol + length(temp$brief)] = paste("t", j, sep = "")
    }
    nsdata <- cbind(s$well, nsdata)
    names(nsdata)[1] <- "well"

    return(nsdata)
  }
}
.write_network_spike_summary <- function(s, parameters) {
  # Creates a list of dataframes for network spike features. Each df corresponds to
  #     a single DIV and contains values for each feature
  # 
  # Args:
  #   s object
  # 
  # Returns:
  #   list of data frames containing spike data

  # initiate empty list to store dataframes
  divs_df = list()

  # calculate network spikes
  for (i in 1:length(s)) {
    div <- paste("DIV", .get_div(s[[i]]), sep = "")

    nspikes_old <- calculate.network.spikes(s[[i]], parameters$sur, parameters$ns.N, parameters$ns.T)
    nspikes <- summarize.network.spikes(s[[i]], nspikes_old, ns.E = 1, parameters$sur)
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]

    df = .compile_ns(s[[i]], nspikes)
    df = as.data.frame(unclass(df)) # convert strings to factors

    df <- df[, - grep("^t[0-9]", colnames(df))]

    divs_df[[div]] = df
  }

  for (name in names(divs_df)) {
    # If no data for DIV - remove the DIV
    if (length(divs_df[[name]]) == 0) {
      divs_df[[name]] <- NULL
    }
  }

  divs_df <- do.call(rbind, lapply(names(divs_df), function(x) cbind(div = x, divs_df[[x]])))

  return(divs_df)
}

.write_burst_summary <- function(s) {
  # Creates a list of dataframes for bursting  features. Each df corresponds to
  #     a single DIV and contains values for each feature
  # 
  # Args:
  #   s object
  # 
  # Returns:
  #   list of data frames containing spike data

  masterSum <- .get_burst_info_averaged_over_well(s)

  divs_df = list()
  for (i in 1:length(s)) {
    div <- paste("DIV", .get_div(s[[i]]), sep = "")
    basename <- get_file_basename(s[[i]]$file)

    ########## data frame summarized over well
    # get number of object in masterSum[[1]] list
    tempdf <- c(); tempcolnames <- c()
    for (j in 2:length(masterSum[[i]])) {
      tempc <- unlist(masterSum[[i]][j])
      tempdf <- cbind(tempdf, tempc)
      tempcolnames <- c(tempcolnames, names(masterSum[[i]][j]))
    } # end of loop through masterSum list objects

    # need to switch around columns so first columns come first
    if (dim(tempdf)[2] > 20) { # for now
      if (dim(tempdf)[1] == 1) {
        df <- cbind(t(tempdf[, 21:25]), t(tempdf[, 1:20]))
      } else {
        df <- cbind(tempdf[, 21:25], tempdf[, 1:20])
      }
      colnames <- c(tempcolnames[21:25], tempcolnames[1:20])
      colnames(df) <- colnames
    }

    df = as.data.frame(unclass(df)) # convert strings to factors
    df$file = NULL
    row.names(df) = df$well
    divs_df[[div]] = df
  }

  divs_df <- do.call(rbind, lapply(names(divs_df), function(x) cbind(div = x, divs_df[[x]])))
  return(divs_df)
}

.create_feat_df <- function(s, df, feature, all_feat_list) {
  #   feat_df <- dcast(df, well+treatment~treatment, div, value.var = feature)
  #   all_feat_list[[feature]] = feat_df

  # test
  df = df[!is.na(df$treatment) & df$treatment != "", ]
  x = data.frame(df$div, df$well, df$treatment, df[, feature])
  colnames(x) = c("div", "well", "treatment", feature)
  y <- dcast(x, well~div, value.var = feature)
  well_to_treatment = x[!duplicated(x$well), c("well", "treatment")]
  # reorder in case early wells show up in later DIVs
  well_to_treatment = well_to_treatment[order(as.character(well_to_treatment$well)), ]


  ymerged = merge(x = y, y = well_to_treatment, by = c("well"), all.x = TRUE)
  ymerged = ymerged[order(as.character(ymerged$well)), ]
  y <- ymerged[c(1, dim(ymerged)[2], 2:(dim(ymerged)[2] - 1))]

  y <- .sort_df(y)
  return(y)
}

.sort_df <- function(df) {
  # natural order sorting
  df_divs <- df[3:ncol(df)]
  df_sorted <- df_divs[, mixedorder(names(df_divs)), drop = FALSE]
  df_sorted = cbind(treatment = df$treatment, df_sorted)
  df_sorted = cbind(well = df$well, df_sorted)
  return(df_sorted)
}

aggregate_features <- function(s, feat_type, parameters=list()) {

  # Takes in s object and creates a dataframe for each feature.
  #     based on the feature type (spikes, ns, etc), it calls appropriate function
  #     (e.g. .write_spike_summary if feat_type is "spikes")
  # 
  # Args:
  #   s object
  #   feat_type = "spike", "ns", or "burst"
  # 
  # Returns:
  #   list of data frames (one df per feature)

  platename <- get_project_plate_name(s[[1]]$file)

  # write feature summaries (calls xxx.summary.by.well from meaRtools)
  if (feat_type == "spike") {
    divs_df = .write_spike_summary(s)
  } else if (feat_type == "ns"){
    divs_df = .write_network_spike_summary(s, parameters)
  } else if (feat_type == "burst") {
    divs_df = .write_burst_summary(s)
    divs_df$size <- NULL;
    divs_df$dose <- NULL;
  }


  all_features = list()

  if (!is.null(divs_df)){
    # create list of dataframes (one dataframe per feature)
    feature_names <- colnames(divs_df)
    remove <- c("div", "treatment", "well")
    feature_names <- setdiff(feature_names, remove)

    # test

    for (i in 1:length(feature_names)) {
      df = .create_feat_df(s, divs_df, feature_names[i], all_features)

      all_features[[feature_names[i]]] = df}
  } else {
    all_features = NULL
  }

  return(all_features)
}

filter_wells <- function(unfiltered_df, nae, min_electrodes = 4, well_filter_maximum_DIV_inactive_ratio = 0.5) {
  # Filters out wells in which there are fewer than 4 active electrodes
  #    at least 70% of the time
  unfiltered_df = unfiltered_df[!(is.na(unfiltered_df$treatment) | unfiltered_df$treatment == ""), ] # remove wells w/o trt

  nae$treatment = NULL
  nae[- 1] <- sapply(nae[- 1], as.numeric)

  num.div <- ncol(nae) - 1

  inactive <- data.frame(num.inactive = rowSums(nae[, - 1] < min_electrodes), total.div = num.div)
  inactive[is.na(inactive$num.inactive), "num.inactive"] = 0
  inactive$fraction <- inactive$num.inactive / inactive$total.div
  inactive$well <- nae$well

  # grab only wells with inactive ratio < well_filter_maximum_DIV_inactive_ratio
  active_wells <- with(inactive, {subset.data.frame(inactive, fraction < well_filter_maximum_DIV_inactive_ratio, select = well)})

  filtered_df = unfiltered_df[unfiltered_df$well %in% active_wells$well, ]

  if (nrow(filtered_df) != 0) 
  {
    # replace na's with 0's
    filtered_df[filtered_df == "NaN"] = NA # first replace NaN with NA
    filtered_df[is.na(filtered_df)] <- 0 # then replace NA's with 0
  }

  return(filtered_df)
}

write_features_to_files <- function(s, features_list, output_dir, type) {
  # Takes in list of dataframes (one per feature) and writes out each
  #     df to a csv file
  # 
  # Args:
  #   s object
  #   features_list = list of dataframes
  #   output_dir = directory where files will be put (will make separate folders
  #                  ns, spikes, and bursts)
  # 
  # Returns:
  #   one csv per feature

  # change to create subdir for each file type
  platename <- get_project_plate_name(s[[1]]$file)
  out_folder <- paste0(output_dir, "/", type)
  dir.create(out_folder, showWarnings = FALSE)
  invisible(sapply(names(features_list),
    function(x) write.csv(features_list[[x]], file = paste0(out_folder, "/", platename, "_", x, ".csv"),
      row.names = F)))
}
