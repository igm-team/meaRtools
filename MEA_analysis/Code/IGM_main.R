options(warn = -1)
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(plyr)))
suppressWarnings(suppressMessages(library(meaRtools)))

#   	   program  : IGM_main.R
# 			purpose: this program now takes apart spike_lists and makes
#       them into data csv's
#

if (!interactive()){
  args <- commandArgs(trailingOnly = FALSE)
  path <- dirname(strsplit(args[4], "=")[[1]][2])
  setwd(dirname(path))
} else {
  #add these just for running code in R
  path <- paste(dirname(getwd()), "/MEA_analysis/Code", sep = "")
  setwd(dirname(path))
}
# Load parameters, called 'parameters'
source(paste(path, "/parameters.r", sep = ""), echo = FALSE)

if (!exists("parameters")) {
  stop("missing parameters file in 'MEA_Analsyis/Code/parameters.R' ")
}


#detect parameter file added or add data
if (interactive()){
  spk_list_files <- get_data(caption = "Choose spike list file(s)")
  experimental_log_file <- get_data(caption = "Choose experimental log file")

}

cat("\n")
cat("Setting directories.\n")
#determine directory for R code and load function bank

data_dir <- dirname(spk_list_files[1])
#directory of Robjects files
output_dir <- paste0(data_dir, "/Analysis")
suppressWarnings(dir.create(output_dir))
output_per_div_dir <- paste0(data_dir, "/Analysis/outputPerDIV")
suppressWarnings(dir.create(output_per_div_dir))
r_object_dir <- paste0(data_dir, "/Analysis/R_Objects")
suppressWarnings(dir.create(r_object_dir))
log_dir <- paste0(output_dir, "/LogFiles")
suppressWarnings(dir.create(log_dir))

analysis <- list(spikeFiles = spk_list_files,
               output_dir = output_dir,
               Routput_dir = r_object_dir,
               output_per_div_dir = output_per_div_dir
               )

write.table(as.matrix(parameters),
            file = paste0(log_dir, "/parameters_",
                          parameters$timeStamp, ".txt"),
            quote = F,
            col.names = F)
l <- length(spk_list_files)
file_num <- 1
for (i in 1:l){
  title <- strsplit(basename(spk_list_files[i]), ".csv")[[1]][1]
  #get plate chemical info for each file in the list
  plate_chem_info <-
    get_experimental_log_file(spk_list_files[i], experimental_log_file)
  if (length(plate_chem_info$well) == 0) {
    # No data in the plate_chem_info
    cat(paste("No data for file", spk_list_files[i],
              "removing file from analysis."))
  }else{
    analysis$Robject[file_num] <-
      read_spikelist(key = title,
                    spk_list_file = spk_list_files[i],
                    chem_info = plate_chem_info,
                    r_object_dir = r_object_dir)
    file_num <- file_num + 1
  }
}

cat("\n")
cat("Making R-objects....\n")
#++++++++++++++++++++++   make s object
s <- calculate_spike_features(analysis$Robject, parameters)

## Calculate mutual information and entropy
if (length(s) > 0) {
  for (i in 1:length(s)){
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste("Calculating STTC and mutual information for ", basename, "...\n"))
    ent_mi_data <- calculate_entropy_and_mi(s[[i]], s[[i]]$treatment, mult_factor=1.5, bin_size=0.1)
    s[[i]]$mutual_inf <- ent_mi_data[["data_dists"]][["MI"]]
    s[[i]]$entropy <- ent_mi_data [["data_dists"]][["ENT"]]
    s[[i]]$mean_sttc <- compute_mean_sttc_by_well(s[[i]])
  }
}

if (length(s) > 0) {
  s <- calculate_burst_features(s)
  for (i in 1:length(s)) {
    #Calculate Network Spikes
    nspikes_old <- calculate_network_spikes(s[[i]],
                  parameters$sur, parameters$ns_n, parameters$ns_t)
    nspikes <- summarize_network_spikes(s[[i]],
                        nspikes_old, ns_e = 1, parameters$sur)
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste("Calculating network spikes for ", basename, "...\n"))

    # plot ns data if there are network spikes
    if (parameters$ns_plot){
      if (has_network_spikes(nspikes)) {
        ns_plot_path <- paste(analysis$output_per_div_dir, "/",
                            basename, "_ns.pdf", sep = "")
        pdf(file = ns_plot_path)
        xyplot_network_spikes(nspikes)
        plot_active_wells_network_spikes(nspikes)
        dev.off()
      }
    }

    # write network spike data to output file
    if (parameters$ns_csv){
      write_network_spikes_to_csv(s[[i]], nspikes,
                      analysis$output_per_div_dir)
    }

    s[[i]]$ns_all <- nspikes$ns_all
    s[[i]] <- calculate_isis(s[[i]])
    s[[i]]$well_stats <- compute_mean_firingrate_by_well(s[[i]])
  }

  # Bursting data - plot and print output csv
  if (parameters$burst_plot){
    batchname <- get_project_plate_name(s[[1]]$file)
    batch_plot_path <- paste(analysis$output_per_div_dir,
              "/", batchname, "_plot.pdf", sep = "")
    pdf(file = batch_plot_path)
    plot_mean_firingrate_by_well_by_div(s)
    plot_mean_firingrate_by_eletrode_by_div(s)
    dev.off()
  }

  #spiking
  if (parameters$spike_plot){
    suppressWarnings(plot_plate_summary_for_spikes(s,
        analysis$output_per_div_dir))
  }
  if (parameters$spike_csv){
    suppressWarnings(write_plate_summary_for_spikes(s,
        analysis$output_per_div_dir))
  }

  # bursting
  if (parameters$burst_plot){
    suppressWarnings(plot_plate_summary_for_bursts(s,
        analysis$output_per_div_dir, parameters))
  }
  if (parameters$burst_csv){
    suppressWarnings(write_plate_summary_for_bursts(s,
          analysis$output_per_div_dir))
  }
} else{
  cat("\n")
  cat("Insufficient data passed filter parameters for analysis.\n")
}

# extract NB features, the parameters are defined
# in parameters.R under NB section
if (parameters$want_nb) {
  if (length(s) > 0) {
    cat("\n")
    cat("calculating network bursts...\n")
    nb_list <- calculate_network_bursts(s, parameters$sigma,
                                       parameters$min_electrodes,
                                       parameters$local_region_min_nae)

    if (!is.null(nb_list$nb_features_merged)){
      nb_features <- nb_matrix_to_feature_dfs(nb_list$nb_features_merged)
    } else{
      nb_features <- c()
    }

    # attach data to s object
    for (i in 1:length(s)){
      s[[i]]$nb_all <- nb_list$nb_all[[i]]
      s[[i]]$data.frame$nb_features <- nb_list$nb_features[[i]]
    }

    nb_file <- paste(analysis$output_per_div_dir, "/",
               get_project_plate_name(s[[1]]$file), "_nb.csv", sep = "")
    write.csv(nb_features, file = nb_file, row.names = FALSE)
  }
}


#+++++++++++++++  save S object
if (length(s) > 0) {
  for (i in 1:length(s)){
    basename <- strsplit(basename(s[[i]]$file), "[.]")[[1]][1]
    cat("\n")
    cat(paste0("Saving file ", basename, "...\n"))
    rm("S")
    S <- s[[i]]
    # Save each s object as S in RData for future use
    save(S, file = paste0(analysis$Routput_dir, "/", basename, ".RData"))
  }
}

# Permutation and plotting
if (length(s) > 0) {
  # aggregate
  cat("\n")
  cat("building feature table...\n")

  spike_features <- aggregate_features(s, "spike", parameters)
  ns_features <- aggregate_features(s, "ns", parameters)
  burst_features <- aggregate_features(s, "burst", parameters)

  #filter
  nae <- spike_features$nae
  nae[is.na(nae)] <- 0

  spike_features <- lapply(spike_features,
    function(x) {
      filter_wells(x, nae, parameters$well_min_rate,
                    parameters$well_max_div_inactive_ratio)
    })
  ns_features <- lapply(ns_features,
    function(x) {
      filter_wells(x, nae, parameters$well_min_rate,
                   parameters$well_max_div_inactive_ratio)
    })
  burst_features <- lapply(burst_features,
    function(x) {
        filter_wells(x, nae, parameters$well_min_rate,
                 parameters$well_max_div_inactive_ratio)
    })

  if (parameters$want_nb){
    nb_features <- lapply(nb_features, function(x) {
      filter_wells(x, nae, parameters$well_min_rate,
                   parameters$well_max_div_inactive_ratio)
      })
    nb_features <- lapply(nb_features, function(x) x[order(x[, "well"]), ])
  }
  #write csvs
  write_features_to_files(s, spike_features, analysis$output_dir, "spikes")
  write_features_to_files(s, ns_features, analysis$output_dir, "ns")
  write_features_to_files(s, burst_features, analysis$output_dir, "bursts")
  if (parameters$want_nb){
    write_features_to_files(s, nb_features, analysis$output_dir, "nb")
  }
}

if (length(s) > 0) {
  cat("\n")
  cat(paste0("calculating permutations, # permutaions ",
             parameters$perm_n, "...\n"))
  # mann whit/perm --> PDF
  if ( !exists("wt") ){wt <- unique(s[[1]]$treatment[s[[1]]$treatment!=""])[1] }
  

  suppressMessages(permute_features_and_plot(s, wt,
            parameters$perm_n, spike_features, "spikes", analysis$output_dir))
  if (length(ns_features) > 0){
    suppressMessages(permute_features_and_plot(s, wt,
            parameters$perm_n, ns_features, "ns", analysis$output_dir))
  }
  if (length(burst_features) > 0){
    suppressMessages(permute_features_and_plot(s, wt,
            parameters$perm_n, burst_features, "bursts", analysis$output_dir))
  }
  if (parameters$want_nb){
    suppressMessages(permute_features_and_plot(s, wt,
            parameters$perm_n, nb_features, "nb", analysis$output_dir))
  }
}

cat("\n")
cat("Done with analysis.\n")