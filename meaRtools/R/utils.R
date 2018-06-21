
get_file_basename <- function(filename) {
  tempname <- strsplit(basename(filename), ".RData")[[1]][1]
  basename <- paste(strsplit(tempname, "_")[[1]][1],
                    strsplit(tempname, "_")[[1]][2],
    strsplit(tempname, "_")[[1]][3], strsplit(tempname, "_")[[1]][4],
    sep = "_")
  basename
}

get_project_plate_name <- function(file) {
  tempname <- strsplit(basename(file), ".RData")[[1]][1]
  basename <- paste(strsplit(tempname, "_")[[1]][1],
                    strsplit(tempname, "_")[[1]][2],
    strsplit(tempname, "_")[[1]][3], sep = "_")
  basename
}

.get_all_electrodes <- function(r) {
  plate <- get_plateinfo(r$layout$array)
  wells <- as.matrix(sort(plate$wells))
  result <- as.vector(apply(wells, c(1, 2), function(well) {
    .get_electrode_layout(r, well)$electrodes
  }))
  result
}

.get_electrode_layout <- function(r, well) {
  plateinfo <- get_plateinfo(r$layout$array)
  d1 <- expand.grid(col = 1:plateinfo$n_elec_c, row = 1:plateinfo$n_elec_r)
  electrodes <- sort(paste(well, "_", d1[, "row"], d1[, "col"], sep = ""))
  layout <- c(plateinfo$n_elec_r, plateinfo$n_elec_c)
  return(list(electrodes = electrodes, layout = layout))
}
