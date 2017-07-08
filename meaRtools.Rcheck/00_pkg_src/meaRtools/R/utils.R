
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
  plate <- .plateinfo(r$layout$array)
  wells <- as.matrix(sort(plate$wells))
  result <- as.vector(apply(wells, c(1, 2), function(well) {
    .get_electrode_layout(r, well)$electrodes
  }))
  result
}

.get_electrode_layout <- function(r, well) {
  plateinfo <- .plateinfo(r$layout$array)
  d1 <- expand.grid(col = 1:plateinfo$n_elec_c, row = 1:plateinfo$n_elec_r)
  electrodes <- sort(paste(well, "_", d1[, "row"], d1[, "col"], sep = ""))
  layout <- c(plateinfo$n_elec_r, plateinfo$n_elec_c)
  return(list(electrodes = electrodes, layout = layout))
}


igm_write_ui_to_log <- function(files=NULL, parameter_list, new_file=F) {

  if (new_file) {
    for (i in 1:length(files)) {
      cur_file <- files[i]
      write(file = files[i], " ", append = F)
    } # end of for
  }

  if (!is.null(files)) {

    for (i in 1:length(files)) {
      cur_file <- files[i]

      # write params
      for (j in 1:length(parameter_list)) {
        write(file = cur_file, " ", append = T)
        if (length(parameter_list[[j]]) > 1){
          write(file = cur_file, names(parameter_list)[j], append = T)
          for (d in 1:length(parameter_list[[j]])) {
            write(file = cur_file, paste(names(parameter_list[[j]])[d], " = ",
              parameter_list[[j]][d]), append = T)
          } # end for
        } else {
          write(file = cur_file, paste(names(parameter_list)[j], " = ",
            parameter_list[j]), append = T)
        }
      } # end of for legnth param list


    } # end of for lenght(files)

  } # end of if(!is.null(files))
} # end of igm_write_ui_to_log
