## General functions useful for processing the Axion data.

## This variable stores all the information related to the wells; typically this
## is accessed through the plateinfo arrayname function.

.axion_plateinfo <- list("Axion 48 well" = list(
  n_well = 48,
  wells = paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
  n_well_r = 6,
  n_well_c = 8,
  layout = c(8, 6),
  n_elec_r = 4,
  n_elec_c = 4),
"Axion 12 well" = list(
  n_well = 12,
  wells = paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
  n_well_r = 3,
  n_well_c = 4,
  layout = c(4, 3),
  n_elec_r = 8,
  n_elec_c = 8))


.plateinfo <- function(arrayname) {
  ## Return useful information related to arrayname
  ##
  ## plateinfo "Axion 12 well"
  res <- .axion_plateinfo[[arrayname]]
  if (is.null(res)) {
    stop("arrayname not recognised:", arrayname)
  } else {
    res
  }
}

.axion_elec_name_to_xy <- function(name, plateinfo) {
  ## Convert electrode name to  (x,y) position.
  ## plateinfo stores all the information about the plates.
  ## and hence the well layout of the plate.

  max_well_row <- plateinfo$n_well_r
#  max_well_col  plateinfo$n_well_c
  max_elec_row <- plateinfo$n_elec_r
  max_elec_col <- plateinfo$n_elec_c


  well_r <- max_well_row - match(substring(name, 1, 1), LETTERS)
  well_c <- as.integer(substring(name, 2, 2)) - 1
  elec_r <- as.integer(substring(name, 5, 5)) - 1
  elec_c <- as.integer(substring(name, 4, 4)) - 1

  gap <- 1
  spacing <- 200 # electrode spacing.
  well.wid <- (max_elec_col + gap) * spacing
  well.ht <- (max_elec_row + gap) * spacing

  x <- (well_c * well.wid) + (elec_c * spacing)
  y <- (well_r * well.ht) + (elec_r * spacing)

  cbind(x, y)

}

.axion_spike_sum <- function(spikes) {
  ## Generate a simple summary of the spikes list.
  ## This version returns a vector, rather than a string.  This is more
  ## useful for building a data frame of values.
  len <- length(spikes)
  all_range <- sapply(spikes, range)
  nspikes <- sum(sapply(spikes, length))
  min <- min(all_range[1, ])
  max <- max(all_range[2, ])
  str <- sprintf("summary: %d electrodes %d spikes, min %.4f max %.4f",
    len, nspikes, min, max)
  ## str
  c(nelectrodes = len, nspikes = nspikes, time.min = min, time.max = max)
}

.axion_spikes_to_df <- function(spikes) {
  ## Convert a list of spikes to a two column  elec, time data frame
  names <- names(spikes)
  names(spikes) <- NULL
  nspikes <- sapply(spikes, length)
  data.frame(elec = rep(names, times = nspikes), time = unlist(spikes))
}


.axion_guess_well_number <- function(channels) {
  ## Given the channel names, guess the number of wells on the plate.
  ## This works on the logic that certain electrode names will only be
  ## found on certain plates. e.g. the electrode name "D6_33" can only appear
  ## on a well with 48 arrays.
  ##
  ## .axion_guess_well_number "D3_33"  ## should be 48.
  ## .axion_guess_well_number "B3_53"  ## should be 12
  ## .axion_guess_well_number "A2_11" ## this is ambiguous.

  well_r <- match(substring(channels, 1, 1), LETTERS)
  well_c <- as.integer(substring(channels, 2, 2))
  elec_r <- as.integer(substring(channels, 5, 5))
  elec_c <- as.integer(substring(channels, 4, 4))

  max_well_r <- max(well_r)
  max_well_c <- max(well_c)

  max_elec_r <- max(elec_r)
  max_elec_c <- max(elec_c)

  nplates <- length(.axion_plateinfo)
  well <- 0
  for (i in 1:nplates) {
    plateinfo <- .axion_plateinfo[[i]]
    if (max_well_r <= plateinfo$n_well_r &&
      max_well_c <= plateinfo$n_well_c &&
      max_elec_r <= plateinfo$n_elec_r &&
      max_elec_c <= plateinfo$n_elec_c) {
      well <- plateinfo$n_well
      break;
    }
  }
  if (well == 0) {
    stop("Cannot guess number of wells on plate.")
  }
  well
}

.axion_electrodes_on_well <- function(well, electrodes) {
  ## Return names of electrodes that are on well WELL.
  matches <- grep(well, electrodes)
  electrodes[matches]
}

.axion_elec_to_well <- function(elec) {
  ## Extract well name from ELECtrode name.
  substring(elec, 1, 2)
}

.get_array_info <- function(data) {
# Array-specific information, maybe this could go in a file, rather
# than be read-in separately.
  pos <- data$epos;  rownames(pos) <- data$names
  array <- data$array

  if (any(grep("^Axion", array))) {
    ## e.g. Neurotox ongoing project.
    xlim <- c(0, 8000)
    ylim <- c(0, 6000)
    spacing <- 200
    corr_breaks <- 0 # TODO; by default, do no breaks!
  }

  array <- as.character(array)
  layout <- list(xlim = xlim, ylim = ylim, spacing = spacing,
    pos = pos, array = array)
  class(layout) <- "mealayout"
  list(layout = layout, corr_breaks = corr_breaks)
}

.filter_channel_names <- function(spikes, ids) {
  ## Filter out some channel names.
  ## Keep only the channels mentioned in IDS.
  ## If the elements of IDS are numeric, they are assumed to be the
  ## indexes of the spike trains; otherwise, they are assumed to be the
  ## names of cells.

  if (any(is.character(ids)))
    ids <- .names_to_indexes(names(spikes), ids)

  spikes[ids]
}

.names_to_indexes <- function(names, elems, allow_na=FALSE, allow_regex=TRUE) {
  ## Return the indexes of where each element of elems is within names.
  ## If the first element of elems is a dash sign,
  ## then return all indexes except those matching elems.
  ## If elems is NULL, then 1 to n is returned, where n is the length of NAMES.

  ## to check if first element is a dash sign, we have to use this more
  ## complex expression, as elems 1 equals dash sign is
  ## an error if the first element by chance is NA.
  if (is.null(elems)) {
    return(1:length(names))
  }
  if (isTRUE(all.equal("-", elems[1]))) {
    invert <- TRUE
    elems <- elems[- 1]
  } else {
    invert <- FALSE

  }

  indexes <- match(elems, names)


  if (allow_regex) {
    ## see if any elements returned NA, in which case try them individually
    ## as regular expressions.
    which_na <- which(is.na(indexes))
    if (any(which_na)) {
      regex_elems <- elems[which_na]
      new_indexes <- lapply(regex_elems, function(r){
        grep(r, names)
      })
      new_indexes <- unique(unlist(new_indexes))
      indexes <- indexes[- which_na]
      indexes <- c(indexes, new_indexes) # TODO, preserve order?
    }
    allow_na <- TRUE # allow NAs through now.
  }

  if (!allow_na) {
    if (any(is.na(indexes)))
    stop("some indexes not found.")
  }

  if (invert)
    indexes <- setdiff(1:(length(names)), indexes)

  indexes

}
