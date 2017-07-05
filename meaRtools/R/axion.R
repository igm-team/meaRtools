## General functions useful for processing the Axion data.

## This variable stores all the information related to the wells; typically this
## is accessed through the plateinfo(arrayname) function.

.axion.plateinfo <- list("Axion 48 well" = list(
  n.well = 48,
  wells = paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
  n.well.r = 6,
  n.well.c = 8,
  layout = c(8, 6),
  n.elec.r = 4,
  n.elec.c = 4),
"Axion 12 well" = list(
  n.well = 12,
  wells = paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
  n.well.r = 3,
  n.well.c = 4,
  layout = c(4, 3),
  n.elec.r = 8,
  n.elec.c = 8))


.plateinfo <- function(arrayname) {
  ## Return useful information related to arrayname
  ## 
  ## plateinfo("Axion 12 well")
  res <- .axion.plateinfo[[arrayname]]
  if (is.null(res)) {
    stop("arrayname not recognised:", arrayname)
  } else {
    res
  }
}

.axion.elec.name.to.xy <- function(name, plateinfo) {
  ## Convert electrode name to  (x,y) position.
  ## plateinfo stores all the information about the plates.
  ## and hence the well layout of the plate.

  max.well.row <- plateinfo$n.well.r
  max.well.col <- plateinfo$n.well.c
  max.elec.row <- plateinfo$n.elec.r
  max.elec.col <- plateinfo$n.elec.c


  well.r <- max.well.row - match(substring(name, 1, 1), LETTERS)
  well.c <- as.integer(substring(name, 2, 2)) - 1
  elec.r <- as.integer(substring(name, 5, 5)) - 1
  elec.c <- as.integer(substring(name, 4, 4)) - 1

  gap <- 1
  spacing <- 200 # electrode spacing.
  well.wid <- (max.elec.col + gap) * spacing
  well.ht <- (max.elec.row + gap) * spacing

  x <- (well.c * well.wid) + (elec.c * spacing)
  y <- (well.r * well.ht) + (elec.r * spacing)

  cbind(x, y)

}

.axion.spikesum2 <- function(spikes) {
  ## Generate a simple summary of the spikes list.
  ## This version returns a vector, rather than a string.  This is more
  ## useful for building a data frame of values.
  len <- length(spikes)
  all.range <- sapply(spikes, range)
  nspikes <- sum(sapply(spikes, length))
  min <- min(all.range[1, ])
  max <- max(all.range[2, ])
  str <- sprintf("summary: %d electrodes %d spikes, min %.4f max %.4f",
    len, nspikes, min, max)
  ## str
  c(nelectrodes = len, nspikes = nspikes, time.min = min, time.max = max)
}

.axion.spikestodf <- function(spikes) {
  ## Convert a list of spikes to a 2-column  (elec, time) data frame.
  names <- names(spikes)
  names(spikes) <- NULL
  nspikes <- sapply(spikes, length)
  data.frame(elec = rep(names, times = nspikes), time = unlist(spikes))
}


.axion.guess.well.number <- function(channels) {
  ## Given the channel names, guess the number of wells on the plate.
  ## This works on the logic that certain electrode names will only be
  ## found on certain plates. e.g. the electrode name "D6_33" can only appear
  ## on a well with 48 arrays.
  ## 
  ## axion.guess.well.number("D3_33")  ## should be 48.
  ## axion.guess.well.number("B3_53")  ## should be 12
  ## axion.guess.well.number("A2_11") ## this is ambiguous.

  well.r <- match(substring(channels, 1, 1), LETTERS)
  well.c <- as.integer(substring(channels, 2, 2))
  elec.r <- as.integer(substring(channels, 5, 5))
  elec.c <- as.integer(substring(channels, 4, 4))

  max.well.r <- max(well.r)
  max.well.c <- max(well.c)

  max.elec.r <- max(elec.r)
  max.elec.c <- max(elec.c)

  nplates <- length(.axion.plateinfo)
  well <- 0
  for (i in 1:nplates) {
    plateinfo <- .axion.plateinfo[[i]]
    if (max.well.r <= plateinfo$n.well.r &&
      max.well.c <= plateinfo$n.well.c &&
      max.elec.r <= plateinfo$n.elec.r &&
      max.elec.c <= plateinfo$n.elec.c) {
      well <- plateinfo$n.well
      break;
    }
  }
  if (well == 0) {
    stop("Cannot guess number of wells on plate.")
  }
  well
}

.axion.electrodes.on.well <- function(well, electrodes) {
  ## Return names of electrodes that are on well WELL.
  matches <- grep(well, electrodes)
  electrodes[matches]
}

.axion.elec2well <- function(elec) {
  ## Extract well name from ELECtrode name.
  substring(elec, 1, 2)
}

.get.array.info <- function(data) {
  ## Array-specific information; maybe this could go in a file, rather
  ## than be read-in separately.  Useful for the HDF5 functions.
  pos <- data$epos;  rownames(pos) <- data$names
  array <- data$array

  if (any(grep("^Axion", array))) {
    ## e.g. Neurotox ongoing project.
    xlim <- c(0, 8000)
    ylim <- c(0, 6000)
    spacing <- 200
    corr.breaks <- 0 # TODO; by default, do no breaks!
  }

  array <- as.character(array)
  layout <- list(xlim = xlim, ylim = ylim, spacing = spacing,
    pos = pos, array = array)
  class(layout) <- "mealayout"
  list(layout = layout, corr.breaks = corr.breaks)
}

.filter.channel.names <- function(spikes, ids) {
  ## Filter out some channel names.
  ## Keep only the channels mentioned in IDS.
  ## If the elements of IDS are numeric, they are assumed to be the
  ## indexes of the spike trains; otherwise, they are assumed to be the
  ## names of cells.
  ## e.g.
  ## spikes2 <- .filter.channel.names(spikes, c('-', 'g4a', 'a6a'))
  ## spikes2 <- .filter.channel.names(spikes, c('g4a', 'a6a'))
  ## spikes2 <- .filter.channel.names(spikes, c(5, 3, 1))
  ## first call throws away two channels; second call keeps just two channels.
  ## third just keeps the three trains mentioned.

  if (any(is.character(ids)))
    ids = .names.to.indexes(names(spikes), ids)

  spikes[ids]
}

.names.to.indexes <- function(names, elems, allow.na=FALSE, allow.regex=TRUE) {
  ## Return the indexes of where each element of ELEMS is within NAMES.
  ## If the first element of ELEMS is '-', then return all indexes except
  ## those matching ELEMS.  If ELEMS is NULL, then 1:n is returned, where n is
  ## the length of NAMES.
  ## Example:
  ## names = c('a', 'b', 'c', 'd', 'e')
  ## .names.to.indexes(names, c('d', 'b', 'a'))  ## 4 2 1
  ## .names.to.indexes(names, c( '-', 'c', 'a')) ## 2 4 5
  ## .names.to.indexes(names, NULL)

  ## to check if first element is "-", we have to use this more
  ## complex expression, as elems[1] == "-" is an error if the first element
  ## by chance is NA.
  if (is.null(elems)) {
    return(1:length(names))
  }
  if (isTRUE(all.equal("-", elems[1]))) {
    invert = TRUE
    elems = elems[- 1]
  } else {
    invert = FALSE

  }

  indexes = match(elems, names)


  if (allow.regex) {
    ## see if any elements returned NA, in which case try them individually
    ## as regular expressions.
    which.na <- which(is.na(indexes))
    if (any(which.na)) {
      regex.elems <- elems[which.na]
      new.indexes <- lapply(regex.elems, function(r) {grep(r, names)})
      new.indexes <- unique(unlist(new.indexes))
      indexes <- indexes[- which.na]
      indexes <- c(indexes, new.indexes) # TODO, preserve order?
    }
    allow.na <- TRUE # allow NAs through now.
  }

  if (!allow.na) {
    if (any(is.na(indexes))) 
    stop("some indexes not found.")
  }

  if (invert)
    indexes = setdiff(1:(length(names)), indexes)

  indexes

}
