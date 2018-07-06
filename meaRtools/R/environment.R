## modelled on https://github.com/lgatto/pRoloc/blob/master/R/environment.R
## 2018-06-21

.meaRtoolsEnv <- new.env(parent=emptyenv(), hash=TRUE)

.axion_plateinfo <- list("Axion 48 well" = list(
  n_well = 48,
  wells = paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
  n_well_r = 6,
  n_well_c = 8,
  layout = c(8, 6),
  n_elec_r = 4,
  n_elec_c = 4,
  xlim = c(-100, 7900),
  ylim = c(0, 6000),
  spacing = 200,
  corr_breaks = 0),
"Axion 12 well" = list(
  n_well = 12,
  wells = paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
  n_well_r = 3,
  n_well_c = 4,
  layout = c(4, 3),
  n_elec_r = 8,
  n_elec_c = 8,
  xlim = c(-100, 7200),
  ylim = c(0, 6000),
  spacing = 200,
  corr_breaks = 0))

## Set the plateinformation that we have so far.
#' MEA plate information
#'
#' @name plateinfo
plateinfo = .axion_plateinfo


##' Return information about an MEA plate format
##'
##' Given a plate name, return a list of information.
##' If the plate name is not recognised, an error is generated.
##' 
##' @title Return information about an MEA plate format
##' @param arrayname Plate name 
##' @return A list storing information about that plate.
##' @author Stephen Eglen
get_plateinfo <- function(arrayname) {
  ## Return useful information related to arrayname
  ##
  ## plateinfo "Axion 12 well"
  info = get("plateinfo", envir = .meaRtoolsEnv)
  res = info[[arrayname]]
  if (is.null(res)) {
    stop(sprintf("arrayname <%s> not found", arrayname))
  } else {
    res
  }
}


##' Provide information about a new MEA plate format
##'
##' Store information about a new platename called arrayname.
##' If arrayname already has been used, overwrite it with this
##' new information.
##' 
##' @title Provide information about a new MEA plate format.
##' @param arrayname Name of the plate
##' @param info List of information to store about this plate
##' @return Nothing.
##' @author Stephen Eglen
add_plateinfo <- function(arrayname, info) {
  ## If arrayname data are alreeady given, this new information
  ## overwrites it.
  plate_info = get("plateinfo", envir = .meaRtoolsEnv)
  plate_info[[arrayname]] = info
  print(names(plate_info))
  assign("plateinfo", plate_info, envir = .meaRtoolsEnv)
}

assign("plateinfo", plateinfo, envir = .meaRtoolsEnv)

