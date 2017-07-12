# Diana Hall
# purpose: generate raster plot with Robject complete with burst + ns data made from IGM.main
#  prompts user for input or the file path may be passed
generate_raster_plot <- function(Robject_file=NULL,
  outputdir=NULL,
  well_for_raster=NULL,
  interval_for_raster=NULL,
  show_bursts=F,
  show_burst_number=F,
  show_networkspikes=F,
  show_ns_number=F,
  show.nb=F,
  window.size=NULL) {

  #
  if (is.null(Robject_file)){
    have_data <- F; counter <- 1
    while (have_data == F) {
      Robject_file <- tk_choose.files(caption =
                          "Select R-object (.RData) for raster plot")
      data.type <- strsplit(basename(Robject_file), split = "[.]")[[1]][2]

      # laod and check
      if (!is.element(data.type, c("rdata", "RData", "rData", "Rdata"))){
        counter <- counter + 1
        tkmessageBox(message = ".RData file needed!",
                     icon = "error", type = "ok")
      } else {
        t <- load(Robject_file, verbose = T)
        s <- list(); s[[1]] <- get(t)
        have_data <- T
        analysis <- list()
        analysis$output_dir <- dirname(Robject_file)
        analysis$Routput_dir <- dirname(Robject_file)
      }
      if (counter == 4) {
        stop()
      }
    }
  } else {
    analysis <- list()
    analysis$output_dir <- dirname(Robject_file)
    analysis$Routput_dir <- dirname(Robject_file)
    t <- load(Robject_file, verbose = T)
    s <- list(); s[[1]] <- get(t)
    have_data <- T
  }

  if (is.null(outputdir)){
    outputdir <- analysis$output_dir
  } else {
    dir.create(outputdir, showWarnings = FALSE)
  }

  setwd(outputdir)

  # error checks
  # check for correct data
  if (!is.element("allb", names(s[[1]]))) {
    tkmessageBox(message = "No burst data in Robject!\n")
    stop("No burst data in Robject!\n")
  }
  if (!is.element("ns_all", names(s[[1]]))) {

    tkmessageBox(message = "No network spike data in Robject!\n")
    stop("No network spike data in Robject!\n")
  }

  ## ready data for plot
  # correct spike times for recording start offset

  #  well for raster
  if (!exists("well_for_raster") || is.null(well_for_raster)) {
    return("Well has no data, please select another well")
  }
  well_for_raster <- toupper(well_for_raster)
  # ensure user entered upper case well names
  # check if its among available channels
  if (!is.element(well_for_raster, unique(s[[1]]$cw))) {

    return("Well has no data, please select another well")
  }


  # +++++++++++++++++++++ show bursts
  if (!exists("show_bursts") || is.na(show_bursts)) {
    show_bursts <- F
  }
  if (!is.element(show_bursts, c(T, F))) {
    show_bursts <- T
  }

  # +++++++++++++++++++++ show network spikes
  if (!exists("show_networkspikes") || is.na(show_networkspikes)) {
    show_networkspikes <- F
  }
  if (!is.element(show_networkspikes, c(T, F))) {
    show_networkspikes <- F
  }

  # +++++++++++++++++++++ show bursts number
  if (!exists("show_burst_number") || is.na(show_burst_number)) {
    show_burst_number <- F
  }
  if (!is.element(show_burst_number, c(T, F))) {
    show_burst_number <- F
  }

  # +++++++++++++++++++++ show network burst
  if (!exists("show.nb") || is.na(show.nb)) {
    show.nb <- F
  }
  if (!is.element(show.nb, c(T, F))) {
    show.nb <- F
  }

  #  window size
  window_size_i <- 1
  if (is.null(window.size)) {
    window_size_i <- 1
  }
  if (!is.element(window.size, as.numeric(names(s[[1]]$nb_all[[1]])))) {
    window_size_i <- 1
  }
  if (is.element(window.size, as.numeric(names(s[[1]]$nb_all[[1]])))) {
    window_size_i <- which(window.size == as.numeric(names(s[[1]]$nb_all[[1]])))
  }

  # ++++++++++++++++++++++++++interval check
  if (is.null(interval_for_raster) || length(interval_for_raster) < 2) {
    if (is.element("rec_time", names(s[[1]]))){
      interval_for_raster <- c(s[[1]]$rec_time[1], s[[1]]$rec_time[2])
    } else {
      interval_for_raster <- c(min(unlist(lapply(s[[1]]$spikes, min))),
        max(unlist(lapply(s[[1]]$spikes, max))))
    }
  }

  if (any(is.na(interval_for_raster))) {
    if (is.na(interval_for_raster[1])) {
      interval_for_raster[1] <- s[[1]]$rec_time[1]
    }
    if (is.na(interval_for_raster[2])) {
      interval_for_raster[2] <- s[[1]]$rec_time[2]
    }
  }
  # error check on times chosen
  if (0 > interval_for_raster[1]) {
    interval_for_raster[1] <- s[[1]]$rec_time[1]
    print(paste("Beginning of raster interval preceeds recording start",
      "resetting start of raster interval to start of recording",
      sep = "\n"))

  }

  if (ceiling(s[[1]]$rec_time[2]) < interval_for_raster[2]) {
    interval_for_raster[2] <- s[[1]]$rec_time[2]

    print(paste("End of raster interval exceeds recording end",
      "resetting end of raster interval to end of recoding",
      sep = "\n"))

  }
  if (ceiling(s[[1]]$rec_time[2]) < interval_for_raster[1]) {
    interval_for_raster[1] <- s[[1]]$rec_time[1]

    print(paste("Start of raster interval exceeds recording end",
      "resetting end of raster interval to end of recoding",
      sep = "\n"))

  }

  if (interval_for_raster[2] < interval_for_raster[1]) {
    interval_for_raster <- c(ceiling(s[[1]]$rec_time[1]),
                             floor(s[[1]]$rec_time[2]))

  }

  # show ns number
  if (!exists("show_ns_number") || is.null(show_ns_number)) {
    show_ns_number <- F
  }
  if (!is.element(show_ns_number, c(T, F))) {
    show_ns_number <- F
  }

  # each spike has a name, e.g. "A2_321", "A2_322" that's 1st and 2nd spike of
  # channel A2_32
  if (show_networkspikes){
    if (!(s[[1]]$ns_all[[well_for_raster]]$brief[1] == 0 ||
          is.na(s[[1]]$ns_all[[well_for_raster]]$brief[1]))){
      raster_ns_t <- s[[1]]$ns_all[[well_for_raster]]$measures[,
                                        c("time", "durn", "peak_val") ]
      if (is.matrix(raster_ns_t)){
        raster_ns_t[, "time"] <- raster_ns_t[, "time"]
        index.want <- which(raster_ns_t[, "time"] < interval_for_raster[2] &
                              raster_ns_t[, "time"] > interval_for_raster[1])
        if (length(index.want) > 0){
          raster_ns <- raster_ns_t[index.want, ]
        } else {
          raster_ns <- NULL
        }

      } else {
        raster_ns_t["time"] <- raster_ns_t["time"]
        index.want <- which(raster_ns_t["time"] < interval_for_raster[2] &
                              raster_ns_t["time"] > interval_for_raster[1])
        if (length(index.want) > 0){
          raster_ns <- raster_ns_t
        } else {
          raster_ns <- NULL
        }
      }
    } else {
      raster_ns <- NULL
    }
  } else {
    raster_ns <- NULL
  }


  # show nb
  raster_nb <- NULL
  if (show.nb) {
    if (is.element("nb_all", names(s[[1]]))) {

      if (is.element(well_for_raster, names(s[[1]]$nb_all))) {
        if (!(nrow(s[[1]]$nb_all[[well_for_raster]][[window_size_i]]) == 0 ||
            is.na(nrow(s[[1]]$nb_all[[well_for_raster]][[window_size_i]])))) {
          raster_nb_t <- s[[1]]$nb_all[[well_for_raster]][[window_size_i]][,
                                                        c("start_t", "end_t") ]
          if (is.data.frame(raster_nb_t)) {
            index.want <- which(raster_nb_t$start_t < interval_for_raster[2] &
              raster_nb_t$end_t > interval_for_raster[1])
            if (length(index.want) > 0) {
              raster_nb <- raster_nb_t[index.want, ]
            }
          }
        }
      }
    }
  }

  # make plot title
  plot_title_first_line <- paste(unlist(strsplit(basename(s[[1]]$file),
                                       split = "_"))[1:4], collapse = "_")
  # Diana change by user request: include treatment of chosen well
  plot_title_first_line <- paste(paste(unlist(strsplit(plot_title_first_line,
                                       split = ".RData"))[1], collapse = "_"),
  paste(well_for_raster, s[[1]]$treatment[well_for_raster], sep = "="),
                                       sep = ", ")
  # ns plot title line
  if (!is.null(raster_ns) && show_networkspikes){
    plot_title_ns_line <- paste("green = ns, # ns.peak ", sep = "")
  } else if (is.null(raster_ns) && show_networkspikes){
    plot_title_ns_line <- paste("no network spikes")
  } else {
    plot_title_ns_line <- paste(" ")
  }
  # nb plot title line
  if (!is.null(raster_nb) && show.nb){
    plot_title_nb_line <- paste("orange = nb, window size ",
            names(s[[1]]$nb_all[[well_for_raster]])[window_size_i], "ms",
            sep = "")
  } else if (is.null(raster_nb) && show.nb){
    plot_title_nb_line <- paste("no network bursts")
  } else {
    plot_title_nb_line <- paste(" ")
  }

  # burst title line
  if (show_bursts && show_burst_number){
    plot_title_b_line <- paste("red (horz)= burst,  ",
      "blue=# spikes/burst", sep = "")
  } else if (show_bursts && !show_burst_number){
    plot_title_b_line <- paste("red= bursts ", sep = "")
  } else {
    plot_title_b_line <- paste(" ")
  }
  plot.title <- paste(plot_title_first_line,
    plot_title_b_line,
    plot_title_ns_line,
    plot_title_nb_line,
    sep = "\n")

  #### plot interesting responses for all files
  raster_plot_path_temp1 <- paste0(analysis$output_dir, "/", "rasterPlot_",
    interval_for_raster[1], "_", interval_for_raster[2], "_",
    well_for_raster)
  if (show_bursts) {
    raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_b")
  }
  if (show_burst_number) {
    raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_bn")
  }
  if (show_networkspikes) {
    raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_ns")
  }
  if (show_ns_number) {
    raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_nsN")
  }
  if (show.nb) {
    raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_nb")
    if (!is.null(names(s[[1]]$nb_all[[well_for_raster]]))) {
      raster_plot_path_temp1 <- paste0(raster_plot_path_temp1, "_",
                     names(s[[1]]$nb_all[[well_for_raster]])[window_size_i])
    }
  }

  raster_plot_path <- paste0(raster_plot_path_temp1, ".pdf")
  pdf(file = raster_plot_path)


  # +++++++++++++
  # Diana 11-29-2016: make 1 common panel function
  panel.function <- function(raster_ns, raster_nb, show_ns_number=T) {
    if (!is.null(raster_nb)) {
      for (cur.lnb in 1:length(raster_nb[, 1 ])) {
        lines(xy.coords(c(raster_nb[cur.lnb, "start_t"],
          raster_nb[cur.lnb, "end_t"]),
        c(- .02, - .02)),
        col = "orange", lwd = 4, lend = "square")
      }
    } # end is.null raster_nb

    if (!is.null(raster_ns)) {
      if (is.matrix(raster_ns)){
        for (cur.l in 1:length(raster_ns[, "time"])) {
          lines(xy.coords(c(raster_ns[cur.l, "time"], raster_ns[cur.l, "time"]),
            c(0, .98)),
          col = "green")
        }
        if (show_ns_number) {
          close.neighbors <- which(c(raster_ns[- 1, 1], tail(raster_ns[- 1, 1] +
                                        1.1, n = 1)) - raster_ns[, 1] < 1) + 1
          for (i in 1:length(raster_ns[, 1])) {
            x.coord <- raster_ns[i, "time"]
            if (is.element(i, close.neighbors)) {
              deltay_t <- (interval_for_raster[2] - interval_for_raster[1]) / 75
              x.coord <- x.coord + deltay_t
            }

            print(x.coord)
            cur_peak_val <- raster_ns[i, "peak_val"]
            print(cur_peak_val)
            text(x <- x.coord, y <- 1.02,
              labels = cur_peak_val, pos = NULL, col = "green")
          } # end of label for loop
        } # end of if(show_ns_number)
        mtext(text = summary(s[[1]]), outer = T, side = 3)
      } else if (is.vector(raster_ns)) {
        lines(xy.coords(c(raster_ns["time"], raster_ns["time"]),
          c(0, .98)),
        col = "green")
        if (show_ns_number) {
          # use vector indexing
          x.coord -> raster_ns["time"]
          cur_peak_val -> raster_ns["peak_val"]
          text(x = x.coord, y = 1.02,
            labels = cur_peak_val, pos = NULL, col = "green")
        }
      }
    } # end of if!is.null raster_nb




  } # end of panel function



  .plot_spike_list(s[[1]], beg = interval_for_raster[1],
    label.cells = T,
    end = interval_for_raster[2],
    show_bursts = show_bursts,
    whichcells = well_for_raster,
    show_burst_number = show_burst_number,
    main = plot.title,
    panel.first = panel.function(raster_ns = raster_ns, raster_nb = raster_nb,
      show_ns_number = T))

  dev.off()
  # pop open file
  system(paste("open ", raster_plot_path, sep = ""))


} # end of function



.plot_spike_list <- function(s, whichcells = NULL, beg = min(unlist(s$spikes),
                                                             na.rm = TRUE),
  end = max(unlist(s$spikes), na.rm = TRUE), label.cells = FALSE,
  show_burst_number=F,
  use_names = TRUE, show_bursts = FALSE, main = NULL, ylab = "Unit",
  xlab = "Time (s)", for.figure = FALSE, show.episodes, episode.y = - 0.01,
  ...) {
  if (length(whichcells) > 0 && is.numeric(whichcells[1])) {
    }
  else {
    whichcells <- .names_to_indexes(names(s$spikes), whichcells,
                                    allow_na = TRUE)
  }
  if (is.null(main)) {
    main <- basename(s$file)
  }
  N <- length(whichcells)
  ticpercell <- 1 / N
  deltay <- ticpercell * 0.8
  yminadd <- ticpercell
  if (show_bursts)
    spikes <- s$spikes
    else spikes <- s$spikes
  if (for.figure) {
    plot(c(beg, end), c(0, 1), type = "n", yaxt = "n", bty = "n",
      main = "", xaxt = "n", xaxs = "i", yaxs = "i", xlab = "",
      ylab = "", ...)
    mtext(main, side = 3, adj = 0, line = 0.5)
  }
  else {
    plot(c(beg, end), c(0, 1), type = "n", bty = "n", yaxt = "n",
      main = main, xlab = xlab, ylab = ylab, ...)
  }
  ymin <- 0
  have.bursts <- ( (length(s$allb) > 0) && show_bursts)
  for (cell in whichcells) {
    ts <- spikes[[cell]]
    n <- length(ts)
    if (n > 0) {
      ys <- numeric(n) + ymin
      segments(ts, ys, ts, ys + deltay, lwd = 0.2)
      if (have.bursts) {
        burst.times <- s$allb[[cell]]
        if (!is.na(burst.times[1])) {
          nbursts <- nrow(burst.times)
          ys <- rep(ymin + deltay / 2, nbursts)
          shimmy <- deltay * 0.25
          odd <- (1:nbursts) %% 2 == 1
          ys[odd] <- ys[odd] + shimmy
          start.burst <- ts[burst.times[, "beg"]]
          end.burst <- ts[burst.times[, "beg"] + burst.times[,
          "len"] - 1]
          segments(start.burst, ys, end.burst, ys, col = "red",
            lwd = 2)
          if (show_burst_number) {
            text(start.burst, rep(ymin + deltay * 1.1,
              nbursts), labels = burst.times[, "len"],
            col = "blue")
          }
        }
      }
    }
    ymin <- ymin + yminadd
  }
  if (label.cells) {
    allys <- seq(from = yminadd / 2, by = yminadd, length = N)
    if (use_names) {
      labels <- names(spikes)[whichcells]
    }
    else {
      labels <- whichcells
    }
    axis(2, at = allys, labels = labels, las = 1, tick = F)
  }
  if (missing(show.episodes)) {
    show.episodes <- ("episodes" %in% names(s))
  }
  if (show.episodes) {
    segments(s$episodes[, "beg"], episode.y, s$episodes[,
      "end"], episode.y, col = "purple", xpd = NA)
  }
}
