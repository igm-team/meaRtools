# Diana Hall
# purpose: generate raster plot with Robject complete with burst + ns data made from IGM.main
#  prompts user for input or the file path may be passed
generate.raster.plot <- function(RobjectFile=NULL,
  outputdir=NULL ,
  well.for.raster=NULL,
  interval.for.raster=NULL,
  show.bursts=F,
  show.burst.number=F,
  show.networkspikes=F,
  show.ns.number=F,
  show.nb=F,
  window.size=NULL) {

  # show.nb=T;show.ns.number=T;show.networkspikes=T;show.burst.number=T; .plot.mm.s=meaRtools:::.plot.mm.s
  # show.bursts=T;interval.for.raster=c(1,15);well.for.raster="B2"; window.size=10;
  # RobjectFile="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/exampleRecording/Analysis/R_Objects/exampleRecording_1012016_plate1_DIV4.RData"
  # outputdir="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/Many_treatments"
  # 
  if (is.null(RobjectFile)){
    have.data = F; counter = 1
    while (have.data == F) {
      RobjectFile <- tk_choose.files(caption = "Select R-object (.RData) for raster plot")
      data.type <- strsplit(basename(RobjectFile), split = "[.]")[[1]][2]

      # laod and check
      if (!is.element(data.type, c("rdata", "RData", "rData", "Rdata"))){
        counter = counter + 1
        tkmessageBox(message = ".RData file needed!", icon = "error", type = "ok")
      } else {
        t = load(RobjectFile, verbose = T)
        s = list(); s[[1]] <- get(t)
        have.data = T
        analysis <- list()
        analysis$output.dir <- dirname(RobjectFile)
        analysis$Routput.dir <- dirname(RobjectFile)
      }
      if (counter == 4) {
        stop()
      }
    }
  } else {
    analysis <- list()
    analysis$output.dir <- dirname(RobjectFile)
    analysis$Routput.dir <- dirname(RobjectFile)
    t = load(RobjectFile, verbose = T)
    s = list(); s[[1]] <- get(t)
    have.data = T
  }

  if (is.null(outputdir)){
    outputdir = analysis$output.dir
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
  if (!is.element("ns.all", names(s[[1]]))) {

    tkmessageBox(message = "No network spike data in Robject!\n")
    stop("No network spike data in Robject!\n")
  }

  ## ready data for plot
  # correct spike times for recording start offset


  # initialize loop and keep raster up
  # want.new.raster<-T;show.bursts<-T;show.burst.number<-T;show.networkspikes<-T; show.ns.number<-T;

  # +++++++++++++++++++++well.for.raster
  if (!exists("well.for.raster") || is.null(well.for.raster)) {
    return("Well has no data, please select another well")
  }
  well.for.raster <- toupper(well.for.raster) # ensure user entered upper case well names
  # check if its among available channels
  if (!is.element(well.for.raster, unique(s[[1]]$cw))) {

    return("Well has no data, please select another well")

  }



  # +++++++++++++++++++++ show bursts
  if (!exists("show.bursts") || is.na(show.bursts)) { show.bursts = F }
  if (!is.element(show.bursts, c(T, F))) {
    show.bursts = T
  }

  # +++++++++++++++++++++ show network spikes
  if (!exists("show.networkspikes") || is.na(show.networkspikes)) {
    show.networkspikes = F }
  if (!is.element(show.networkspikes, c(T, F))) {
    show.networkspikes = F
  }

  # +++++++++++++++++++++ show bursts number
  if (!exists("show.burst.number") || is.na(show.burst.number)) {
    show.burst.number = F }
  if (!is.element(show.burst.number, c(T, F))) {
    show.burst.number = F
  }

  # +++++++++++++++++++++ show network burst
  if (!exists("show.nb") || is.na(show.nb)) {
    show.nb = F }
  if (!is.element(show.nb, c(T, F))) {
    show.nb = F
  }

  # +++++++++++++++++++++ window.size
  window.size.i = 1
  if (is.null(window.size)) {
    window.size.i = 1 }
  if (!is.element(window.size, as.numeric(names(s[[1]]$nb.all[[1]])))) {
    window.size.i = 1
  }
  if (is.element(window.size, as.numeric(names(s[[1]]$nb.all[[1]])))) {
    window.size.i = which(window.size == as.numeric(names(s[[1]]$nb.all[[1]])))
  }


  # ++++++++++++++++++++++++++interval check
  if (is.null(interval.for.raster) || length(interval.for.raster) < 2) {
    if (is.element("rec_time", names(s[[1]]))){
      interval.for.raster <- c(s[[1]]$rec_time[1], s[[1]]$rec_time[2])
    } else {
      interval.for.raster <- c(min(unlist(lapply(s[[1]]$spikes, min))),
        max(unlist(lapply(s[[1]]$spikes, max))))
    }

  }

  if (any(is.na(interval.for.raster))) {
    if (is.na(interval.for.raster[1])) {
      interval.for.raster[1] <- s[[1]]$rec_time[1]
    }
    if (is.na(interval.for.raster[2])) {
      interval.for.raster[2] <- s[[1]]$rec_time[2]
    }
  }
  # error check on times chosen
  if (0 > interval.for.raster[1]) {
    interval.for.raster[1] <- s[[1]]$rec_time[1]
    print(paste("Beginning of raster interval preceeds recording start",
      "resetting start of raster interval to start of recording",
      sep = "\n"))

  }

  if (ceiling(s[[1]]$rec_time[2]) < interval.for.raster[2]) {
    interval.for.raster[2] <- s[[1]]$rec_time[2]

    print(paste("End of raster interval exceeds recording end",
      "resetting end of raster interval to end of recoding",
      sep = "\n"))

  }
  if (ceiling(s[[1]]$rec_time[2]) < interval.for.raster[1]) {
    interval.for.raster[1] <- s[[1]]$rec_time[1]

    print(paste("Start of raster interval exceeds recording end",
      "resetting end of raster interval to end of recoding",
      sep = "\n"))

  }

  if (interval.for.raster[2] < interval.for.raster[1]) {
    interval.for.raster <- c(ceiling(s[[1]]$rec_time[1]), floor(s[[1]]$rec_time[2]))

  }

  # +++++++++++++++++++++ show.ns.number
  if (!exists("show.ns.number") || is.null(show.ns.number)) {
    show.ns.number = F }
  if (!is.element(show.ns.number, c(T, F))) {
    show.ns.number = F
  }


  # each spike has a name, e.g. "A2_321", "A2_322" that's 1st and 2nd spike of channel A2_32
  if (show.networkspikes){
    if (!(s[[1]]$ns.all[[well.for.raster]]$brief[1] == 0 || is.na(s[[1]]$ns.all[[well.for.raster]]$brief[1]))){
      raster.ns.t <- s[[1]]$ns.all[[well.for.raster]]$measures[ , c("time", "durn", "peak.val") ]
      if (is.matrix(raster.ns.t)){
        raster.ns.t[, "time"] = raster.ns.t[, "time"]
        index.want <- which(raster.ns.t[, "time"] < interval.for.raster[2] & raster.ns.t[, "time"] > interval.for.raster[1])
        if (length(index.want) > 0){
          raster.ns <- raster.ns.t[index.want, ]
        } else {
          raster.ns = NULL
        }

      } else {
        raster.ns.t["time"] = raster.ns.t["time"]
        index.want <- which(raster.ns.t["time"] < interval.for.raster[2] & raster.ns.t["time"] > interval.for.raster[1])
        if (length(index.want) > 0){
          raster.ns <- raster.ns.t
        } else {
          raster.ns = NULL
        }

      }

    } else {
      raster.ns = NULL
    }
  } else {
    raster.ns = NULL
  }


  # +++++++++++++++++++++++++++++++++++++++show.nb
  raster.nb = NULL
  if (show.nb) {
    if (is.element("nb.all", names(s[[1]]))) {
      if (is.element(well.for.raster, names(s[[1]]$nb.all))) {
        if (!(nrow(s[[1]]$nb.all[[well.for.raster]][[window.size.i]]) == 0 ||
            is.na(nrow(s[[1]]$nb.all[[well.for.raster]][[window.size.i]])))) {
          raster.nb.t <- s[[1]]$nb.all[[well.for.raster]][[window.size.i]][ , c("startT", "endT") ]
          if (is.data.frame(raster.nb.t)) {

            index.want <- which(raster.nb.t$startT < interval.for.raster[2] &
              raster.nb.t$endT > interval.for.raster[1])
            if (length(index.want) > 0) {
              raster.nb <- raster.nb.t[index.want, ]}
          }
        }
      }
    }
  }

  ####################################################### 



  ss <- summary(s[[1]])

  # make plot title
  plot.title.first.line <- paste(unlist(strsplit(basename(s[[1]]$file), split = "_"))[1:4],
    collapse = "_")
  # Diana change by user request: include treatment of chosen well
  plot.title.first.line <- paste(paste(unlist(strsplit(plot.title.first.line, split = ".RData"))[1],
    collapse = "_"),
  paste(well.for.raster,
    s[[1]]$treatment[well.for.raster], sep = "=")
  , sep = ", ")
  # ns plot title line
  if (!is.null(raster.ns) && show.networkspikes){
    plot.title.ns.line <- paste("green = ns, # ns.peak " , sep = "")
  } else if (is.null(raster.ns) && show.networkspikes){
    plot.title.ns.line <- paste("no network spikes")
  } else {
    plot.title.ns.line <- paste(" ")
  }
  # nb plot title line
  if (!is.null(raster.nb) && show.nb){
    plot.title.nb.line <- paste("orange = nb, window size ", names(s[[1]]$nb.all[[well.for.raster]])[window.size.i], "ms", sep = "")
  } else if (is.null(raster.nb) && show.nb){
    plot.title.nb.line <- paste("no network bursts")
  } else {
    plot.title.nb.line <- paste(" ")
  }

  # burst title line
  if (show.bursts && show.burst.number){
    plot.title.b.line <- paste("red (horz)= burst,  ",
      "blue=# spikes/burst", sep = "")
  } else if (show.bursts && !show.burst.number){
    plot.title.b.line <- paste("red= bursts ", sep = "")
  } else {
    plot.title.b.line <- paste(" ")
  }
  plot.title <- paste(plot.title.first.line,
    plot.title.b.line,
    plot.title.ns.line,
    plot.title.nb.line,
    sep = "\n")


  #### plot interesting responses for all files
  RasterPlotPathTemp1 = paste0(analysis$output.dir, "/rasterPlot_",
    interval.for.raster[1], "_", interval.for.raster[2], "_",
    well.for.raster)
  if (show.bursts) {
    RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_b")
  }
  if (show.burst.number) {
    RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_bn")
  }
  if (show.networkspikes) {
    RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_ns")
  }
  if (show.ns.number) {
    RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_nsN")
  }
  if (show.nb) {
    RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_nb")
    if (!is.null(names(s[[1]]$nb.all[[well.for.raster]]))) {
      RasterPlotPathTemp1 <- paste0(RasterPlotPathTemp1, "_", names(s[[1]]$nb.all[[well.for.raster]])[window.size.i])
    }
  }

  RasterPlotPath <- paste0(RasterPlotPathTemp1, ".pdf")
  pdf(file = RasterPlotPath)


  # +++++++++++++
  # Diana 11-29-2016: make 1 common panel function
  panel.function <- function(raster.ns, raster.nb, show.ns.number=T) {
    if (!is.null(raster.nb)) {
      for (cur.lnb in 1:length(raster.nb[, 1 ])) {
        lines(xy.coords(c(raster.nb[cur.lnb, "startT"],
          raster.nb[cur.lnb, "endT"]),
        c(- .02, - .02)) ,
        col = "orange", lwd = 4, lend = "square") }
    } # end is.null raster.nb

    if (!is.null(raster.ns)) {
      if (is.matrix(raster.ns)){
        for (cur.l in 1:length(raster.ns[, "time"])) {
          lines(xy.coords(c(raster.ns[cur.l, "time"], raster.ns[cur.l, "time"]),
            c(0, .98)) ,
          col = "green") }
        if (show.ns.number) {
          n.digits = sum(floor(raster.ns[, 3] / 10)) + length(raster.ns[, 3])
          close.neighbors = which(c(raster.ns[- 1, 1], tail(raster.ns[- 1, 1] + 1.1, n = 1)) - raster.ns[, 1] < 1) + 1
          for (i in 1:length(raster.ns[, 1])) {
            x.coord = raster.ns[i, "time"]
            if (is.element(i, close.neighbors)) {
              deltay_t = (interval.for.raster[2] - interval.for.raster[1]) / 75
              x.coord = x.coord + deltay_t
            }

            print(x.coord)
            cur.peak.val = raster.ns[i, "peak.val"]
            print(cur.peak.val)
            text(x = x.coord, y = 1.02  ,
              labels = cur.peak.val , pos = NULL, col = "green")
          } # end of label for loop
        } # end of if(show.ns.number)
        mtext(text = summary(s[[1]]), outer = T, side = 3)
      } else if (is.vector(raster.ns)) {
        lines(xy.coords(c(raster.ns["time"], raster.ns["time"]),
          c(0, .98)) ,
        col = "green")
        if (show.ns.number) {
          # use vector indexing
          x.coord = raster.ns["time"]
          cur.peak.val = raster.ns["peak.val"]
          text(x = x.coord, y = 1.02 ,
            labels = cur.peak.val , pos = NULL, col = "green")
        }

      }
    } # end of if!is.null raster.nb




  } # end of panel function



  .plot.spike.list(s[[1]], beg = interval.for.raster[1],
    label.cells = T,
    end = interval.for.raster[2] ,
    show.bursts = show.bursts,
    whichcells = well.for.raster ,
    show.burst.number = show.burst.number,
    main = plot.title,
    panel.first = panel.function(raster.ns = raster.ns, raster.nb = raster.nb,
      show.ns.number = T))

  dev.off()
  # pop open file
  system(paste("open ", RasterPlotPath, sep = ""))


} # end of function



.plot.spike.list <- function(s, whichcells = NULL, beg = min(unlist(s$spikes), na.rm = TRUE),
  end = max(unlist(s$spikes), na.rm = TRUE), label.cells = FALSE,
  show.burst.number=F ,
  use.names = TRUE, show.bursts = FALSE, main = NULL, ylab = "Unit",
  xlab = "Time (s)", for.figure = FALSE, show.episodes, episode.y = - 0.01,
  ...) {
  if (length(whichcells) > 0 && is.numeric(whichcells[1])) {
    }
  else {
    whichcells = .names.to.indexes(names(s$spikes), whichcells, allow.na = TRUE)
  }
  if (is.null(main)) {
    main <- basename(s$file)
  }
  N <- length(whichcells)
  ticpercell <- 1 / N
  deltay <- ticpercell * 0.8
  yminadd <- ticpercell
  if (show.bursts)
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
  have.bursts <- ((length(s$allb) > 0) && show.bursts)
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
          if (show.burst.number) {
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
    if (use.names) {
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
