load.spikelist<-function (spkDataFile) {
  temp<-load(spkDataFile ) # temp contains objects in loaded workspace
  data<-get(temp)
}

# remake this function so that it can read .RData objects
.Robject.read.spikes<-function (spkDataFile, 
                               ids = NULL, 
                               time.interval = 1, 
                               beg = NULL, 
                               end = NULL, corr.breaks) {
  
  temp<-load(spkDataFile ) # temp contains objects in loaded workspace
  data<-get(temp)
  spikes<-data$spikes
  
  arrayinfo <- .get.array.info(data)
  layout <- arrayinfo$layout
  if (missing(corr.breaks)) {
    corr.breaks <- arrayinfo$corr.breaks
  }
  s <- .construct.s(spikes, ids, time.interval, beg, end, corr.breaks, 
                   layout, filename = spkDataFile)
  s$dose <- data$dose
  s$treatment <- data$treatment
  s$size <- data$size
  s$units <- data$units
  s$well <- data$well
  s <- get.num.AE(s)
  s
}

calculate.spike.features <- function(RobjectFiles,parameters) {
  RobjectFiles <- sort(RobjectFiles)
  s = list()
  count <- 0
  for (i in 1:length(RobjectFiles)) {
    timepoint <- substring(basename(RobjectFiles[i]), nchar(basename(RobjectFiles[i])) - 
                             8, nchar(basename(RobjectFiles[i])) - 7)
    current <- .filter.spikes.Robject(RobjectFiles[i], 
                                      elec.min.rate = parameters$elec.min.rate, 
                                      elec.max.rate = parameters$elec.max.rate, 
                                      well.min.rate = parameters$well.min.rate)
    current$parameters <- parameters
    current$timepoint <- timepoint
    if (length(current$nspikes) > 0) {
      count <- count + 1
      s[[count]] <- current
    }
  }
  s
}

calculate.burst.features <- function(s) {
  for (i in 1:length(s)) {
    current <- s[[i]]
    if (length(current$nspikes) > 0) {
      if (current$parameters$burst.type=="ps"){
        current$allb <- lapply(current$spikes, si.find.bursts, s.min=current$parameters$s.min)
        current$bs <- calc.burst.summary(current)
        current$bs$burst.type="ps"
      } else {
        current$allb <- lapply(current$spikes, mi.find.bursts,current$parameters$mi.par)
        current$bs <- calc.burst.summary(current)
        current$bs$burst.type="mi"
      }
      s[[i]] <- current
    }
  }
  s
}

.filter.spikes.Robject<-function (RobjectFiles, elec.min.rate = (1/60), elec.max.rate = 25, 
                                 well.min.rate = 4) {
  for (i in 1:length(RobjectFiles)) {
    if (!(i == 1)) {
      rm(s1, s2)
    }
    s1 <- load.spikelist(RobjectFiles[i])
    if (class(s1) != "spike.list") {
      s1 <- .Robject.read.spikes(RobjectFiles[i])
    }
    low <- which(s1$meanfiringrate < elec.min.rate)
    high <- which(s1$meanfiringrate > elec.max.rate)
    extremes <- c(low, high)
    bad.ids <- names(extremes)
    bad.ids <- c("-", bad.ids)
    s2 <- remove.spikes(s1, bad.ids)
    s2$treatment <- s1$treatment
    s2$size <- s1$size
    s2$units <- s1$units
    s2$dose <- s1$dose
    s2$well <- s1$well
    s2 <- get.num.AE(s2)
    low <- which(s2$nAE < well.min.rate)
    bad.wells <- names(low)
    bad.wells <- c("-", bad.wells)
    s <- remove.spikes(s2, bad.wells)
    s$treatment <- s1$treatment
    names(s$treatment) <- s1$well
    # remove from good wells analysis any wells without treatment and below min required nAE
    s$goodwells <- names(which(s2$nAE >= well.min.rate))[names(which(s2$nAE >= well.min.rate))%in%names(s$treatment[!is.na(s$treatment) & (s$treatment != "")])]
    s$size <- s1$size
    names(s$size) <- s1$well
    s$units <- s1$units
    names(s$units) <- s1$well
    s$dose <- s1$dose
    names(s$dose) <- s1$well
    s$well <- s1$well
    s <- get.num.AE(s)
  }
  s
}

.spkList2list <-function (file) {
    data.raw<-read.csv(file,header=T,colClasses=c("NULL", "NULL", NA, NA, NA))
    
    # remove rows beyond end of spike data
    lastIndex=which(data.raw[,1]=="",arr.ind=TRUE)[1]
    if (!is.na(lastIndex))
    {
      data.raw=data.raw[1:lastIndex-1,]
    }

    data.raw$Electrode <- factor(data.raw$Electrode)
    data.raw$Time..s. <- as.numeric(as.character(data.raw$Time..s.))

    #remove NA
    ind.want<-which(!is.na(data.raw[,1]) )

    if (length( ind.want )>0){
      data.raw2<-data.frame(
        elect<-data.raw[ ind.want ,"Electrode"],
        timestamps<-data.raw[ ind.want ,"Time..s."]
      )

      data.raw2<-data.raw2[order(data.raw2$elect), ]
      spikes<-split(data.raw2$timestamps, data.raw2$elect,drop=T)
    } else {
      spikes<-NULL
    }
    
    spikes
  }

.spkList.to.Robject<-function (spikes, chem.info, RobjectFile ){
  RobjectFile <- path.expand(RobjectFile)
  if (file.exists(RobjectFile)) 
    unlink(RobjectFile)
  nspikes <- sapply(spikes, length)
  channels <- names(spikes)
  well <- chem.info$well
  treatment <- chem.info$treatment
  size <- chem.info$size
  dose <- chem.info$dose
  units <- chem.info$units
  wells <- .axion.guess.well.number(channels)
  array <- sprintf("Axion %d well", wells)
  plateinfo <- .plateinfo(array)
  epos <- .axion.elec.name.to.xy(channels, plateinfo)
 
  S<-list()
  
  sum.spikes <- sum(nspikes)
  
  S$spikes<-spikes
  
  S$sCount<-nspikes
  
  S$epos<-epos
  
  S$names<-channels
  
  S$array<-array
  S$treatment<-as.array( treatment )
  S$dose<-as.array( dose )
  S$size<-as.array( size )
  S$well<-as.array( well )
  S$units<-as.array( units )
  print(names(S))
  S
}

read.spikelist<-function(key, spkListFile, chem.info, Robject.dir){
  #function to convert spike list Robject
  #remove _spike_list
  key<-unlist( strsplit(key, split="_spike_list") )
  
  RobjectFile <- gsub("\\(|\\)", "_", sprintf("%s/%s", Robject.dir, key) )
  RobjectFile<-paste0(
    paste(strsplit(basename( RobjectFile ),split="_")[[1]][1:4], collapse="_"),".RData")
  
  #f is a list of all files
  f <- spkListFile
  
  #get spikes
  spikes.sep <- lapply(f, .spkList2list)
  short.filenames <- gsub("_spike_list.csv", "", basename(f))
  
  summary.table <- t(sapply(spikes.sep, .axion.spikesum2) )
  rownames(summary.table) <- short.filenames
  ma <- do.call("rbind", lapply(spikes.sep, .axion.spikestodf))
  #s2 is a list with all the channels and spikes under each channel
  s2 <- split(ma$time, ma$elec)
  numelec <- length(s2)
  total.spikes <- sum(sapply(s2, length))
  time.ranges <- sapply(s2, range)
  time.min <- min(time.ranges[1, ])
  time.max <- max(time.ranges[2, ])
  #printf formats the text and variables to output ready formats
  #cat contatenates files and then prints them
  cat(sprintf("Total number of spikes: %d\n", total.spikes))
  cat(sprintf("Unique number of electrodes: %d\n", numelec))
  cat(sprintf("Time range [%.3f %.3f] (seconds)\n", time.min, 
             time.max))
  print(summary.table)
  S<-.spkList.to.Robject(s2, chem.info, RobjectFile)
  save.file<-paste0(Robject.dir,"/", RobjectFile )
  save(S, file=save.file  )
  save.file
}

