filter.nonactive.spikes <- function(MEA,spikes.per.minute.min=1) {
  # remove electrode data where spike.per.minute rate does not
  # meet or exceed user-defined value 
  # (default = 1 spike/min = 1 Hz)
  timespan <- MEA$rec.time[2] - MEA$rec.time[1]
  MEA$spikes <- sapply(names(MEA$spikes),function(y) {
    if ((length(MEA$spikes[[y]]) / 60) >= spikes.per.minute.min) {
      return(MEA$spikes[[y]])
    }
  })
  MEA$spikes <- MEA$spikes[!sapply(MEA$spikes, is.null)]
  return(MEA)
}

calculate.entropy.and.MI <- function(MEA,treatments,mult.factor=1.5,bin.size=0.1) {
  data.dists <- list( "ENT"=list(), "MI"=list())
  norm.MIs.per.well = list();
  norm.ents.per.well <- .calculate.entropy.by.well(MEA,mult.factor)
  for (treatment in treatments) {
    ## get wells classified as treatment and  subset well data on well classification
    treatment.wells <- names(MEA$treatment[MEA$treatment==treatment])
    treatment.wells <- treatment.wells[treatment.wells %in% names(norm.ents.per.well)]
    norm.ents.per.well.treatment <- .wells.subset(norm.ents.per.well,treatment.wells)
    ## get mean entropy per well set for treatment
    norm.ent.means.per.well <- lapply(norm.ents.per.well.treatment,function(x) {mean(x)})
    ## calculate MI values
    norm.MIs.per.well[[treatment]] <- .pairwise.dists.per.well(MEA,wellnames=treatment.wells,
                                                              dist.metric="mutual.information",
                                                              bin.size)
    norm.MI.means.per.well <- lapply(norm.MIs.per.well[[treatment]],function(x) {mean(x)})
    # store summary stats to list
    data.dists[["ENT"]][[treatment]] <- .list.to.vals(norm.ent.means.per.well)
    data.dists[["MI"]][[treatment]] <- .list.to.vals(norm.MI.means.per.well)
  }
  return(list("data.dists"=data.dists, "norm.MIs.per.well"=norm.MIs.per.well ))
}

.calculate.entropy.by.well <- function(MEA, mult.factor=1.5) {
  norm.ents.per.well <- .entropies.per.well(MEA,diff=diff)
  norm.ents.per.well <- .filter.list(norm.ents.per.well,mult.factor=1.5)
  return(norm.ents.per.well)
}

.pairwise.dists.per.well<-function(MEA,wellnames=c(),bin.size=NA,dist.max=200,
                                  dist.metric="mutual.information",normalize=T) {
  # iterate through all wells, get all pairwise mutual 
  # information scores for each well
  dists.list <- list()
  t.0 <- MEA$rec.time[1]
  t.end <- MEA$rec.time[2]
  if (length(wellnames)==0) {
    wellnames <- MEA$well
  }
  for (wellname in wellnames) {
    dists.list[[wellname]] <- c()
    well.spikes <- .MEA.vals.subset(MEA,"spikes",wellname)
    well.elec.names <- names(well.spikes)
    elec.pairs <- .electrode.dist.pairs(MEA,elec.names=well.elec.names,
                                       dist.max=dist.max)
    if (length(elec.pairs) <= 1) {next}
    dists.list[[wellname]] <- sapply(elec.pairs,function(x){
      elec.pair <- strsplit(x,":")[[1]]
      spikes.i <- well.spikes[[elec.pair[1]]]
      spikes.j <- well.spikes[[elec.pair[2]]]
      spikes.i.len <- length(spikes.i)
      spikes.j.len <- length(spikes.j)
      if (min(spikes.i.len,spikes.j.len) <= 1) {return(NA)}
      dist <- .dist.electrode.pair(spikes.i, spikes.j, 
                                  t.0, t.end,bin.size=bin.size,
                                  dist.metric=dist.metric,normalize=normalize) 
      return(dist)
    })
  }
  return(dists.list)
}

.entropies.per.well <- function(MEA,diff=F,well.names=c()) {
  # iterate through all electrodes, store entropy vals
  # for each corresponding well
  ents.list <- list()
  for (elec in names(MEA$spikes)) {
    ent <- .entropy.electrode(MEA,elec) 
    if(is.na(ent)==T | is.nan(ent)==T) {
      next
    }
    elec.data <- strsplit(elec,"_")[[1]]
    well <- elec.data[1]
    if (length(well.names)>0) {
      if ((well %in% well.names) == F) {
        next
      }
    }
    if ((well %in% names(ents.list))==F) { 
      ents.list[[well]] = c()
    }
    ents.list[[well]] <- c(ents.list[[well]],ent)
  }
  return(ents.list)
}

.filter.list <- function(x.list,mult.factor=1.5) {
  for (item in names(x.list)) {
    item.vals <- x.list[[item]]
    item.vals.range <- .IQR.range(item.vals,mult.factor)
    item.vals <- item.vals[item.vals >= item.vals.range[1]
                           & item.vals <= item.vals.range[2]]
    x.list[[item]] <- item.vals
  }
  return(x.list)
}

.MEA.vals.subset <- function(MEA,attr,wellname) {
  # get set of attr vals for electrodes corresponding to specific inputted well
  new.obj = list()
  attr.node = MEA[[attr]]
  for (electrode in names(MEA[[attr]])) {
    if (grepl(wellname,electrode)==T) {
      new.obj[[electrode]] = attr.node[[electrode]]
    }
  }
  return(new.obj)
}

.dist.electrode.pair <- function(spikes.a, spikes.b, t.0, t.end,
                                 bin.size=NA,
                                 normalize=F,
                                 dist.metric="mutual.information",
                                 corr.method="pearson") {
  # use spike data and t.0+t.end to calculate mutual 
  # information between spikes from electrodes a and b
  if (is.na(bin.size)) {
    a.n <- length(spikes.a)
    b.n <- length(spikes.b)
    bin.count <- min(a.n,b.n)
  } else {
    bin.count <- 1
  }
  bins <- .uniform.bins(t.0,t.end,bin.count,bin.size=bin.size)
  spikes.a.bin <- .spikes.in.bins(spikes.a,bins) 
  spikes.b.bin <- .spikes.in.bins(spikes.b,bins)
  if (dist.metric=="mutual.information") {
    dist <- .mutual.information(spikes.a.bin, spikes.b.bin,
                               normalize=normalize)
  #} else if (dist.metric=="maximal.information.coefficient") {
  #  dist <- mine(spikes.a.bin, spikes.b.bin)$MIC
  } else {
    dist <- .correlation(spikes.a.bin, spikes.b.bin,
                        corr.method=corr.method)
  }
  return(dist)
}

.electrode.dist.pairs <- function(MEA,elec.names,dist.max=200,
                                  same.well.only=T) {
  # makes all possible comparisons of electrode coordinates
  # on plate, returns list of electrode:electrode name
  # combinations where dist. btwn electrodes is <= dist.max
  elec.combos <- c()
  if (length(elec.names)<=1) {return(c())}
  epos.subset <- MEA$layout$pos[elec.names,]
  epos.elecs <- rownames(epos.subset)
  for (i in 1:(length(epos.elecs)-1)) {
    elec.i.name <- epos.elecs[i]
    well.i <- strsplit(elec.i.name,"_")[[1]]
    elec.i.xy <- epos.subset[elec.i.name,]
    for (j in (i+1):length(epos.elecs)) {
      elec.j.name <- epos.elecs[j]
      well.j <- strsplit(elec.j.name,"_")[[1]]
      if (well.i != well.j && same.well.only==T) {
        next
      }
      elec.j.xy <- epos.subset[elec.j.name,]
      i.j.dist <- .euc.dist(elec.i.xy,elec.j.xy)
      if (i.j.dist <= dist.max) {
        elec.ij <- paste(elec.i.name,elec.j.name,sep=":")
        elec.combos <- c(elec.combos,elec.ij)
      }
    }
  }
  return(elec.combos)
}
.euc.dist <- function(coords.i, coords.j) {
  # calculate euclidian distance between coordinates i and j
  stopifnot(length(coords.i)==length(coords.j))
  dists.squared <- (coords.i-coords.j)^2
  dist <- sqrt(sum(dists.squared))
  return(dist)
}

.uniform.bins <- function(t.0, t.end,n,bin.size=NA) {
  # for a defined t.0, t.end and n.spikes, return n nonoverlapping
  # bins that are all of equal size and span [t.0, t.end]
  if (is.na(bin.size)) {
    bin.size <- (t.end-t.0)/n
  }
  bins <- seq(from=t.0,to=t.end,by=bin.size)
  if (bins[length(bins)]!=t.end) {
    bins <- c(bins,t.end)
  }
  return(bins)
}

.correlation <- function(a,b,corr.method="pearson",normalize=F) {
  ## calculate the correlation btwn probability distributions a and b
  
  # make sure a and b have same number of bins (equal bin sizes assumed)
  stopifnot(length(a) == length(b))
  # get sum of counts across bins for a, b, a+b
  a.total <- sum(a)
  b.total <- sum(b)
  ab.total <- a.total +  b.total
  # calc p(i) for a and b seperately
  p.a <- a / a.total
  p.b <- b / b.total
  
  if (normalize == T) {
    return(cor(p.a,p.b,method=corr.method))
  } else {
    return(cor(a,b,method=corr.method))
  }
}

.IQR.range <- function(x,mult.factor = 1) {
  x.median <- median(x)
  x.IQR <- IQR(x)
  x.IQR.min <- x.median - (mult.factor * x.IQR)
  x.IQR.max <- x.median + (mult.factor * x.IQR)
  x.IQR.range <- c(x.IQR.min,x.IQR.max)
  return(x.IQR.range)
}

.spikes.in.bins <- function(spikes,bins) {
  # count number of spikes in each bin
  hist.data <- hist(spikes,breaks=bins,plot=FALSE)
  spike.counts <- hist.data$counts
  return(spike.counts)
}

.p.bins <- function(bin.sizes) {
  # return prob of each bin assuming prob is linear
  # with bin size
  bins.totalsize <- sum(bin.sizes)
  p.bin <- bin.sizes / bins.totalsize
  return(p.bin)
}

.pdist.electrode.spikes <- function(spikes,t.0,t.end,
                                    bin.starts=c(),bin.ends=c(),
                                    bin.size=NA,
                                    probs=T) {
  # count number of spikes in each bin (uniform or user-defined), 
  # if user indicates,form prob.distribution based on the spike 
  # counts per bin, otherwise return spike counts per bin
  if (length(bin.starts) > 0 & length(bin.ends) > 0) {
    bin.edges <- unique(sort(c(t.0,bin.starts,bin.ends,t.end)))
  } else if (is.na(bin.size)==F) {
    bin.edges <- uniform.bins(t.0,t.end,1,bin.size=bin.size)
  } else {  
    bin.size <- (t.end-t.0) / length(spikes)
    bin.edges <- seq(t.0,t.end,by = bin.size)
    bin.starts.i <- seq(1,(length(bin.edges)-1),by=1)
    bin.ends.i <- seq(2,length(bin.edges),by=1)
    bin.starts <- bin.edges[bin.starts.i]
    bin.ends <- bin.edges[bin.ends.i]
  }
  spike.counts <- .spikes.in.bins(spikes, bin.edges)
  if (probs==T) {
    p.counts.bins <- .p.bins(spike.counts)
    return(p.counts.bins)
  } else {
    return(spike.counts)
  }
}

.entropy <- function(x,normalized.uniform=F) {
  # change counts to probs for x
  x.total = sum(x)
  p.x <- x / x.total
  entropy.x <- p.x * log2(p.x)
  # normalized entropy vals by theoretical max # bits stored in seq
  if (normalized.uniform == T) {
    entropy.x = entropy.x / log2(length(x))
  }
  # remove any NaN's produced in computation
  entropy.x <- entropy.x[is.nan(entropy.x)==F & is.na(entropy.x)==F]
  # calc total entropy
  entropy.x.total = -(sum(entropy.x))    
  return(entropy.x.total)
}

.KL.divergence <- function(x,y) {
  # if counts provided, ensure vals are changed to probs
  p.x <- x / sum(x)
  p.y <- y / sum(y)
  # get log2(p.x/p.y) likelihood ratio, ensure 0 division isn't encountered
  LR <- ifelse(p.x > 0, log2(p.x/p.y),0)
  # complete KL divergence calc by summing LR product with p.x
  KL.div <- sum(p.x * LR)
  return(KL.div)
}

.mutual.information <- function(a,b,normalize=F) {
  # make sure a and b have same number of bins (equal bin sizes assumed)
  stopifnot(length(a) == length(b))
  # get sum of counts across bins for a, b, a+b
  a.total <- sum(a)
  b.total <- sum(b)
  ab.total <- a.total +  b.total
  # calc p(i) for a and b seperately
  p.a <- a / a.total
  p.b <- b / b.total
  # form matrix with a,b counts, calc p(i,j) across a+b prob. space
  p.ab <- rbind(a,b) / ab.total
  # calc p(i) for a across a+b
  p.a.i.ab <- a / ab.total
  # calc p(j) for b across a+b
  p.b.j.ab <- b / ab.total
  # get total prob of an observation falling in a, given a+b
  p.a.ab <- sum(p.a.i.ab)
  # get total prob of an observation falling in b, given a+b    
  p.b.ab <- sum(p.b.j.ab)
  # get total p(i) in a+b
  p.i.ab <- (p.a.i.ab + p.b.j.ab)
  # form col of [p(a|ab),p(b|ab)]
  p.x <- c(p.a.ab,p.b.ab)
  # rename p(i) in a+b
  p.y <- p.i.ab
  # form matrix of expected probs based on [p(a|ab),p(b|ab)], p(i) in a+b
  p.null <- p.x %o% p.y
  # calc KL divergence between joint prob dist (a+b) and indep dists (a)(b)
  MI<-.KL.divergence(p.ab,p.null)
  if (normalize == T) {
    ent.a <- .entropy(a)
    ent.b <- .entropy(b)
    MI <- MI / (sqrt(ent.a*ent.b))
  }
  return(MI)
}

.get.bin.fullset <- function(bin.starts,bin.ends,t.0,t.end) {
  # take bin.starts vector, bin.ends vector, t.0 and t.end,
  # and return one vector of bin edges
  bin.fullset <- c(t.0,bin.starts,bin.ends,t.end)
  bin.fullset <- unique(sort(bin.fullset))
  return(bin.fullset)
}

.get.bin.sizes <- function(bin.set,pairs.only=F) {
  # take vector of bin edges and return bin sizes
  by.iter = 1
  if (pairs.only==T) {
    by.iter = 2
  } 
  stopifnot(length(bin.set) %% 2 == 0)
  bin.starts.i <- seq(1,length(bin.set)-1,by=by.iter)
  bin.ends.i <- seq(2,length(bin.set),by=by.iter)        
  bin.starts <- bin.set[bin.starts.i]
  bin.ends <- bin.set[bin.ends.i]
  return(bin.ends-bin.starts)
}

.entropy.electrode<-function(MEA,elec.name,bin.size=NA) {
  # return calculated entropy value for inputted elctrode name
  spikes <- MEA$spikes[[elec.name]]
  t.0 <- MEA$rec.time[1]
  t.end <- MEA$rec.time[2]
  spikes.pdist <- .pdist.electrode.spikes(spikes,t.0,t.end,
                                          bin.size=bin.size)
  ent <- .entropy(spikes.pdist,normalized.uniform=T)    
  return(ent)
}

.list.to.vals <- function(list.node,numeric=T) {
  # extract and return all values stored in list node
  vals <- c()
  for (name in names(list.node)) {
    if (numeric == T) {
      vals <- c(vals, as.numeric(list.node[[name]]))
    } else {
      vals <- c(vals, listnode[[name]])
    }
  }
  return(vals)
}

.wells.subset <- function(wells.node,wells) {
  wells.node.subset = list()
  for (name in intersect(names(wells.node),wells)) {
    wells.node.subset[[name]] <- wells.node[[name]]
  }
  return(wells.node.subset)
}
