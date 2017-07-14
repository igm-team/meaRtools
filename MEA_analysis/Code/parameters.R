parameters<- list( 
  # spikes
  spike.csv=TRUE,
  spike.plot=TRUE,
  
  
  # Burst 
  burst.csv=TRUE,
  burst.plot=TRUE,
  
  
  # network spikes 
  ns.csv=TRUE,
  ns.plot=TRUE,
  
  burst.type="mi" ,
  s.min=5 ,
  perm.n =100,
  
  elec.min.rate=0.016,
  elec.max.rate=1000,
  well.min.rate=4,
  well.filter.maximum.DIV.inactive.ratio=0.5,
  
  mi.par=list(beg.isi =0.1,
              end.isi =0.25,
              min.ibi =0.3,
              min.durn =0.05,
              min.spikes = 5),
  
  
  ns.T=0.01,
  ns.N=3,
  sur=100,
  
  # Distributions IBI
  burst.distribution.IBI=list(
    perform = 1,
    min.cases = 15,
    x.lim = 20,
    bins.in.seg = 5,
    min.values = 0,
    filter.by.min = 0,
    per.well = 0), 
  
  # burst duration distribution parameters
  burst.distribution.durn=list(
    perform = 1,
    min.cases = 15,
    x.lim = 18,
    bins.in.seg = 5,
    min.values = 0,
    filter.by.min = 0,
    per.well = 0), 
  
  # burst distribution ISI parameters
  burst.distribution.ISI=list(
    perform = 1,
    min.cases = 15,
    x.lim = 0.5,
    bins.in.seg = 100,
    min.values = 0,
    filter.by.min = 0,
    per.well = 0), 
  
  # burst distribution nSpikes parameters
  burst.distribution.nSpikes=list(
    perform = 1,
    min.cases = 5,
    x.lim = 200,
    bins.in.seg = 1,
    min.values = 0,
    filter.by.min = 0,
    per.well = 0) ,
  
  # burst distribution spikeFreq parameters
  burst.distribution.spikeFreq=list(
    perform = 1,
    min.cases = 15,
    x.lim = 300,
    bins.in.seg = 1,
    min.values = 0,
    filter.by.min = 0,
    per.well = 0), 
  
  
  want.nb=TRUE,
  
  local_region_min_nAE= 0,
  min_electrodes = 5,
  Sigma= c(10,20,50),
  
  timeStamp="DATE_TIME"
)