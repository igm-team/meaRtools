\name{write.plate.summary.for.bursts}
\alias{write.plate.summary.for.bursts}
\title{
Prints bursting features
}
\description{
The function reads the MEA data structure and uses the 'allb' list built using mi.find.bursts.  It then prints all bursting features summary per well and per channel in _bursts.csv and _well_bursts.csv}
\usage{
write.plate.summary.for.bursts(s, outputdir)
}
\arguments{
  \item{s}{
MEA data structure}
  \item{outputdir}{
Output directory}
}

\value{
Output file _bursts.csv holds all features generated for bursts per well and per channel:
\item{treatment}{the treatment/genotype based on the experimental log file plan}
\item{well}{well number}
\item{nAE}{number of active electrodes}
\item{nAB}{number of electrodes with bursts}
\item{duration}{total duration of bursts}
\item{mean.dur}{mean duration of bursts}
\item{mean.freq}{firing rate (Hz)}
\item{nbursts}{number of bursts}
\item{bursts.per.sec}{bursts/second.matrix(nrow=0,ncol=1)}
\item{bursts.per.min}{bursts/min}
\item{sd.dur}{sd of burst duration}
\item{mean.freq.in.burst}{average frequency of spikes in a burst}
\item{sd.freq.in.burst}{sd of frequency of spikes in a burst}


\item{mean.spikes.in.burst}{mean number of spikes in a burst}
\item{sd.spikes.in.burst }{sd of number of spikes in a burst}
\item{total.spikes.in.burst }{total number of spikes in a bursts}
\item{per.spikes.in.burst}{percent of spikes in a burst}

\item{mean.ISIs}{mean ISI within a burst}
\item{sd.ISIs}{sd ISI within a burst}  
\item{mean.IBIs}{mean IBI}
\item{sd.IBIs}{sd of IBIs}
\item{cv.IBIs}{Coefficient of variation of IBI (= mean.IBI/sd.IBI)}
\item{file}{input recording file}
}

\examples{
data("S")  
d<-dir.create(paste0(getwd(),"/Analysis") )
s<-list(); s[[1]]<-S
write.plate.summary.for.bursts(s, paste0(getwd() ) )
}
\keyword{ bursts }
\keyword{ IBI }
\keyword{ duration }
\keyword{ frequency }