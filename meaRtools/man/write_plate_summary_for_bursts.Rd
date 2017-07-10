\name{write_plate_summary_for_bursts}
\alias{write_plate_summary_for_bursts}
\title{
Prints bursting features
}
\description{
The function reads the MEA data structure and uses the 'allb' list built using mi.find.bursts.  It then prints all bursting features summary per well and per channel in _bursts.csv and _well_bursts.csv}
\usage{
write_plate_summary_for_bursts(s, outputdir)
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
\item{mean_dur}{mean duration of bursts}
\item{mean_freq}{firing rate (Hz)}
\item{nbursts}{number of bursts}
\item{bursts_per_sec}{bursts/second.matrix(nrow=0,ncol=1)}
\item{bursts_per_min}{bursts/min}
\item{sd_dur}{sd of burst duration}
\item{mean_freq_in_burst}{average frequency of spikes in a burst}
\item{sd_freq_in_burst}{sd of frequency of spikes in a burst}


\item{mean_spikes_in_burst}{mean number of spikes in a burst}
\item{sd_spikes_in_burst }{sd of number of spikes in a burst}
\item{total_spikes_in_burst }{total number of spikes in a bursts}
\item{per_spikes_in_burst}{percent of spikes in a burst}

\item{mean_ISIs}{mean ISI within a burst}
\item{sd_ISIs}{sd ISI within a burst}  
\item{mean_IBIs}{mean IBI}
\item{sd_IBIs}{sd of IBIs}
\item{cv_IBIs}{Coefficient of variation of IBI (= mean.IBI/sd.IBI)}
\item{file}{input recording file}
}

\examples{
data("S")  
d<-dir.create(paste0(getwd(),"/Analysis") )
s<-list(); s[[1]]<-S
write_plate_summary_for_bursts(s, paste0(getwd() ) )
}
\keyword{ bursts }
\keyword{ IBI }
\keyword{ duration }
\keyword{ frequency }