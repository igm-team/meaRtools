\name{calc_burst_summary}
\alias{calc_burst_summary}
\title{
Calculate average and standard deviation of the bursting features.}
\description{
The function calculates a summary of all the bursting features and returns a data.frame with those values.}
\usage{
calc_burst_summary(s, bursty_threshold = 1)
}
\arguments{
  \item{s}{
MEA data structure
}
  \item{bursty_threshold}{
min number of  bursts/minute to count as a bursty unit.}
}

\value{
A data frame with the following columns:

\item{channels}{electrode name}
\item{spikes}{#spikes}
\item{mean_freq}{firing rate (Hz)}
\item{nbursts}{#bursts detected}
\item{bursts_per_sec}{#bursts/second.matrix(nrow=0,ncol=1)}
\item{bursts_per_min}{#bursts/min}
\item{bursty}{is bursts_per_min >bursty_threshold (defaults to 1 burst/min)}
\item{mean_dur}{mean burst duration}
\item{sd_dur}{sd}
\item{mean_spikes}{mean #spikes in a burst}
\item{sd_spikes }{sd}
\item{per_spikes_in_burst}{\% of spikes in a burst}
\item{per_spikes_out_burst}{\% of spikes not in a burst}
\item{mean_si}{mean Surprise Index (only for poisson .surprise measure)}
\item{mean_isis}{mean ISI within a burst}
\item{sd_mean_isis}{sd}  
\item{mean_ibis}{mean IBI}
\item{sd_ibis}{sd}
\item{cv_ibis}{Coefficient of variation of IBI (= mean.IBI/sd.IBI)}
}

\examples{
# Load exapmple of recording Robject (MEA data structure)
data("S")  
S$bs<-calc_burst_summary(S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ burst }
\keyword{ frequency }
\keyword{ duration }
\keyword{ bursts_per_min }
\keyword{ IBI }
\keyword{ isi }