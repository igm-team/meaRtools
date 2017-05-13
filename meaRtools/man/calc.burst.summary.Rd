\name{calc.burst.summary}
\alias{calc.burst.summary}
\title{
Calculate average and standard deviation of the bursting features.}
\description{
The function calculates a summary of all the bursting features and returns a data.frame with those values.}
\usage{
calc.burst.summary(s, bursty.threshold = 1)
}
\arguments{
  \item{s}{
MEA data structure
}
  \item{bursty.threshold}{
min number of  bursts/minute to count as a bursty unit.}
}

\value{
A data frame with the following columns:

\item{channels}{electrode name}
\item{spikes}{#spikes}
\item{mean.freq}{firing rate (Hz)}
\item{nbursts}{#bursts detected}
\item{bursts.per.sec}{#bursts/second.matrix(nrow=0,ncol=1)}
\item{bursts.per.min}{#bursts/min}
\item{bursty}{is bursts.per.min >bursty.threshold (defaults to 1 burst/min)}
\item{mean.dur}{mean burst duration}
\item{sd.dur}{sd}
\item{mean.spikes}{mean #spikes in a burst}
\item{sd.spikes }{sd}
\item{per.spikes.in.burst}{\% of spikes in a burst}
\item{per.spikes.out.burst}{\% of spikes not in a burst}
\item{mean.si}{mean Surprise Index (only for poisson .surprise measure)}
\item{mean.isis}{mean ISI within a burst}
\item{sd.mean.isis}{sd}  
\item{mean.IBIs}{mean IBI}
\item{sd.IBIs}{sd}
\item{cv.IBIs}{Coefficient of variation of IBI (= mean.IBI/sd.IBI)}
}

\examples{
# Load exapmple of recording Robject (MEA data structure)
data("S")  
S$bs<-calc.burst.summary(S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ burst }
\keyword{ frequency }
\keyword{ duration }
\keyword{ bursts.per.min }
\keyword{ IBI }
\keyword{ isi }