\name{si.find.bursts}
\alias{si.find.bursts}
\title{
Find bursts}
\description{
For one spike train, find the bursts using the Poisson surprise method.}
\usage{
si.find.bursts(spikes,s.min,burst.isi.max)
}
\arguments{
  \item{spikes}{
A spike train of one channel, located in MEA data structure (example S$spikes[[1]]). }
  \item{s.min}{ A minimum value for the surprise index}
  \item{burst.isi.max}{ ISI threshold used by the the surprise index algorithm.}
  
}
\value{
Returns a matrix of burst information for a specific channel.
Matrix columns are:
\item{beg}{the number of spike that is first in the burst}
\item{end}{number of the last spike in the burst}
\item{IBI}{time interval from previous burst }
\item{durn}{durarion of burst in seconds }
\item{mean.isis}{average inter spike interval within the burst }
\item{SI}{surprise index }
}
\references{
Eytan and Marom (2006) J Neuroscience.
}
\author{ Stephen Eglen }
\examples{
data("S")  
allb <- lapply(S$spikes, si.find.bursts, S$parameters$s.min )
}
\keyword{poisson surprise}
\keyword{burst}
