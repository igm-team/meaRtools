\name{si_find_bursts}
\alias{si_find_bursts}
\title{
Find bursts}
\description{
For one spike train, find the bursts using the Poisson surprise method.}
\usage{
si_find_bursts(spikes,s_min,burst.isi.max)
}
\arguments{
  \item{spikes}{
A spike train of one channel, located in MEA data structure (example S$spikes[[1]]). }
  \item{s_min}{ A minimum value for the surprise index}
}
\value{
Returns a matrix of burst information for a specific channel.
Matrix columns are:
\item{beg}{the number of spike that is first in the burst}
\item{end}{number of the last spike in the burst}
\item{IBI}{time interval from previous burst }
\item{durn}{durarion of burst in seconds }
\item{mean_isis}{average inter spike interval within the burst }
\item{SI}{surprise index }
}
\references{
Eytan and Marom (2006) J Neuroscience.
}
\author{ Stephen Eglen }
\examples{
data("S")  
allb <- lapply(S$spikes, si_find_bursts, S$parameters$s.min )
}
\keyword{poisson surprise}
\keyword{burst}
