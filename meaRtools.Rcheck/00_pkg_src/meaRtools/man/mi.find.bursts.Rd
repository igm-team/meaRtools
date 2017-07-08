\name{mi.find.bursts}
\alias{mi.find.bursts}
\title{
Find bursts}
\description{
For one spike train, find the bursts using the maximum interval method.}
\usage{
mi.find.bursts(spikes,mi.par)
}
\arguments{
  \item{spikes}{
A spike train of one channel, located in MEA data structure (example S$spikes[[1]]). }
  \item{mi.par}{ A list of burst features:\describe{
      \item{beg.isi}{Beginning inter spike interval}
      \item{end.isi}{Ending inter spike interval}
      \item{min.ibi}{Minimum inter burst interval to combine bursts}
      \item{min.durn}{Minimum duration to consider as burst}
      \item{min.spikes}{Minimum spikes to consider as burst}
  }
}
}
\value{
Returns a matrix of burst information for a specific channel.
Matrix columns are:
\item{beg}{the number of spike that is first in the burst}
\item{end}{number of the last spike in the burst}
\item{IBI}{time interval from previous burst }
\item{durn}{durarion of burst in seconds }
\item{mean_isis}{average inter spike interval within the burst }
\item{SI}{surprise index, allways 1 for mi algorithm }
}
\references{
Eytan and Marom (2006) J Neuroscience.
}
\author{ Stephen Eglen }
\examples{
data("S")  
allb <- lapply(S$spikes, mi.find.bursts, S$parameters$mi.par )
}
\keyword{maximum interval}
\keyword{burst}
\keyword{IBI}
\keyword{allb}
