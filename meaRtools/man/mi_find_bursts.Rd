\name{mi_find_bursts}
\alias{mi_find_bursts}
\title{
Find bursts}
\description{
For one spike train, find the bursts using the maximum interval method.}
\usage{
mi_find_bursts(spikes,mi_par)
}
\arguments{
  \item{spikes}{
A spike train of one channel, located in MEA data structure (example S$spikes[[1]]). }
  \item{mi_par}{ A list of burst features:\describe{
      \item{beg_isi}{Beginning inter spike interval}
      \item{end_isi}{Ending inter spike interval}
      \item{min_ibi}{Minimum inter burst interval to combine bursts}
      \item{min_durn}{Minimum duration to consider as burst}
      \item{min_spikes}{Minimum spikes to consider as burst}
  }
}
}
\value{
Returns a matrix of burst information for a specific channel.
Matrix columns are:
\item{beg}{the number of spike that is first in the burst}
\item{end}{number of the last spike in the burst}
\item{ibi}{time interval from previous burst }
\item{durn}{durarion of burst in seconds }
\item{mean_isis}{average inter spike interval within the burst }
\item{si}{surprise index, allways 1 for mi algorithm }
}
\references{
Eytan and Marom (2006) J Neuroscience.
}
\author{ Stephen Eglen }
\examples{
data("S")  
allb <- lapply(S$spikes, mi_find_bursts, S$parameters$mi_par )
}
\keyword{maximum interval}
\keyword{burst}
\keyword{IBI}
\keyword{allb}
