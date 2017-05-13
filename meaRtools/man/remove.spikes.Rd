\name{remove.spikes}
\alias{remove.spikes}
\title{
remove.spikes
}
\description{
removes all spikes and associated meta data from 's' spike object except those specified by 'ids'.
}
\usage{
remove.spikes(s, ids)
}
\arguments{
  \item{s}{
's' list object, needs to contain a 'spikes' field with spike train 
}
  \item{ids}{
Name or index of channel(s) to be kept, all other channels removed.
either name of channel, e.g. "E5_12" or an vector of idices c(1,2) corresponding to channel index. If a negative index is given, then that channel and associated data will be removed. 

}
}

\value{
's' object. 
}

\seealso{
construct.s
}
\examples{
data("S") # load data
r<-remove.spikes(S, c(-1, -2))

S$channels[1:2] # original 's' object first 2 channels
r$channels[1:2] # first 2 channels have been removed

S$NCells # original count of channels
r$NCells # count of channels after 2 channels removed

S$nspikes # original spike count of first 2 channels
r$nspikes # spike count of first 2 channels after 2 channels removed

# OR keep only first 2 channels
t<-remove.spikes(S, c(1, 2))
t$channels
}
\keyword{ spikes }

