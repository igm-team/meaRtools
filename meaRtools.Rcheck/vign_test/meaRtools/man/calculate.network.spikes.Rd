\name{calculate.network.spikes}
\alias{calculate.network.spikes}
\title{
Compute the netwrok spikes statistics from spike lists.
}
\description{
Taken a spike list object for a set of electrodes, this function searches network spikes returns a list of all network spikes.
}
\usage{
calculate.network.spikes(e, sur = 100,ns.N, ns.T)
}
\arguments{
  \item{e}{
  A spike list object for a set of electrodes.
}
  \item{sur}{
  This parameter is related to the number of datapoints to be used in summmarizing mean network spikes, which will be only used for network spike diagnostics. The default value of 100 will usually be sufficient. 
}
  \item{ns.T}{global variable, time window of a network spike}
  \item{ns.N}{global variable, minimum number of coincident electrodes }
}

\value{
  Returns a list of object, containing network spikes.
  \item{wells }{A list of wells that network spikes were found and defined.}
  \item{ns.all }{A list of network spikes computed from the spike lists.}
  \item{well.layout}{The plate/well layout identified by the function.}
}
\references{
Need to find the paper describe this method.
}

\keyword{ network spikes }
\keyword{ spike list }
