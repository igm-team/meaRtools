\name{calculate_network_spikes}
\alias{calculate_network_spikes}
\title{
Compute the netwrok spikes statistics from spike lists.
}
\description{
Taken a spike list object for a set of electrodes, this function searches network spikes returns a list of all network spikes.
}
\usage{
calculate_network_spikes(e, sur = 100,ns_n, ns_t)
}
\arguments{
  \item{e}{
  A spike list object for a set of electrodes.
}
  \item{sur}{
  This parameter is related to the number of datapoints to be used in summmarizing mean network spikes, which will be only used for network spike diagnostics. The default value of 100 will usually be sufficient. 
}
  \item{ns_t}{global variable, time window of a network spike}
  \item{ns_n}{global variable, minimum number of coincident electrodes }
}

\value{
  Returns a list of object, containing network spikes.
  \item{wells }{A list of wells that network spikes were found and defined.}
  \item{ns_all }{A list of network spikes computed from the spike lists.}
  \item{well_layout}{The plate/well layout identified by the function.}
}
\references{
Eytan D, Marom S (2006) Dynamics and Effective Topology Underlying Synchronization in Networks of Cortical Neurons. Journal of Neuroscience 26:8465–8476.
}

\keyword{ network spikes }
\keyword{ spike list }
