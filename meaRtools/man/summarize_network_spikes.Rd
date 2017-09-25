\name{summarize_network_spikes}
\alias{summarize_network_spikes}

\title{
Generate network spikes based features.
}
\description{
This function takes the returned object from calcualte.network.spikes function and parse and filter them using customed filters to regenerate features used by IGM MEA projects. 
}
\usage{
summarize_network_spikes(e, nspikes, ns_e, sur)
}
\arguments{
  \item{e}{
  A spike list object for a set of electrodes.
}
  \item{nspikes}{
  The spike list object returned from calling calculate_network_spikes.
}
  \item{ns_e}{
  Minumum number of spikes for each electrode within the network spike window. Most IGM MEA projects use a value of 2.
}
  \item{sur}{
This parameter is related to the number of datapoints to be used in summmarizing mean network spikes, which will be only used for network spike diagnostics. The default value of 100 will usually be sufficient. 
}
}

\value{
Returns a new spikes object with filtered and re-calcualted features.
}

\author{
Quanli Wang
}

\keyword{network spikes}

