\name{calculate_burst_features}
\alias{calculate_burst_features}

\title{
Filter spikes and bursts in recording objects
}
\description{
Apply user defined filters on the spikes that were recorded and calculate spike features.
}
\usage{
calculate_burst_features(s)
}

\arguments{
  \item{s}{ A spikelist object returned from calling calculate_spike_features.}
}

\value{
Returns an 's' object containing all the spikes and bursts of all the loaded recording Robjects.
}
\author{
Diana Hall
}

\keyword{ spikes }
\keyword{ filter }
