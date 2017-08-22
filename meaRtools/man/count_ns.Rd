\name{count_ns}
\alias{count_ns}
\title{
Count number of spikes within evenly spaced time intervals(bins) from input spike trains. 
}
\description{
Given a list of spike trains, this function creates evenly spaced bins and returns number of spikes from all spike trains for each bins.
}
\usage{
count_ns(spikes, beg, end, wid, nbins)
}
\arguments{
  \item{spikes}{
  The input list of spike trains.
}
  \item{beg}{
  Start time of recording in seconds.
}
  \item{end}{
  End time of recording in seconds.
}
  \item{wid}{
  Bin width in seconds.
}
  \item{nbins}{
  Number of evenly spaced bins for given time interval.
}
}

\value{
   Return a vector of counts of spikes from all spike trains for user-defined, evenly spaced bins.
}

\author{
Stephen Eglen
}



