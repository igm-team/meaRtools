\name{calculate.network.bursts}
\alias{calculate.network.bursts}
\title{
Compute network bursts for a list of MEA recordings.
}
\description{
For a list of MEA recordings, ususally from the same plate at different time point,This function detects and report network burst features at the well level.}
\usage{
calculate.network.bursts(s,Sigma, min_electrodes, local_region_min_nAE)
}
\arguments{
  \item{s}{ 
  A list of MEA recordings, typically from the same MEA plate at different time point.}
  \item{Sigma}{
  The window size used to generate network bursts.}
  \item{min_electrodes}{ 
  Minimum number of electrodes to call a network burst}
  \item{local_region_min_nAE}{
  Indicates if an adaptive threthold method should be used.}
}
\value{
Returns an object containing summary, nb_all, nb_features, result, and nb_features_merged.

  \item{summary}{ brief summary }
  \item{nb_all}{ Each well has 3 data frames with nb times, one for each smoothing window }
  \item{nb_features}{ a list containing a data-frame for each DIV analyzed }
  \item{ result }{ for each DIV analyzed, information on the DIV, times of nb for each well and each smoothing window }
  \item{ nb_features_merged }{ data frame with nb related features averaged across DIVs }

  
}
\references{
Add reference to Yi-Fan Lu's paper when it is in press.
}
\author{
Quanli Wang
}


\keyword{ network bursts }
