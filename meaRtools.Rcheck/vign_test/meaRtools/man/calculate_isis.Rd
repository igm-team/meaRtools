\name{calculate_isis}
\alias{calculate_isis}
\title{
Calculate inter spike intervals}
\description{
The function calculates all the interspikes interval between all spikes of each of the channels recorded.}
\usage{
calculate_isis(s)
}
\arguments{
  \item{s}{
MEA data structure
}
}

\value{
Returns the MEA data structure (S object in the example) with the following new lists:
\item{S$isis}{list of all isis for each channel}
\item{S$mean_isis}{mean isis for each channel}
\item{S$sd_isis}{sd of isis for each channel}
}
\examples{
data("S")  
S <- calculate_isis(S)
}
\keyword{ isis }
\keyword{ inter spike interval }