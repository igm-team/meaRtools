\name{calculate_entropy_and_mi}
\alias{calculate_entropy_and_mi}
\title{
Calculate Entropy and mututal information for each treatment level
}
\description{
Given an MEA recording, this function computes entropy and mutual information measures for each treatment level.
}
\usage{
calculate_entropy_and_mi(mea, treatments, mult_factor = 1.5, bin_size = 0.1)
}
\arguments{
  \item{mea}{
  The input mea spikelist object 
}
  \item{treatments}{
  The treatment levels that MI and entropy will be computed.
}
  \item{mult_factor}{
  The multiplication factor relating to the inter quartile range used in the algorithm. It serves as a tuning parameter with a default value of 1.5.
}
  \item{bin_size}{
  The bin size(in second) used to compute mutual information.
}
}

\value{
A list object holding MI and Entropy for each treatment level.
}

\keyword{Mutual Information }
\keyword{Entropy}
