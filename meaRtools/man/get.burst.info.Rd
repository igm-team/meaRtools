\name{get_burst_info}
\alias{get_burst_info}
\title{
get burst feature information }
\description{
The function returns a list of values of a burst feature for a desired channel}
\usage{
get_burst_info(allb, index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{allb}{
The bursting features matrix of a channel (located in recording object - S object in example: S$allb[[channel number]]
}
  \item{index}{
Name of the requested burting feature. Can be "beg","end", "IBI","len", "durn", "mean_isis" or "SI"".}
}
\value{
List of values of the requested feature (index) for the desired channel.
}
\examples{
data("S")  
S$allb[[1]]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ burst }
\keyword{ IBI }
\keyword{ duration }% __ONLY ONE__ keyword per line
