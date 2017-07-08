\name{get.num.AE}
\alias{get.num.AE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get.num.AE
}
\description{
Adds a field to a 's' spike object 'nAE' that lists for each well the # of active electrodes (electodes firing > 5spike/minute).
}
\usage{
get.num.AE(s2)
}
\arguments{
  \item{s2}{
  an 's' object containing spike trains, channel names, etc.
}
}
\value{
returns 'nAE' field in 's' which is a vector of # of active electrodes (electodes firing > 5 spikes/minute).  Each vector entry is named by the well to which the data corresponds.
}
\author{
%%  ~~who you are~~
Diana Hall
}
\examples{
data("S") # load data
b<-get.num.AE(S)
b$nAE

}
\keyword{ spikes  }

