\name{get_num_ae}
\alias{get_num_ae}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_num_ae
}
\description{
Adds a field to a 's' spike object 'nae' that lists for each well the # of active electrodes (electodes firing > 5spike/minute).
}
\usage{
get_num_ae(s2)
}
\arguments{
  \item{s2}{
  an 's' object containing spike trains, channel names, etc.
}
}
\value{
returns 'nae' field in 's' which is a vector of # of active electrodes (electodes firing > 5 spikes/minute).  Each vector entry is named by the well to which the data corresponds.
}
\author{
%%  ~~who you are~~
Diana Hall
}
\examples{
data("S") # load data
b<-get_num_ae(S)
b$nae

}
\keyword{ spikes  }

