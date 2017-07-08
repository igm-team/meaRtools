\name{isi}
\alias{isi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isi
}
\description{
calculates the isi (inter-spike interval) (s) between sucessive spikes in a input spike train.
}
\usage{
isi(train)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
spike train: a set of non-decreasing timestamps (s)
}
}
\value{
a vector of isis: first entry is ISI between first & second spike in input spike train and so forth. Total length is 1 less than input spike train.
}
\author{
Diana Hall
}
\examples{

data("S") # load data
b<-isi(S$spikes[[1]])
S$spikes[[1]][1:4]
b[1:3]

}

\keyword{ isi }
\keyword{ spikes }
\keyword{ interval }% __ONLY ONE__ keyword per line
