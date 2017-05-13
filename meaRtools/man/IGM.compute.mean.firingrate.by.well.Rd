\name{IGM.compute.mean.firingrate.by.well}
\alias{IGM.compute.mean.firingrate.by.well}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mean.firingrate.by.well
}
\description{
Creates a data frame with columns for well firing rate, mean electrode firing rate, well name and DIV.  See details for computations.
}
\usage{
IGM.compute.mean.firingrate.by.well(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
Well firing rate= total spikes per well/recording time. 
}
}
\details{
Well firing rate= total spikes per well/recording time. 
Electrode level firing rate= average across all electrodes in a well( total spikes on electrode/recording time)
}
\examples{
data("S") 

res<-IGM.compute.mean.firingrate.by.well(S)
res[1:4,]

}

\keyword{ spikes }
\keyword{ mfr }

