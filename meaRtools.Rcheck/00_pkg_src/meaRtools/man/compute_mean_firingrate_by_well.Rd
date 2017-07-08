\name{compute_mean_firingrate_by_well}
\alias{compute_mean_firingrate_by_well}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mean.firingrate.by.well
}
\description{
Creates a data frame with columns for well firing rate, mean electrode firing rate, well name and DIV.  See details for computations.
}
\usage{
compute_mean_firingrate_by_well(s)
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

res<-compute_mean_firingrate_by_well(S)
res[1:4,]

}

\keyword{ spikes }
\keyword{ mfr }

