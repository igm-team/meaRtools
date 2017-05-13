\name{IGM.plot.mean.firingrate.by.well.by.div}
\alias{IGM.plot.mean.firingrate.by.well.by.div}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.mean.firingrate.by.well.by.div
}
\description{
Displays average firing rate by well for each DIV available.  First plot well rate in average Hz/electrode and second plot is Hz/total spikes well.
}
\usage{
IGM.plot.mean.firingrate.by.well.by.div(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
 's' object. must be a list, with each DIV a different entry.
}
}

\value{
Plot is output, location and path to plot may be controlled by R's plotting apparatus e.g. 'pdf()'
}
\author{
%%  ~~who you are~~
Diana Hall  
}
\examples{
data("S") 
s<-list()
s[[1]]<-S
#plot.mean.firingrate.by.well.by.div(s)

}
\keyword{ firing }
\keyword{ rate }
\keyword{ spikes }% __ONLY ONE__ keyword per line
