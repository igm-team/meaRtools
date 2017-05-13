\name{IGM.plot.active.wells.network.spikes}
\alias{IGM.plot.active.wells.network.spikes}
\title{
plot.active.wells.network.spikes
}
\description{
Plots related to network spike for each well with network spikes in format of users choosing.
}
\usage{
IGM.plot.active.wells.network.spikes(nspikes)
}
\arguments{
  \item{nspikes}{
list of attributes related to network spikes: wells, plate layout and network spike information for each well.  See calculate.network.spikes for further details.
}
}
\value{
returns a multi-page plot. 
}

\seealso{

calculate.network.spikes
xyplot.network.spikes

}
\examples{
data("S") 
data('parameters')
nspikes <- calculate.network.spikes( S, parameters$sur ,parameters$ns.N, parameters$ns.T )
## pdf(file=NSPlotPath)
## xyplot.network.spikes(nspikes)	
## plot.active.wells.network.spikes(nspikes)
## dev.off()

}
\keyword{ network }
\keyword{ spikes }

