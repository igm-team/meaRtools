\name{plot_active_wells_network_spikes}
\alias{plot_active_wells_network_spikes}
\title{
plot_active_wells_network_spikes
}
\description{
Plots related to network spike for each well with network spikes in format of users choosing.
}
\usage{
plot_active_wells_network_spikes(nspikes)
}
\arguments{
  \item{nspikes}{
list of attributes related to network spikes: wells, plate layout and network spike information for each well.  See calculate_network_spikes for further details.
}
}
\value{
returns a multi-page plot. 
}

\seealso{

calculate_network_spikes
xyplot_network_spikes

}
\examples{
data("S") 
data('parameters')
nspikes <- calculate_network_spikes( S, parameters$sur ,parameters$ns_n, parameters$ns_t )
\donttest{
  pdf(file=NSPlotPath)
  xyplot.network.spikes(nspikes)	
  plot_active_wells_network_spikes(nspikes)
  dev.off()
}

}
\keyword{ network }
\keyword{ spikes }

