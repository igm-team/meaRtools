\name{IGM.plot.plate.summary.for.bursts}
\alias{IGM.plot.plate.summary.for.bursts}
\title{
Plot burst features}
\description{
Plots all bursting features in a _burst_plot.pdf under the output directory.
}
\usage{
IGM.plot.plate.summary.for.bursts(s, outputdir,parameters)
}
\arguments{
  \item{s}{
MEA data structure
}
  \item{outputdir}{
Output directory}
  \item{parameters}{
meaRtools basic parameter list}
}
\details{
The plot function will plot all the features calculated for the bursts in the recording. Those include: Mean Firing Rate by Plate (Hz), Mean firing rate, Mean Duration, Number of bursts by channel and well, Mean Inter Burst Interval, Mean ISI within bursts, Mean burst per minute, Mean spikes in a burst and \% spikes in a burst.
The function also calls calc.burst.distributions to calculate and plot burst feature distributions.
}
\value{
A _burst_plot.pdf is printed under the output directory
}
\examples{
data("S")  
#IGM.plot.plate.summary.for.bursts(S,"/Analysis")
}
\keyword{ distribution }
\keyword{ burst }
