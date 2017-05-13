\name{IGM.plot.plate.summary.for.spikes}
\alias{IGM.plot.plate.summary.for.spikes}
\title{
plot.plate.summary.for.spikes
}
\description{
Diana needs to add document here.
}
\usage{
IGM.plot.plate.summary.for.spikes(s, outputdir)
}
\arguments{
  \item{s}{
  's' .RData object. Each DIV must constitute one entry in list.
}
  \item{outputdir}{
directory path where plot will be saved to.
}
}

\value{
Multiple page plot in pdf format containing data on which electrodes have recorded any spikes, ISI (inter-spike interval) histogram by plate and by well by electrode, log ISI histogram by plate and by electrode, average electrode firing rate by well, & binned electrode firing rate over recording duration.

}

\author{
Diana Hall
}

\examples{

data("S") 
s<-list()
s[[1]]<-S
## plot.plate.summary.for.spikes(s, outputdir="/Desktop")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spikes }

