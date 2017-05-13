\name{write.network.spikes.to.csv}
\alias{write.network.spikes.to.csv}
\title{
Summarize and write netowrk spikes features into a csv file.
}
\description{
Summarize and write netowrk spikes features into a csv file. 
}
\usage{
write.network.spikes.to.csv(s, nspikes, outputdir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{A list of MEA recordings, typically from the same MEA plate at different time point.
}
  \item{nspikes}{
  The spike list object returned from calling calculate.network.spikes.
}
  \item{outputdir}{
The user defined output directory while the cvs file to be writen. There should not have a file sperator at the end of the outputdir.
}
}

\value{
None.
}

\keyword{network spikes}

