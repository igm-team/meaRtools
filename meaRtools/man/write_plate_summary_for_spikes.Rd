\name{write_plate_summary_for_spikes}
\alias{write_plate_summary_for_spikes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
write_plate_summary_for_spikes
}
\description{
Produces csv output related to firing rate by DIV to directory of user specified output directory
}
\usage{
write_plate_summary_for_spikes(s, outputdir)
}
\arguments{
  \item{s}{
's' spike .RData object. Must be a list with one entry per DIV.
}
  \item{outputdir}{

}
}

\value{
One .csv file for each DIV is output and one additional file comprising all DIVs. Quantification of activity levels including total spike count, well and elctrode level firing rate, as well as ISI and standard deviation of applicable features.
}
\author{
Diana Hall
}
\examples{
data("S") 
s<-list()
s[[1]]<-S
\donttest{
  path<-system.file()
  write_plate_summary_for_spikes(s , path)
}
}
\keyword{ spikes }
