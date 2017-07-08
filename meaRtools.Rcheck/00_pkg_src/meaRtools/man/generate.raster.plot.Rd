\name{generate.raster.plot}
\alias{generate.raster.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate.raster.plot
}
\description{
Creates a pdf raster plot of selected user selected well from an 's' object. Options include verticle lines showing network spike times, vertical bars showing bursts as well as # showing count of spikes in burst and network spikes.
}
\usage{
generate.raster.plot(RobjectFile = NULL, outputdir = NULL, well.for.raster
                 = NULL, interval.for.raster = NULL, show.bursts = F,
                 show.burst.number = F, show.networkspikes = F,
                 show.ns.number = F,  show.nb=F, window.size=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RobjectFile}{
Default value is NULL, in which case tcltk pop-up file chooser will prompt user to select an 's' object.  Otherwise, provide a full path to to a .RData 's' object that contains burst and network data.
}

  \item{outputdir}{
   A directory (character string in quotes) where pdf is to be saved.
   Default is NULL, in which case the plot will be saved in the directory RobjectFile location.
}
  \item{well.for.raster}{
   A well name, character string, from plate. e.g. well.for.raster="A3".
   Default is NULL, in which case first well in plate will appear in plot.
}
  \item{interval.for.raster}{
     A vector of min and max time (s) for raster marks. e.g. interval.for.raster=c(30,60)
     Default is NULL, in which case the whole recording interval will be used.
}
  \item{show.bursts}{
  A boolean value sets whether bursts are indicated by red horizontal line (TRUE/FALSE)
  e.g. show.bursts=FALSE
  Default=FALSE
}
  \item{show.burst.number}{
 A boolean value sets whether # spikes/bursts are indicated (TRUE/FALSE).
 show.bursts must be set to true in order that show.burst.number=T
 e.g. show.burst.number=FALSE
 Default=FALSE
}
  \item{show.networkspikes}{
A boolean value sets whether network spikes are indicated by green vertical line (TRUE/FALSE)
  e.g. show.networkspikes=FALSE
   Default=FALSE
}
  \item{show.ns.number}{
A boolean value sets whether # electrodes in network spikes are indicated (TRUE/FALSE)
  e.g. show.ns.number=FALSE
   Default=FALSE
}
  \item{show.nb}{
A boolean value sets whether network bursts should be indicated in raster by orange horizontal lines (TRUE/FALSE)
  e.g. show.ns.number=FALSE
   Default=FALSE
}
  \item{ window.size }{
A numeric value indicating which of the three smoothing sizes available in the R-object should be used in network burst identification e.g. show.ns.number=10Default=NULL
}

}
\value{
A pdf raster plot will be displayed in system viewer.
}
\author{
%%  ~~who you are~~
Diana Hall
}
\examples{
##generate.raster.plot(RobjectFile=NULL,
#                      well.for.raster=NULL, 
#                      interval.for.raster=NULL,
#                      show.bursts=F, 
#                      show.burst.number=F, 
#                      show.networkspikes=F,
#                      show.ns.number=F,
#                      show.nb=F,
#                      window.size=NULL )

}
\keyword{ utility }
\keyword{ spikes }% __ONLY ONE__ keyword per line
