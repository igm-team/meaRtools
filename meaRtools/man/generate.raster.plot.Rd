\name{generate_raster_plot}
\alias{generate_raster_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate_raster_plot
}
\description{
Creates a pdf raster plot of selected user selected well from an 's' object. Options include verticle lines showing network spike times, vertical bars showing bursts as well as # showing count of spikes in burst and network spikes.
}
\usage{
generate_raster_plot(r_object_file = NULL, outputdir = NULL, well_for_raster
                 = NULL, interval_for_raster = NULL, show_bursts = F,
                 show_burst_number = F, show_networkspikes = F,
                 show_ns_number = F,  show_nb=F, window_size=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r_object_file}{
Default value is NULL, in which case tcltk pop-up file chooser will prompt user to select an 's' object.  Otherwise, provide a full path to to a .RData 's' object that contains burst and network data.
}

  \item{outputdir}{
   A directory (character string in quotes) where pdf is to be saved.
   Default is NULL, in which case the plot will be saved in the directory r_object_file location.
}
  \item{well_for_raster}{
   A well name, character string, from plate. e.g. well_for_raster="A3".
   Default is NULL, in which case first well in plate will appear in plot.
}
  \item{interval_for_raster}{
     A vector of min and max time (s) for raster marks. e.g. interval_for_raster=c(30,60)
     Default is NULL, in which case the whole recording interval will be used.
}
  \item{show_bursts}{
  A boolean value sets whether bursts are indicated by red horizontal line (TRUE/FALSE)
  e.g. show_bursts=FALSE
  Default=FALSE
}
  \item{show_burst_number}{
 A boolean value sets whether # spikes/bursts are indicated (TRUE/FALSE).
 show_bursts must be set to true in order that show_burst_number=T
 e.g. show_burst_number=FALSE
 Default=FALSE
}
  \item{show_networkspikes}{
A boolean value sets whether network spikes are indicated by green vertical line (TRUE/FALSE)
  e.g. show_networkspikes=FALSE
   Default=FALSE
}
  \item{show_ns_number}{
A boolean value sets whether # electrodes in network spikes are indicated (TRUE/FALSE)
  e.g. show_ns_number=FALSE
   Default=FALSE
}
  \item{show_nb}{
A boolean value sets whether network bursts should be indicated in raster by orange horizontal lines (TRUE/FALSE)
  e.g. show_ns_number=FALSE
   Default=FALSE
}
  \item{ window_size }{
A numeric value indicating which of the three smoothing sizes available in the R-object should be used in network burst identification e.g. show_ns_number=10Default=NULL
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
\donttest{
generate_raster_plot(r_object_file=NULL,
                      well_for_raster=NULL, 
                      interval_for_raster=NULL,
                      show_bursts=F, 
                      show_burst_number=F, 
                      show_networkspikes=F,
                      show_ns_number=F,
                      show_nb=F,
                      window_size=NULL )
}

}
\keyword{ utility }
\keyword{ spikes }% __ONLY ONE__ keyword per line
