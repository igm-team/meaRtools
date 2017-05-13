\name{parameters}
\alias{parameters}
\docType{data}
\title{
A list of parameters with default values that user can customize.
}
\description{
A list of parameters with default values that user can customize.
}
\usage{data("parameters")}
\format{
  The format is:
List of 20
 $ spike.csv                   : logi TRUE
 $ spike.plot                  : logi TRUE
 $ burst.csv                   : logi TRUE
 $ burst.plot                  : logi TRUE
 $ burst.type                  : chr "mi"
 $ s.min                       : num 5
 $ ns.csv                      : logi TRUE
 $ ns.plot                     : logi TRUE
 $ elec.min.rate               : num 0.0167
 $ elec.max.rate               : num 1000
 $ well.min.rate               : num 0
 $ mi.par                      :List of 5
  ..$ beg.isi   : num 0.1
  ..$ end.isi   : num 0.25
  ..$ min.ibi   : num 0.8
  ..$ min.durn  : num 0.05
  ..$ min.spikes: num 5
 $ ns.T                        : num 0.01
 $ ns.N                        : num 3
 $ sur                         : num 100
 $ burst.distribution.IBI      :List of 7
  ..$ perform      : num 1
  ..$ min.cases    : num 15
  ..$ x.lim        : num 20
  ..$ bins.in.seg  : num 5
  ..$ min.values   : num 0
  ..$ filter.by.min: num 0
  ..$ per.well     : num 0
 $ burst.distribution.durn     :List of 7
  ..$ perform      : num 1
  ..$ min.cases    : num 15
  ..$ x.lim        : num 18
  ..$ bins.in.seg  : num 10
  ..$ min.values   : num 0
  ..$ filter.by.min: num 0
  ..$ per.well     : num 0
 $ burst.distribution.ISI      :List of 7
  ..$ perform      : num 1
  ..$ min.cases    : num 15
  ..$ x.lim        : num 0.5
  ..$ bins.in.seg  : num 100
  ..$ min.values   : num 0
  ..$ filter.by.min: num 0
  ..$ per.well     : num 0
 $ burst.distribution.nSpikes  :List of 7
  ..$ perform      : num 1
  ..$ min.cases    : num 5
  ..$ x.lim        : num 200
  ..$ bins.in.seg  : num 1
  ..$ min.values   : num 0
  ..$ filter.by.min: num 0
  ..$ per.well     : num 0
 $ burst.distribution.spikeFreq:List of 7
  ..$ perform      : num 1
  ..$ min.cases    : num 15
  ..$ x.lim        : num 300
  ..$ bins.in.seg  : num 1
  ..$ min.values   : num 0
  ..$ filter.by.min: num 0
  ..$ per.well     : num 0
}

\examples{
data(parameters)
}
\keyword{datasets}
