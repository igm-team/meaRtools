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
 $ burst_type                  : chr "mi"
 $ s_min                       : num 5
 $ ns.csv                      : logi TRUE
 $ ns.plot                     : logi TRUE
 $ elec_min_rate               : num 0.0167
 $ elec_max_rate               : num 1000
 $ well_min_rate               : num 0
 $ mi.par                      :List of 5
  ..$ beg.isi   : num 0.1
  ..$ end.isi   : num 0.25
  ..$ min.ibi   : num 0.8
  ..$ min.durn  : num 0.05
  ..$ min.spikes: num 5
 $ ns_t                        : num 0.01
 $ ns_n                        : num 3
 $ sur                         : num 100
 $ burst_distribution_IBI      :List of 7
  ..$ perform      : num 1
  ..$ min_cases    : num 15
  ..$ x_axis_lim        : num 20
  ..$ bins_in_sec  : num 5
  ..$ min_values   : num 0
  ..$ filter_by_min: num 0
  ..$ per_well     : num 0
 $ burst_distribution_durn     :List of 7
  ..$ perform      : num 1
  ..$ min_cases    : num 15
  ..$ x_axis_lim        : num 18
  ..$ bins_in_sec  : num 10
  ..$ min_values   : num 0
  ..$ filter_by_min: num 0
  ..$ per_well     : num 0
 $ burst_distribution_ISI      :List of 7
  ..$ perform      : num 1
  ..$ min_cases    : num 15
  ..$ x_axis_lim        : num 0.5
  ..$ bins_in_sec  : num 100
  ..$ min_values   : num 0
  ..$ filter_by_min: num 0
  ..$ per_well     : num 0
 $ burst_distribution_nspikes  :List of 7
  ..$ perform      : num 1
  ..$ min_cases    : num 5
  ..$ x_axis_lim        : num 200
  ..$ bins_in_sec  : num 1
  ..$ min_values   : num 0
  ..$ filter_by_min: num 0
  ..$ per_well     : num 0
 $ burst_distribution_spike_freq:List of 7
  ..$ perform      : num 1
  ..$ min_cases    : num 15
  ..$ x_axis_lim        : num 300
  ..$ bins_in_sec  : num 1
  ..$ min_values   : num 0
  ..$ filter_by_min: num 0
  ..$ per_well     : num 0
}

\examples{
data(parameters)
}
\keyword{datasets}
