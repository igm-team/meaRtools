\name{S}
\alias{S}
\docType{data}
\title{
example 'S' object 
}
\description{
An example 'S' list object containing multiple fields describing 1 minute recording on a 48 well plate.
}
\usage{data("S")}
\format{
\describe{
    \item{channels	}{electrode names } 
    \item{spikes}{ a list of spike trains for each channel}
    \item{nspikes}{# spikes for each channel}
    \item{NCells}{total # electrodes}
    \item{meanfiringrate}{mean firing rate by channel}
    \item{file}{full path of file}
    \item{layout}{electrodes grid positions for all electrodes on plate}
    \item{rates}{list with average count and firing per time_interval (s) as well as plate average}
    \item{rec_time}{2 element vector of first and last spike time of recording}
    \item{goodwells}{well names for all wells meeting minimum firing criteria}
    \item{treatment}{treatments for each well}
    \item{size}{chemical compound size for each treatment}
    \item{units}{units of dose of treatment}
    \item{dose}{ dose of treatment}
    \item{well}{well names}
    \item{nae}{# active electrodes (firing>5spikes/min)}
    \item{cw}{wells that each channel belongs to }
    \item{parameters}{ A list of parameters, see data("parameters") }
    \item{allb}{for each electrode, a matrix of burst related information }
    \item{bs}{burst summary, a data frame containing burst endpoints by electrode}
    \item{ns_all}{for each well, a list of network spike information}
    \item{isis}{list of inter-spike interval (isi) (s) by channel}
    \item{mean_isis}{list of average isi by channel}
    \item{sd_isis}{list of standard deviation of isi by channel}
    \item{well_stats}{data frame containing well level firing rate information}

    }
}
\details{
Created by use of functions available in package. 
}

\examples{
data('S')
names(S)
}
\keyword{datasets}
