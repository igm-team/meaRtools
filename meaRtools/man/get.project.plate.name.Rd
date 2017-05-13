\name{get.project.plate.name}
\alias{get.project.plate.name}
\title{
get.project.plate.name
}
\description{
returns the first portion of file .RData spike object named according to convention of 
Project name, experiment date (MMDDYYYY format) and plate serial number separated by a '_' as in "exampleRecording_1012016_plate1_DIV1_spike_list.csv" in data package directory. 

}
\usage{
get.project.plate.name(file)
}
\arguments{
  \item{file}{
a full file path or file name 
}
}
\value{
Returns a character string of the project name, experiment date and plate serial number in a .RData file path. see example.

}

\examples{
data("S") # load data
get.project.plate.name(S$file)
}
\keyword{ utility }

