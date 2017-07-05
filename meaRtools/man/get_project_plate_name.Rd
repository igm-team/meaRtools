\name{get_project_plate_name}
\alias{get_project_plate_name}
\title{
get_project_plate_name
}
\description{
returns the first portion of file .RData spike object named according to convention of 
Project name, experiment date (MMDDYYYY format) and plate serial number separated by a '_' as in "exampleRecording_1012016_plate1_DIV1_spike_list.csv" in data package directory. 

}
\usage{
get_project_plate_name(file)
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
get_project_plate_name(S$file)
}
\keyword{ utility }

