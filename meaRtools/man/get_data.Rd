\name{get_data}
\alias{get_data}
\title{
get_data
}
\description{
pop up file chooser with caption. Also, sets directory of analysis output.
}
\usage{
get_data(caption = "")
}
\arguments{
  \item{caption}{
 text to display in pop-up file chooser to prompt user to select appropriate file.
 Default is no caption.
}
}
\value{
Creates 2 directories:

'Analysis' directory in the parent directory of user selected file.

'R_objects' a subdirectory of 'Analysis'
}

\examples{
\donttest{
 get_data(caption="Please select a spike-list file for analysis")
}
}
\keyword{ s-object }

