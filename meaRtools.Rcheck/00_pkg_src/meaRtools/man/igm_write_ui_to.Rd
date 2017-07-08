\name{igm_write_ui_to_log}
\alias{igm_write_ui_to_log}
\title{
igm_write_ui_to_log
}
\description{
Writes a named list e.g. parameters or file paths to a vector of specified file paths.
Useful for documenting the parameters and files used in an analysis.
}
\usage{
igm_write_ui_to_log(files=NULL,parameter_list, new_file=F )
}
\arguments{
  \item{files}{
  vector of full file paths to already created .txt files where parameter_list content will be written. e.g. files=c('/Desktop/log1.txt', '/Desktop/output/log2.txt)
  }
  \item{parameter_list}{
  A named list of containing pertinent information. An entry of parameter_list can itself be a named list, in which case the resulting output file with write the names and contents of each sublist.
 }
 \item{new_file}{
 new_file=TRUE will overwrite any existing file, restarting the log in effect.
 Default: new_file=FALSE
 }
}
\value{
writes to .txt files specified.
}
\author{
Diana Hall
}
\examples{
# igm_write_ui_to_log(files='/Desktop/logfile.txt',parameter_list, new_file=F )
}

\keyword{ log }

