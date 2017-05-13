\name{IGM.write.UI.to.log}
\alias{IGM.write.UI.to.log}
\title{
IGM.write.UI.to.log
}
\description{
Writes a named list e.g. parameters or file paths to a vector of specified file paths.
Useful for documenting the parameters and files used in an analysis.
}
\usage{
IGM.write.UI.to.log(files=NULL,parameterList, new.file=F )
}
\arguments{
  \item{files}{
  vector of full file paths to already created .txt files where parameterList content will be written. e.g. files=c('/Desktop/log1.txt', '/Desktop/output/log2.txt)
  }
  \item{parameterList}{
  A named list of containing pertinent information. An entry of parameterList can itself be a named list, in which case the resulting output file with write the names and contents of each sublist.
 }
 \item{new.file}{
 new.file=TRUE will overwrite any existing file, restarting the log in effect.
 Default: new.file=FALSE
 }
}
\value{
writes to .txt files specified.
}
\author{
Diana Hall
}
\examples{
# IGM.write.UI.to.log(files='/Desktop/logfile.txt',parameterList, new.file=F )
}

\keyword{ log }

