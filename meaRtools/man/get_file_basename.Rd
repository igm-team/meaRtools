\name{get_file_basename}
\alias{get_file_basename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_file_basename
}
\description{
Retreives the first 4 character strings separated by '_' from from a file path to a .RData object.
}
\usage{
get_file_basename(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
 a file name or full file path. filename must have file extension '.RData'.
}
}
\details{
filename must have file extension '.RData'.
}
\value{
Returns the first 4 character strings separated by '_' from from a file path to a .RData object.
}
\author{
%%  ~~who you are~~
Diana Hall
}
\examples{

data("S") # load data
get_file_basename(S$file)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spike-list }

