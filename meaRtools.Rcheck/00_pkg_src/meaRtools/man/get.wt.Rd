\name{get.wt}
\alias{get.wt}
\title{Get WT
}
\description{
Extracts all treatments/genotypes and allows user to choose single treatment as wild type/reference for downstream analyses
}
\usage{
get.wt(s)
}
\arguments{
  \item{s}{MEA dataframe structure}
}
\value{A string corresponding to the user's choice
}
\examples{
	data("S")
	s<-list()
	s[[1]]<-S
	##wt <- get.wt(s)
}
\keyword{ wt }
\keyword{ treatment }% __ONLY ONE__ keyword per line
