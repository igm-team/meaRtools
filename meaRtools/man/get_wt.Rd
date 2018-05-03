\name{get_wt}
\alias{get_wt}
\title{Get WT
}
\description{
Extracts all treatments/genotypes and allows user to choose single treatment as wild type/reference for downstream analyses
}
\usage{
get_wt(s)
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
	\donttest{
	wt <- get_wt(s)
	}
}
\keyword{ wt }
\keyword{ treatment }% __ONLY ONE__ keyword per line
