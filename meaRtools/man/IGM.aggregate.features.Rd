\name{IGM.aggregate.features}
\alias{IGM.aggregate.features}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aggregate Feature Data
}
\description{
Takes data from S object (MEA data structure) and makes a list of dataframes. Each dataframe corresponds to one feature, containing values for each well across each DIV of recording
}
\usage{
IGM.aggregate.features(s, feat.type, parameters)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{MEA data structure}
  \item{feat.type}{Type of features (e.g. "spikes", "ns", "bursts")}
  \item{parameters}{ A list of parameters, see data("parameters")  }
}
\value{A list of dataframes for a given set of features}
\examples{
	data("S")
	data("parameters")
	s<-list()
	s[[1]]<-S
	spike.features = suppressWarnings( IGM.aggregate.features(s, "spike", parameters))
	ns.features = suppressWarnings( IGM.aggregate.features(s, "ns", parameters) )
	burst.features = suppressWarnings( IGM.aggregate.features(s, "burst", parameters) )
}
\keyword{MEA}
\keyword{Aggregate}% __ONLY ONE__ keyword per line
