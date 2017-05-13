\name{write.features.to.files}
\alias{write.features.to.files}
\title{
Write feature data to an output file
}
\description{
Takes in list of dataframes (one per feature) from an MEA data structure that is produced by \link{IGM.aggregate.features} and writes output to Files. Each dataframe corresponds to one feature, containing values for each well across each DIV of recording

}
\usage{
write.features.to.files(s, features.list, output.dir, type) 
}
\arguments{
  \item{s}{MEA data structure}
  \item{features.list}{list of dataframes, one for each feature.}
  \item{output.dir}{Output directory)}
  \item{type}{Type of features (e.g. "spikes", "ns", "bursts")}
}
\value{Write one csv per feature for the feature type requested.}
\examples{
	data("S")
	s<-list()
	s[[1]]<-S
  spike.features = IGM.aggregate.features(s, "spike")

#  write.features.to.files(s, spike.features, analysis$output.dir, "spikes")
}
\keyword{spikes}
\keyword{bursts}
\keyword{network}
\keyword{features}
\keyword{print}
