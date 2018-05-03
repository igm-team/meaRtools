\name{write_features_to_files}
\alias{write_features_to_files}
\title{
Write feature data to an output file
}
\description{
Takes in list of dataframes (one per feature) from an MEA data structure that is produced by \link{aggregate_features} and writes output to Files. Each dataframe corresponds to one feature, containing values for each well across each DIV of recording

}
\usage{
write_features_to_files(s, features_list, output_dir, type) 
}
\arguments{
  \item{s}{MEA data structure}
  \item{features_list}{list of dataframes, one for each feature.}
  \item{output_dir}{Output directory)}
  \item{type}{Type of features (e.g. "spikes", "ns", "bursts")}
}
\value{Write one csv per feature for the feature type requested.}
\examples{
	data("S")
	s<-list()
	s[[1]]<-S
  spike_features = aggregate_features(s, "spike")
  \donttest{
    write_features_to_files(s, spike_features, analysis$output_dir, "spikes")
  }
}
\keyword{spikes}
\keyword{bursts}
\keyword{network}
\keyword{features}
\keyword{print}
