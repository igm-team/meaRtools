\name{permute_features_and_plot}
\alias{permute_features_and_plot}
\title{Write PDF}
\description{Generates a PDF containing plots and p-values for each feature. P-values are generating using Mann Whitney and permutation tests. This function requires that you create a list of dataframes for a given feature type (e.g. spikes) using the aggregate.data() function}
\usage{permute_features_and_plot(s, wt, np, features_list, type, output_dir)}

\arguments{
  \item{s}{MEA data structure}
  \item{wt}{The treatment that will act as the wildtype/reference for the Mann Whitney and Permutation tests}
  \item{np}{Number of permutations to be performed}
  \item{features_list}{A list of dataframes containing data for a given feature}
  \item{type}{Type of features contained in features_list (e.g. spikes, ns, or bursts)}
  \item{output_dir}{Directory where output files will be generated}
}

\value{
A PDF file containing the plots and p-values. 
}

\author{
Ryan Dhindsa
}

\examples{
	data("S")
	#spike_features<-aggregate_features(S, feat_type="spike" )
	#wt <- "untreated"
	#output_dir = getwd()
<<<<<<< HEAD:meaRtools/man/permute_features_and_plot.Rd
	#permute_features_and_plot(S, wt, np, spike.features, "spikes", output_dir)}
=======
	#permute_features_and_plot(S, wt, np, spike_features, "spikes", output_dir)}
>>>>>>> MI:meaRtools/man/permute_features_and_plot.Rd

\keyword{MEA}
\keyword{analysis}
\keyword{PDF}
