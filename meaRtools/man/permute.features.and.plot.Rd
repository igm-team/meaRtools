\name{permute.features.and.plot}
\alias{permute.features.and.plot}
\title{Write PDF}
\description{Generates a PDF containing plots and p-values for each feature. P-values are generating using Mann Whitney and permutation tests. This function requires that you create a list of dataframes for a given feature type (e.g. spikes) using the aggregate.data() function}
\usage{permute.features.and.plot(s, wt, np, features.list, type, output.dir)}

\arguments{
  \item{s}{MEA data structure}
  \item{wt}{The treatment that will act as the wildtype/reference for the Mann Whitney and Permutation tests}
  \item{np}{Number of permutations to be performed}
  \item{features.list}{A list of dataframes containing data for a given feature}
  \item{type}{Type of features contained in features.list (e.g. spikes, ns, or bursts)}
  \item{output.dir}{Directory where output files will be generated}
}

\value{
A PDF file containing the plots and p-values. 
}

\author{
Ryan Dhindsa
}

\examples{
	data("S")
	#spike.features<-IGM.aggregate.features(S, feat.type="spike" )
	#wt <- "untreated"
	#output.dir = getwd()
	#permute.features.and.plot(S, wt, np, spike.features, "spikes", output.dir)}

\keyword{MEA}
\keyword{analysis}
\keyword{PDF}
