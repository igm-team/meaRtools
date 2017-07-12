\name{nb_matrix_to_feature_dfs}
\alias{nb_matrix_to_feature_dfs}

\title{
Convert network burst data matrix to a list of data frames.
}
\description{
Convert network burst data matrix to a list of dataframes. Each dataframe has rows representing wells while columns representing different timepoints(DIVs). The dataframe format alllows well level permutaiton based tests to be done much easier. 
}
\usage{
nb_matrix_to_feature_dfs(matrix_and_feature_names)
}

\arguments{
  \item{matrix_and_feature_names}{ The data matrix return by calling function calculate_network_bursts.
}
}

\value{
Returns a list of dataframes, ith each representing a feature matrix, with rows for wells and columns for different timepoints(DIVs).
}

\author{
Quanli Wang
}

\seealso{
   \code{\link{calculate_network_bursts}}
}

\keyword{network bursts}
\keyword{permutation test}
