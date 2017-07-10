\name{dist_perm}
\alias{dist_perm}
\title{
Burst distribution permutations
}
\description{
Perform two statistical tests to quantify difference between two burst probability distributions using burst probability distribution data. Performed test are the Maximum Distance between cumulative distributions and Earth Movers Distance between the original probability distributions.
}
\usage{
dist_perm(datafile,np,type,kotype)
}
\arguments{
  \item{datafile}{ A _distributions.csv input file. Format as the output of calc_burst_distributions
}
  \item{np}{ Number of permutations to perform}
  \item{type}{ Name of first genotype}
  \item{kotype}{ Name of second genotype}
}

\value{
A list containing results of two statistical tests for the input probability distributions data.
\item{data.EMD }{Original value of EMD distance }
\item{data.EMD }{Original value of maximum distance }
\item{perm.EMD }{A permuted p.value of the EMD distance }
\item{perm.p }{A permuted p.value of the maximum distance }
\item{outp }{Maximum distances between genotypes for all permutations performed}
\item{outEMD }{Maximum Earth Movers Distance between genotypes for all permutations performed }
\item{data.wt }{Cumulative probabilities of the first genotype}
\item{data.ko }{Cumulative probabilities of the second genotype}
\item{data.wt.Original }{Probabilities distribution of the first genotype}
\item{data.ko.Original }{Probabilities distribution of the second genotype}
}
\references{
See https://redmine.igm.cumc.columbia.edu/projects/mea/wiki for further details
}
\examples{
#    result <- dist_perm(distributionFilePath,10000,"WT","KO")
}
\keyword{ distribution }
\keyword{ permutation }