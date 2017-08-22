\name{calculate_entropy_and_mi}
\alias{calculate_entropy_and_mi}
\title{
Calculate Entropy and mututal information for each treatment level
}
\description{
Given an MEA recording, this function computes entropy and mutual information measures for each treatment level.
}
\usage{
calculate_entropy_and_mi(mea, treatments, mult_factor = 1.5, bin_size = 0.1)
}
\arguments{
  \item{mea}{
  The input mea spikelist object 
}
  \item{treatments}{
  The treatment levels that MI and entropy will be computed.
}
  \item{mult_factor}{
  The multiplication factor relating to the inter quartile range used in the algorithm. It serves as a tuning parameter with a default value of 1.5.
}
  \item{bin_size}{
  The bin size(in second) used to compute mutual information.
}
}

\value{
A list object holding MI and Entropy for each treatment level.
}

\examples{
library(meaRtools)

data(S)
mea <- filter_nonactive_spikes(S,spikes_per_minute_min=1)

treatments <- c("treatX", "treatY")
## compute entropies and MI's  
ENT.MI <- calculate_entropy_and_mi(mea, treatments, mult_factor=1.5, bin_size=0.1)
data_dists <- ENT.MI[["data_dists"]]
norm_mis_per_well <- ENT.MI[["norm_mis_per_well"]]

# test for difference in mean entropy between treatmentA, treatmentB
ent <- data_dists[["ENT"]]
ent.WT <- mean(ent[[treatments[1]]])
ent.MUT <- mean(ent[[treatments[2]]])
ent.res <- wilcox.test(ent[[treatments[1]]], ent[[treatments[2]]])
cat("entropy means (WT / MUT) :", ent.WT, "/", ent.MUT, "\n")
print(ent.res)

# test for diff in mutual info btwn treatmentA, treatmentB
mi <- data_dists[["MI"]]
mi.WT <- mean(mi[[treatments[1]]])
mi.MUT <- mean(mi[[treatments[2]]])
mi.res <- wilcox.test(mi[[treatments[1]]], mi[[treatments[2]]])
cat("mutual info means (WT / MUT) :", mi.WT, "/", mi.MUT, "\n") 
print(mi.res)

#plot(density(mi[[treatments[1]]]))
#lines(density(mi[[treatments[2]]]), col="red")

}

\keyword{Mutual Information }
\keyword{Entropy}
