rm(list = ls())
library(meaRtools)

# load MEA data from h5 file (can also be in form 'Rdata')
data(S)
mea <- filter.nonactive.spikes(S,spikes.per.minute.min=1)

treatments <- c("treatX", "treatY")
## compute entropies and MI's  
ENT.MI <- calculate.entropy.and.MI(mea,treatments, mult.factor=1.5, bin.size=0.1)
data.dists <- ENT.MI[["data.dists"]]
norm.MIs.per.well <- ENT.MI[["norm.MIs.per.well"]]

# test for difference in mean entropy between treatmentA, treatmentB
ent <- data.dists[["ENT"]]
ent.WT <- mean(ent[[treatments[1]]])
ent.MUT <- mean(ent[[treatments[2]]])
ent.res <- wilcox.test(ent[[treatments[1]]], ent[[treatments[2]]])
cat("entropy means (WT / MUT) :", ent.WT, "/", ent.MUT, "\n")
print(ent.res)

# test for diff in mutual info btwn treatmentA, treatmentB
mi <- data.dists[["MI"]]
mi.WT <- mean(mi[[treatments[1]]])
mi.MUT <- mean(mi[[treatments[2]]])
mi.res <- wilcox.test(mi[[treatments[1]]], mi[[treatments[2]]])
cat("mutual info means (WT / MUT) :", mi.WT, "/", mi.MUT, "\n") 
print(mi.res)

# make boxplots                                                                 
boxplot(norm.MIs.per.well[[treatments[1]]], main=treatments[1])                                 
boxplot(norm.MIs.per.well[[treatments[2]]], main=treatments[2]) 

plot(density(mi[[treatments[1]]]))
lines(density(mi[[treatments[2]]]), col="red")
