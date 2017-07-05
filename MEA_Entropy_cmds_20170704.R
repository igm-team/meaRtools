rm(list = ls())
library(meaRtools)

# load MEA data from h5 file (can also be in form 'Rdata')
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

# make boxplots                                                                 
boxplot(norm_mis_per_well[[treatments[1]]], main=treatments[1])                                 
boxplot(norm_mis_per_well[[treatments[2]]], main=treatments[2]) 

plot(density(mi[[treatments[1]]]))
lines(density(mi[[treatments[2]]]), col="red")
