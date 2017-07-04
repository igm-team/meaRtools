rm(list = ls())
library(meaRtools)

# load MEA data from h5 file (can also be in form 'Rdata')
data(S)
mea <- S

# PARAMETERS
treatmentA <- "treatX"
treatmentB <- "treatY"

## collect entropy values, seperate output list by well classifs
mea <- filter.nonactive.spikes(mea,spikes.per.minute.min=1)
norm.ents.per.well <- entropies.per.well(mea,diff=diff)
norm.ents.per.well <- filter.list(norm.ents.per.well,mult.factor=1.5)

data.dists <- list( "ENT"=list(), "MI"=list())

## get wells classified as 'WT' and  subset well data on well classification
WT.wells <- names(mea$treatment[mea$treatment==treatmentA])
WT.wells <- WT.wells[WT.wells %in% names(norm.ents.per.well)]
norm.ents.per.well.WT <- wells.subset(norm.ents.per.well,WT.wells)
## get mean entropy per well set for WT
norm.ent.means.per.well.WT <- lapply(norm.ents.per.well.WT,function(x) {mean(x)})
norm.ent.sds.per.well.WT <- lapply(norm.ents.per.well.WT,function(x) {sd(x)})
norm.ents.WT <- list.to.vals(norm.ents.per.well.WT)
## calculate MI values
norm.MIs.per.well.WT <- pairwise.dists.per.well(mea,wellnames=WT.wells,
                                                dist.metric="mutual.information",
                                                bin.size=0.1)
norm.MI.means.per.well.WT <- lapply(norm.MIs.per.well.WT,function(x) {mean(x)})
# store summary stats to list
data.dists[["ENT"]][[treatmentA]] <- list.to.vals(norm.ent.means.per.well.WT)
data.dists[["MI"]][[treatmentA]] <- list.to.vals(norm.MI.means.per.well.WT)

## get wells classified as 'MUT' and  subset well data on well classification
MUT.wells <- names(mea$treatment[mea$treatment==treatmentB])
MUT.wells <- MUT.wells[MUT.wells %in% names(norm.ents.per.well)]
norm.ents.per.well.MUT <- wells.subset(norm.ents.per.well,MUT.wells)
## get mean entropy per well set for MUT
norm.ent.means.per.well.MUT <- lapply(norm.ents.per.well.MUT,function(x) {mean(x)})
norm.ent.sds.per.well.MUT <- lapply(norm.ents.per.well.MUT,function(x) {sd(x)})
norm.ents.MUT <- list.to.vals(norm.ents.per.well.MUT)
norm.MIs.per.well.MUT <- pairwise.dists.per.well(mea,
                                                 dist.metric="mutual.information",
                                                 wellnames=MUT.wells,
                                                 bin.size=0.1)
norm.MI.means.per.well.MUT <- lapply(norm.MIs.per.well.MUT,function(x) {mean(x)})
data.dists[["ENT"]][[treatmentB]] <- list.to.vals(norm.ent.means.per.well.MUT)
data.dists[["MI"]][[treatmentB]] <- list.to.vals(norm.MI.means.per.well.MUT)

# test for difference in mean entropy between treatmentA, treatmentB
ent <- data.dists[["ENT"]]
ent.WT <- mean(ent[[treatmentA]])
ent.MUT <- mean(ent[[treatmentB]])
ent.res <- wilcox.test(ent[[treatmentA]], ent[[treatmentB]])
cat("entropy means (WT / MUT) :", ent.WT, "/", ent.MUT, "\n")
print(ent.res)

# test for diff in mutual info btwn treatmentA, treatmentB
mi <- data.dists[["MI"]]
mi.WT <- mean(mi[[treatmentA]])
mi.MUT <- mean(mi[[treatmentB]])
mi.res <- wilcox.test(mi[[treatmentA]], mi[[treatmentB]])
cat("mutual info means (WT / MUT) :", mi.WT, "/", mi.MUT, "\n") 
print(mi.res)

# make boxplots                                                                 
boxplot(norm.MIs.per.well.WT, main=treatmentA)                                 
boxplot(norm.MIs.per.well.MUT, main=treatmentB) 

plot(density(mi[[treatmentA]]))
lines(density(mi[[treatmentB]]), col="red")
