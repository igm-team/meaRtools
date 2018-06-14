require(devtools)
load_all("meaRtools")

## simulation of 6-well MEA, from some old P9/P11 recordings.
times = system.file("extdata/textreader/demas.times", package="meaRtools")
pos = system.file("extdata/textreader/demas.pos", package="meaRtools")

s = read_spikelist_text(times, pos)

meaRtools:::.plot_spike_list(s)
## Sahar: following doesn't yet work
##meaRtools:::.plot_mealayout(s$layout, use_names = T, cex = 1)

plot(s$layout$pos[,1], s$layout$pos[,2], pch=20, cex=.1, asp=1)
text(s$layout$pos[,1], s$layout$pos[,2], rownames(s$layout$pos))


##  breaks = seq(from=0, to=1000, by=50)
##  s$corr = corr.index(s, distance.breaks = breaks)
##  plot.corr.index(s, log="y", las=1, ylim=c(0.1, 100),
##                xlabel = ci.xlabel, pch=20, show.fit=FALSE)

######################################################################

