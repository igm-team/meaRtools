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


######################################################################


s$well

## ideally for: ~/langs/R/meaRtools/meaRtools/R/sttc.R





sttc_file = tempfile(fileext=".csv")
write.csv(sttc_results, file=sttc_file, row.names=FALSE)

## to view the file:
file.edit(sttc_file)
