######################################################################
s = read_spikelist_text("~/proj/2018/maxwell/test1/times.csv",
                        "~/proj/2018/maxwell/test1/pos.csv")


source("~/langs/R/sjemea/R/corr_index.R")



s = read_spikelist_text("~/proj/2018/maxwell/test1/wong1993_p0.times",
                        "~/proj/2018/maxwell/test1/wong1993_p0.pos")

meaRtools:::.plot_spike_list(s)
meaRtools:::.plot_mealayout(s$layout, use_names = T, cex = 1)



breaks = seq(from=0, to=1000, by=50)
s$corr = corr.index(s, distance.breaks = breaks)
plot.corr.index(s, log="y", las=1, ylim=c(0.1, 100),
                xlabel = ci.xlabel,
                pch=20, show.fit=FALSE)








######################################################################

## An example of 500 sec of data.
s = read_spikelist_text("~/proj/2018/maxwell/test1/demas.times",
                        "~/proj/2018/maxwell/test1/demas.pos")

meaRtools:::.plot_spike_list(s)
meaRtools:::.plot_mealayout(s$layout, use_names = T, cex = 1)
