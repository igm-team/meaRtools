## sje testing functions.
require(devtools)
load_all()
require(sjemea)

require(lattice)




options(sjemea.corr.method = "Tiling")
options(sjemea.corr.dt = 0.05)          #default 50ms.

## check network spikes

data.file <- system.file("examples", "TC89_DIV15_A.nexTimestamps",
                              package = "sjemea")
s <- sanger.read.spikes( data.file, beg=400, end=700)
ns1 <- sjemea::compute.ns(s, ns.T=0.003, ns.N=10,sur=100)
ns2 <- .compute_ns(s, ns_t=0.003, ns_n=10,sur=100)
plot(ns1)
plot_network_spikes(ns2) ## Note problem: this should  be just plot(ns2)
colnames(ns1$measures) <- NULL
colnames(ns2$measures) <- NULL
all.equal(ns1, ns2, check.names = FALSE)

## TODO plot_network_spikes is the wrong function name, it should be plot.ns() as this is the
## S3 plot() class for the "ns" type.


## check STTC
t1 <- sjemea::tiling.allpairwise(s)
levelplot(t1)
t2 <- sttc_allspikes1(s$spikes, dt=getOption("sjemea.corr.dt"),
                                           s$rec.time[1], s$rec.time[2])
all.equal(t1, t2)

## NB: I need to check how to easily convert rec.time to rec_time everywhere.

## check correlation index

## NOT YET FIXED!!!  .make.corr.indexes2 will not work!

## check firing rate

r1 = sjemea::make.spikes.to.frate(s$spikes)
r2 = .make_spikes_to_frate(s$spikes)
all.equal(r1,r2)

# see issue about the inequality changing in the above function.


## Check the plot function
data(S)
S1 = list(S=S)
plot_plate_summary_for_spikes(S1, "/tmp")

demas_platelayout = list(n_well = 6,
                        wells = paste0("w", "1:6"),
                        n_well_r = 2,
                        n_well_c = 3,
                        layout = c(3, 2),
                        n_elec_r = 8,
                        n_elec_c = 8,
                        xlim = c(-100, 7200),
                        ylim = c(0, 6000),
                        spacing = 200,
                        corr_breaks = 0
                        )

add_plateinfo("demas-6well", demas_platelayout)
times = system.file("extdata/textreader/demas.times", package="meaRtools")
pos = system.file("extdata/textreader/demas.pos", package="meaRtools")
s = read_spikelist_text(times, pos, array="demas-6well")
.plot_mealayout(s$layout, use_names = TRUE, cex=0.3)

## two problems - w_ch_00 ==> channel name should be ch_00
## position of plots.

.plot_meanfiringrate(s, main = "Mean Firing Rate by Plate (Hz)")
## next: work through next plot_plate_summary_for_spikes
.channel_plot_by_well(s, resp = "meanfiringrate",
                      resp_label = "Mean Firing Rate (Hz)")

## Sahar: are ISIs not added by default?
s <- calculate_isis(s)
.plot_isis_by_plate(s)
.plot_isis_by_electrode(s) ## does nothing?


## what is the s$cw field?  wells that are okay?
s = get_num_ae(s)
.channel_plot_by_well(s, resp = "meanfiringrate",
                      resp_label = "Mean Firing Rate (Hz)")
.plot_mean_firingrate_by_electrode(S)

.plot_mean_firingrate_by_electrode(s)
