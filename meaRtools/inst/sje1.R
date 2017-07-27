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

