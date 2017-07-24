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
ns2 <- .compute.ns(s, ns.T=0.003, ns.N=10,sur=100)
plot(ns2)
all.equal(ns1, ns2)



## check STTC
t1 <- sjemea::tiling.allpairwise(s)
levelplot(t1)
t2 <- sttc_allspikes1(s$spikes, dt=getOption("sjemea.corr.dt"),
                                           s$rec.time[1], s$rec.time[2])
all.equal(t1, t2)


## check correlation index

## NOT YET FIXED!!!  .make.corr.indexes2 will not work!

## check firing rate

r1 = sjemea::make.spikes.to.frate(s$spikes)
r2 = .make.spikes.to.frate(s$spikes)
all.equal(r1,r2)

# see issue about the inequality changing in the above function.

