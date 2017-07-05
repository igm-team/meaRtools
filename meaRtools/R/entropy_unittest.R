# unit tests for MEA functions
# stopifnot(test.Entropy.funcs()==T)
# stopifnot(test.Entropy.funcs.MEA()==T)

##################################################################### 
# 
# unit testing functions
# 
##################################################################### 

.test.Entropy.funcs <- function() {
  # general tests for entropy calculations on a perfectly uniform dist
  t.span = c(0, 1600)
  spikeset.max.entropy <- c(100, 300, 500, 700, 900, 1100, 1300, 1500)
  testbin.starts <- c(0, 200, 550, 900, 1200, 1450)
  testbin.ends <- c(200, 550, 900, 1200, 1450, 1600)
  max.ent.pdist <- .pdist.electrode.spikes(spikeset.max.entropy,
    t.span[1], t.span[2])
  stopifnot(max(max.ent.pdist) == min(max.ent.pdist) & max(max.ent.pdist) == 0.125)
  ent <- .entropy(max.ent.pdist)
  stopifnot(ent == 3)
  ent.norm <- .entropy(max.ent.pdist, normalized.uniform = T)
  stopifnot(ent.norm == 1)
  stopifnot(.KL.divergence(max.ent.pdist, max.ent.pdist) == 0)
  stopifnot(.mutual.information(max.ent.pdist, max.ent.pdist) == 0)

  # tests for entropy calcs on a non-uniform distribution
  testbin.pdist <- .pdist.electrode.spikes(spikeset.max.entropy, t.span[1],
    t.span[2], bin.starts = testbin.starts,
    bin.ends = testbin.ends)
  stopifnot(testbin.pdist == c(0.125, 0.250, 0.250, 0.125, 0.125, 0.125))
  testbin.pdist.uniform <- c(1, 1, 1, 1, 1, 1)
  stopifnot(.entropy(testbin.pdist) == 2.5)
  stopifnot(.KL.divergence(testbin.pdist, testbin.pdist.uniform) == 0.084962500721156186678)
  stopifnot(.mutual.information(testbin.pdist, testbin.pdist.uniform) == 0.01031810090963993301)
  return(T)
}

.test.Entropy.funcs.MEA <- function(test.elec="F8_43") {
  # create synthetic spikes, burst intervals, t.0, t.end
  test.spikes <- c(2, 3, 4, 7, 8, 9)
  test.burst.starts <- c(2, 7)
  test.burst.ends <- c(4, 9)
  t.0 <- 1
  t.end <- 10
  # test ability to get bin.set vector, bin sizes, p(bins)
  bin.set <- .get.bin.fullset(test.burst.starts,
    test.burst.ends, t.0, t.end)
  stopifnot(bin.set == c(1, 2, 4, 7, 9, 10))
  bin.sizes <- .get.bin.sizes(bin.set)
  stopifnot(bin.sizes == c(1, 2, 3, 2, 1))
  pdist.bins <- .p.bins(bin.sizes)
  stopifnot(pdist.bins == c(1 / 9, 2 / 9, 3 / 9, 2 / 9, 1 / 9))
  # test ability to get pdist of spikes in provided binstart/ends
  pdist.spikes <- .pdist.electrode.spikes(test.spikes, t.0, t.end,
    bin.starts = test.burst.starts,
    bin.ends = test.burst.ends)
  stopifnot(pdist.spikes == c(1 / 6, 2 / 6, 1 / 6, 2 / 6, 0))

  # test functions on burst-only interval subsets
  bin.set.burst <- unique(sort(c(test.burst.starts,
    test.burst.ends)))
  bin.sizes.burst <- .get.bin.sizes(bin.set.burst, pairs.only = T)
  stopifnot(bin.sizes.burst == c(2, 2))
  pdist.bins.burst <- .p.bins(bin.sizes.burst)
  stopifnot(pdist.bins.burst == c(0.5, 0.5))

  return(T)
}
