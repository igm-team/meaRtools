# unit tests for MEA functions
# stopifnot(test_entropy_funcs()==T)
# stopifnot(test_entropy_funcs.mea()==T)

#####################################################################
#
# unit testing functions
#
#####################################################################

.test_entropy_funcs <- function() {
  # general tests for entropy calculations on a perfectly uniform dist
  t_span <- c(0, 1600)
  spikeset_max_entropy <- c(100, 300, 500, 700, 900, 1100, 1300, 1500)
  testbin_starts <- c(0, 200, 550, 900, 1200, 1450)
  testbin_ends <- c(200, 550, 900, 1200, 1450, 1600)
  max_ent_pdist <- .pdist_electrode_spikes(spikeset_max_entropy,
    t_span[1], t_span[2])
  stopifnot(max(max_ent_pdist) == min(max_ent_pdist) &
              max(max_ent_pdist) == 0.125)
  ent <- .entropy(max_ent_pdist)
  stopifnot(ent == 3)
  ent_norm <- .entropy(max_ent_pdist, normalized_uniform = T)
  stopifnot(ent_norm == 1)
  stopifnot(.kl_divergence(max_ent_pdist, max_ent_pdist) == 0)
  stopifnot(.mutual_information(max_ent_pdist, max_ent_pdist) == 0)

  # tests for entropy calcs on a non-uniform distribution
  testbin_pdist <- .pdist_electrode_spikes(spikeset_max_entropy, t_span[1],
    t_span[2], bin_starts = testbin_starts,
    bin_ends = testbin_ends)
  stopifnot(testbin_pdist == c(0.125, 0.250, 0.250, 0.125, 0.125, 0.125))
  testbin_pdist_uniform <- c(1, 1, 1, 1, 1, 1)
  stopifnot(.entropy(testbin_pdist) == 2.5)
  stopifnot(.kl_divergence(testbin_pdist, testbin_pdist_uniform) ==
              0.084962500721156186678)
  stopifnot(.mutual_information(testbin_pdist, testbin_pdist_uniform) ==
              0.01031810090963993301)
  return(T)
}

.test_entropy_funcs_mea <- function(test_elec="F8_43") {
  # create synthetic spikes, burst intervals, t_0, t_end
  test_spikes <- c(2, 3, 4, 7, 8, 9)
  test_burst_starts <- c(2, 7)
  test_burst_ends <- c(4, 9)
  t_0 <- 1
  t_end <- 10
  # test ability to get bin_set vector, bin sizes, p(bins)
  bin_set <- .get_bin_fullset(test_burst_starts,
    test_burst_ends, t_0, t_end)
  stopifnot(bin_set == c(1, 2, 4, 7, 9, 10))
  bin_sizes <- .get_bin_sizes(bin_set)
  stopifnot(bin_sizes == c(1, 2, 3, 2, 1))
  pdist_bins <- .p_bins(bin_sizes)
  stopifnot(pdist_bins == c(1 / 9, 2 / 9, 3 / 9, 2 / 9, 1 / 9))
  # test ability to get pdist of spikes in provided binstart/ends
  pdist_spikes <- .pdist_electrode_spikes(test_spikes, t_0, t_end,
    bin_starts = test_burst_starts,
    bin_ends = test_burst_ends)
  stopifnot(pdist_spikes == c(1 / 6, 2 / 6, 1 / 6, 2 / 6, 0))

  # test functions on burst-only interval subsets
  bin_set_burst <- unique(sort(c(test_burst_starts,
    test_burst_ends)))
  bin_sizes_burst <- .get_bin_sizes(bin_set_burst, pairs_only = T)
  stopifnot(bin_sizes_burst == c(2, 2))
  pdist_bins_burst <- .p_bins(bin_sizes_burst)
  stopifnot(pdist_bins_burst == c(0.5, 0.5))

  return(T)
}
