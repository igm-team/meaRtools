##' run the STTC code for a spike train (Cpp version)
##'
##' Internal computation
##' @title Compute STCC direct in Cpp
##' @param dt bin width for 
##' @param start start time in seconds
##' @param end end time in seconds
##' @param spike_times_1 spike train 1
##' @param spike_times_2 spike train 2
##' @return STTC value
##' @author Stephen Eglen
"run_TMcpp"

##' Compute STTC for all unique pairs of spike trains
##'
##' Return a matrix of all STTC values
##' @title Compute STTC for all pairs of spike trains
##' @param spikes List of spike trains
##' @param dt tiling window
##' @param beg start time
##' @param end end time
##' @return Matrix of STTC values.  Upper diagonal matrix only; diagonal
##' elements should be 1.  
##' @author Stephen Eglen
"sttc_allspikes1"

##' Compute STTC profile for two spike trains
##'
##' Compute the STTC profile for two spike trains using C++.
##' @title Compute STTC profile for two spike trains
##' @param a Spike train 1
##' @param b Spike train 2
##' @param start Start time
##' @param end End time
##' @param dt coincidence window for STTC
##' @param tau_sep step size for tau in [-tau_max, +tau_max]
##' @param tau_max maximum tau value
##' @return obj  An object of type "sttcp", containing the tau values and correlations.
##' @author Stephen Eglen
"sttcp_ab"


##' Estimate the population firing rate, averaging over all spikes.
##'
##' We compute the array-wide average activity for a list of spike
##' trains.  The duration of the recording is given in seconds by BEG
##' and END.  Time is divided up into NBINS bins, each of duration WID.  Each spike
##' is then placed in the appropriate bin and then we return the average count in each bin.
##' 
##' 
##' @title Estimate population firing rate using fixed-width time bins.
##' @param spikes List of simultaneously recorded spike trains
##' @param beg Start of the recording, in seconds.
##' @param end The start time of  the last bin, in seconds.
##' @param wid The duration of each bin
##' @param nbins The number of bins to generate.  
##' @return The population firing rate (in Hz) for each bin.
##' @author Stephen Eglen
"frate_counts"


##' Compute all STTPs for a set of spike trains
##'
##' .. content for \details{} ..
##' @title Compute all STTPs for a set of spike trains
##' @param spikes Concatenated list of spike trains
##' @param n number of spike trains
##' @param nspikes Vector containing the number of spikes in each train
##' @param first_spike Index to the first spike in each train.
##' @param start Start time of recording in seconds
##' @param end End time of recording in seconds
##' @param dt Coincidence window for STTC
##' @param tau_sep Step size for taus.
##' @param tau_max Maximum absolute tau value.
##' @return Pairwise STTPs for all spike trains
##' @author Tom Edinburgh
"tiling_correlogramcpp"

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Compute STTPs for just two spike trains, A and B
##' @param spikes Concatenated list of spike trains
##' @param n number of spike trains
##' @param nspikes Vector containing the number of spikes in each train
##' @param first_spike Index to the first spike in each train.
##' @param start Start time of recording in seconds
##' @param end End time of recording in seconds
##' @param dt Coincidence window for STTC
##' @param tau_sep Step size for taus.
##' @param tau_max Maximum absolute tau value.
##' @param a Number of first spike train
##' @param b Number of second spike train
##' @return STTP for spike trains a and b
##' @author Tom Edinburgh
"tiling_correlogramcpp_index"

