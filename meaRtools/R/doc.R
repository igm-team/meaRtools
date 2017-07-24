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
