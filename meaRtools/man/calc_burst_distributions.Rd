\name{calc_burst_distributions}
\alias{calc_burst_distributions}
\title{ calculate and plot burst featues distributions}
\description{
The function calculates normalized distributions of selected bursting features and plots distribution graphs of all treatments in a recording. The function also prints csv output in the /Analysis directory for downstream stats such as permutation test of treatment labels}
\usage{
calc_burst_distributions(s, min_vals = 1, xlimit = 25, bins_in_sec = 5, 
feature = "non", filter_values_by_min = 0, min_values = 0, per_well = 0, 
outputdir = getwd(), min_electrodes = 4, time_stamp = "DATE_TIME")
}
\arguments{
  \item{s}{
MEA data structure
}
  \item{min_vals}{
minimum values number per electrode, electrodes with a smaller number of values than that are discarded}
  \item{xlimit}{
max limit of values, for example: xlimit = 25 for IBI analysis means that IBIs longer than 25 seconds will not be part of distribution calculations}
  \item{bins_in_sec}{
how many bins to cut each of the segments. For example: IBI analysis has 25 seconds as xlimit, to analyse in a 0.1 sec resolution bins_in_sec should be set to 10, for 1 sec resolution set bins_in_sec to 1
}
  \item{feature}{
what feature to analyze, options are "IBI", "ISI, "nspikesInBurst", "duration", "spikesDensityInBurst"}
  \item{filter_values_by_min}{
should analysis disregard values with lower then filter_values_by_min number of values ? (0/1, default is 0). For example, if set to 1 for duration analysis, should analysis consider also bursts shorter than filter_values_by_min ?}
  \item{min_values}{
disregards values with lower then filter_values_by_min , only if filter_values_by_min set to 1 }
  \item{per_well}{
should distribution analysis be performed by testing treatment differences on well level means (1) or electrode level means(0) }
  \item{outputdir}{
output directory}
  \item{min_electrodes}{
minimum electrodes for an active well}
  \item{time_stamp}{
time stamp for the output files}
}
\details{
Plot distributions calculates normalized distributions of bursting features. 'Normalized distribution' are a way to handle biases caused by noisy electrodes/wells. The function will calculate a normalized histogram (values 0-1) of each feature for each electrode. Next, it will average histogram values either per well and then average all wells per treatment, or directly per treatment. All comparisons between treatments will be then made by plotting the normalized histograms of each treatment and running a K-S test between them. }
\author{
Sahar Gelfman
}
\note{
  Output is a made of: 1) Plots of all selected burst features distributions.
  2) CSV files ending with _distributions.csv that harbor all electrodes per treatment for all the recordings loaded in the meaRtools pipeline for a specific MEA plate}


\examples{
# Load exapmple of recording Robject (MEA data structure)
data("S")  
feature="IBI"; 
#calc_burst_distributions(S, min_vals = 15, xlimit = 20, bins_in_sec = 5, 
#feature = feature, per_well = 0, outputdir = "/Analysis")}

\keyword{ distributions }
\keyword{ IBI }
\keyword{ frequency }
\keyword{ burst }
