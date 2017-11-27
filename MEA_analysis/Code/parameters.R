# executed : 2017-11-27-06_45_55
analysis<-list();
spk_list_files<-c();
analysis$spk_list_files[1]<-'/Users/dh2744/Dropbox/Columbia/Software/github/meaRtools/test/exampleRecording/exampleRecording_1012016_plate1_DIV3_spike_list.csv';
spk_list_files[1]<-'/Users/dh2744/Dropbox/Columbia/Software/github/meaRtools/test/exampleRecording/exampleRecording_1012016_plate1_DIV3_spike_list.csv';

experimental_log_file<-'/Users/dh2744/Dropbox/Columbia/Software/github/meaRtools/test/exampleRecording/exampleRecording_1012016_plate1_expLog.csv';

analysis$Routput_dir<-'Analysis//R_objects' 
analysis$output_dir<-'Analysis'

parameters<- list( 
 # spikes
spike_csv=TRUE,
spike_plot=TRUE,


# Burst 
burst_csv=TRUE,
burst_plot=TRUE,


# network spikes 
ns_csv=TRUE,
ns_plot=TRUE,

burst_type="ps" ,
s_min=5
,
perm_n =100,

elec_min_rate=0.016,
elec_max_rate=1000,
well_min_rate=4,
well_max_div_inactive_ratio=0.5,

mi_par=list(beg_isi =0.1,
end_isi =0.25,
min_ibi =0.3,
min_durn =0.05,
min_spikes = 5),


ns_t=0.01,
ns_n=3,
sur=100,

# Distributions IBI
burst_distribution_ibi=list(
perform = 1,
min_cases = 15,
x_axis_lim = 20,
bins_in_sec = 5,
min_values = 0,
filter_by_min = 0,
per_well = 0), 

# burst duration distribution parameters
burst_distribution_durn=list(
perform = 1,
min_cases = 15,
x_axis_lim = 18,
bins_in_sec = 5,
min_values = 0,
filter_by_min = 0,
per_well = 0), 

# burst distribution ISI parameters
burst_distribution_isi=list(
perform = 1,
min_cases = 15,
x_axis_lim = 0.5,
bins_in_sec = 100,
min_values = 0,
filter_by_min = 0,
per_well = 0), 

# burst distribution nspikes parameters
burst_distribution_nspikes=list(
perform = 1,
min_cases = 5,
x_axis_lim = 200,
bins_in_sec = 1,
min_values = 0,
filter_by_min = 0,
per_well = 0) ,

# burst distribution spikeFreq parameters
burst_distribution_spike_freq=list(
perform = 1,
min_cases = 15,
x_axis_lim = 300,
bins_in_sec = 1,
min_values = 0,
filter_by_min = 0,
per_well = 0), 


want_nb=TRUE,

local_region_min_nae= 0,
min_electrodes = 5,
sigma = c(10,20,50)

,timeStamp='2017-11-27-06_45_55'
)

