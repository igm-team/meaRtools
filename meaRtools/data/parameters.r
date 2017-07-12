parameters <- list(

burst_type = "mi",
s_min=5,
  
perm_n = 1000, ##Might want to specify in parameters or give user option?

elec_min_rate = (1/60),
elec_max_rate = 1000,
# minimum nae
well_min_rate=4,
# The maximum DIV inactive/active ratio (below which a well will be considered active
# for a set of DIVs)
well_max_div_inactive_ratio=0.5,

#parameters for max-interval burst detection
mi.par = list(beg.isi =    0.1,
               end.isi =    0.25,
               min.ibi =    0.3,
               min.durn =   0.05,
               min.spikes = 5),

## Parameters for network.spikes
ns_t = 0.01,    	#time in seconds
ns_n = 3,        #how many coincident electrodes?
sur=100, # num. ms before and after spike to check I think, used in ms

#sahar -added for ver 2.0
# Burst parameters for distribution analysis

# Parameters for inter burst interval distribution analysis
burst_distribution_IBI = list(
  perform = 1,        # 0/1 - run this analysis ?
  min_cases = 900/60, # minimum number of bursts, below which electrode will be ingnored
  # 900 is length of recording divided by 60 sec
  x_axis_lim =         20,  # x max limit for distribution plot
  bins_in_sec =  5,   # how many values to be calculated in each segment of xLim 
  # (overall values will be bins_in_sec * xlim)
  min_values = 0,     # bursts with values below this threshold be ignored with filter_by_min=1
  filter_by_min = 0,  # 0/1 ignore bursts with values below min_values
  per_well = 0),       # 0/1 - perform analysis per well=1 or per electrode=0

# Parameters for burst duration distribution analysis
burst_distribution_durn = list(
  perform = 1,        # 0/1 - run this analysis ?
  min_cases = 900/60, # minimum number of bursts, below which electrode will be ingnored
  # 900 is length of recording divided by 60 sec
  x_axis_lim =         18,  # x max limit for distribution plot
  bins_in_sec =  10,   # how many values to be calculated in each segment of xLim 
  # (overall values will be bins_in_sec * xlim)
  min_values = 0,     # bursts with values below this threshold be ignored with filter_by_min=1
  filter_by_min = 0,  # 0/1 ignore bursts with values below min_values
  per_well = 0),      # 0/1 - perform analysis per well=1 or per electrode=0

# Parameters for inter spike interval within burst distribution analysis
burst_distribution_ISI = list(
  perform = 1,        # 0/1 - run this analysis ?
  min_cases = 900/60, # minimum number of bursts, below which electrode will be ingnored
  x_axis_lim =         0.5,  # x max limit for distribution plot
  bins_in_sec =  100,   # how many values to be calculated in each segment of xLim 
  # (overall values will be bins_in_sec * xlim)
  min_values = 0,     # bursts with values below this threshold be ignored with filter_by_min=1
  filter_by_min = 0,  # 0/1 ignore bursts with values below min_values
  per_well = 0),      # 0/1 - perform analysis per well=1 or per electrode=0

# Parameters for number of spikes in burst distribution analysis
burst_distribution_nspikes = list(
  perform = 1,        # 0/1 - run this analysis ?
  min_cases = 5, # minimum number of bursts, below which electrode will be ingnored
  x_axis_lim =         200,  # x max limit for distribution plot
  bins_in_sec =  1,   # how many values to be calculated in each segment of xLim 
  # (overall values will be bins_in_sec * xlim)
  min_values = 0,     # bursts with values below this threshold be ignored with filter_by_min=1
  filter_by_min = 0,  # 0/1 ignore bursts with values below min_values
  per_well = 0),      # 0/1 - perform analysis per well=1 or per electrode=0

# Parameters for average spike frequency in burst distribution analysis
burst_distribution_spike_freq = list(
  perform = 1,        # 0/1 - run this analysis ?
  min_cases = 900/60, # minimum number of bursts, below which electrode will be ingnored
  x_axis_lim =         300,  # x max limit for distribution plot
  bins_in_sec =  1,   # how many values to be calculated in each segment of xLim 
  # (overall values will be bins_in_sec * xlim)
  min_values = 0,     # bursts with values below this threshold be ignored with filter_by_min=1
  filter_by_min = 0,  # 0/1 ignore bursts with values below min_values
  per_well = 0),     # 0/1 - perform analysis per well=1 or per electrode=0

  #network burst parameters
  local_region_min_nae= 0, #do not change for now
  min_electrodes= 4 ,
  Sigma= c(10,20,50) , # a list of window size to be considered 

  time_stamp="DATE_TIME" # system time 

) # end of parameters list


