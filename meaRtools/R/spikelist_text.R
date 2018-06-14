## Functions for reading in text in a generic format.

read_spikelist_text <- function(spike_text_file, channel_text_file, chem_info) {
  spike_data = read.csv(spike_text_file)

  spikes = split(spike_data$Time, spike_data$Channel)
  
  channel_data = read.csv(channel_text_file)
  
  ## if no well information given, assume NA.
  if (!is.element("Well", names(channel_data))) {
    wells = NA
    print(wells)
  }

  ## what should these all be?
  channels <- names(spikes)
  epos <- NULL
  array <- NULL
  treatment <- NULL
  dose <- NULL
  size <- NULL
  well <- NULL
  units <- NULL

  s = list(spikes=spikes,
           scount=sapply(spikes, length),
           epos=epos,
           names=channels,
           array=array,
           treatment=treatment,
           dose=dose,
           size=size,
           well=well,
           units=units
           )

  s
           
           ##channel_data=channel_data)
}



######################################################################
s = read_spikelist_text("~/proj/2018/maxwell/test1/times.csv",
                        "~/proj/2018/maxwell/test1/pos.csv")


