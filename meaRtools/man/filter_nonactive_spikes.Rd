\name{filter_nonactive_spikes}
\alias{filter_nonactive_spikes}
\title{
Filter nonactive spikes from recordings
}
\description{
Given an input MEA recording, this function removes spike trains that are deemed as nonactive based on the number of spikes per minutes.
}
\usage{
filter_nonactive_spikes(mea, spikes_per_minute_min = 1)
}
\arguments{
  \item{mea}{
  The input MEA spikelist object.
}
  \item{spikes_per_minute_min}{
  Minimum number of spikes per minute for a spike train to be considered as active.
}
}

\value{
  An MEA spikelist object with nonactive spike trains removed.
}
