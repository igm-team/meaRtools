# meaRtools
meaRtools provides a platform for the analysis of neuronal networks recorded on Multi-Electrode Arrays (MEAs). meaRtools provides tools to extract neuronal features such as spikes, bursts and network spikes, as well as algorithms to assess neuronal network synchronization and bursts variability originating from mixed-cell neuronal cultures. The package also provides a statistical platform customized for the analysis of MEA data that can combine multiple MEA recordings and compare extracted features between different genetic models or treatments. The package accepts electrode spike trains as input and performs feature extraction, statistical comparisons and visualization, providing graphical comparisons between all treatments. The package is freely available under the GPL license (GPL≥3) and is updated frequently on the CRAN repository. The package, along with full documentation can be downloaded from: https://redmine.igm.cumc.columbia.edu/projects/mea/wiki/MEA_Analysis_guide.


## Installation 

Although versions of this package are available from CRAN, if you wish
to use the current master from github then you can do:

```
devtools::install_github(repo='meaRtools', username='igm-team', subdir="meaRtools")
```
