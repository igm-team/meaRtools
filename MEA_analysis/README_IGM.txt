GENERAL REMARKS FOR IGM_MEA analysis tool Version 0.3.5

1- The analysis tool is a java user interface (UI) that interacts with an R package. The tool installs the latest meaRtools R package from the public CRAN server (currently meaRtools version 1.0.1). The tool allows changing parameters for the different analyses and integrates tools for downstream analysis such as raster plots and permutation analyses. The actual analysis pipeline is performed by the R package and runs automatically through the main tab of the tool.

2- Mac and windows compatibility - the tool and R package fit both environments and should be installed the same. Both users would use the same IGM_MEA_Analysis.jar file to run the tool.

3- The new network burst features summarize synchronized network bursting activity of all electrodes within each well for 3 different ms window sizes that can be set/changed within the ’network events’ tab of the UI.

4- Burst features distributions and genotype/treatment data comparisons : the pipeline performs automatic burst distribution calculations and plotting based on sets of parameters that are in the different tabs of the tool’s UI. Available features include: Inter Burst Interval (IBI), Inter Spike Interval withing bursts (ISI), Duration of bursts, Mean frequency of spikes withing bursts (spike density) and Number of spikes in bursts.
Output is :
  a. The regular burst_plot.pdfs in the ‘outputPerDIV’ folder for each DIV now holds a distribution comparison and ks-test result for all treatments in the experiment.
  b. A csv file for each feature in the ‘outputPerDIV/distributionFiles’ folder that holds the distribution of each electrode in all loaded DIVs. This file can be used for plotting or as input to the new permutation tool (next paragraph).

5- Well filtering. Currently the pipeline filters out wells that are inactive through the experiment. The algorithm decides an active well as having at least 1/4 of the electrodes active. Then it filters out from the analysis wells that are not active in at least 30% of the analysis DIVs. Future algorithm should filter active wells in a much stricter way requiring at least 70% active DIVs, that should be implemented by filtering wells based on the whole experiment (all recorded DIVs) and use the wells that pass this filter for any other analysis of that experiment.

6- Output is printed into an ’/Analysis’ folder under the same directory as input files. Output folder consists of several sub-folders:

	logFiles     - holds messages of the pipeline for each pipeline run

	R_Objects    - data objects of each DIV analyzed in format ready for loading using R statistical computing program. Used also for plotting raster plots.

	outputPerDIV - basic .csv and .pdf output of all features for each DIV  

	outputPerDIV/distributionFiles - distribution files of burst distributions to be loaded to ‘distribution permutation’ tool available in the ‘tools’ tab of the UI. This directory will be available only in CRAN version 0.3.2. Current version prints this output to the parent folder (outputPerDIV).

	spikes - .csv files of each of the spike features, each holding data for all analyzed DIVs. PDF is also printed with graphs to display all comparisons between reference genotype/treatment and all other treatments. 1000 permutations are performed for each comparison and p.value is plotted.  

	bursts - same as spikes but for bursting features.

	ns     - same as spikes but for network spike features.

	nb     - same as spikes but for network bursts features.


ADVANCED TOOLS added to the UI tool:

1- Distributions permutation tool - this tool expects a '..._distributions.csv' file from each experiment (spanning all tested DIVs, automatically produced). It asks for 2 treatments/genotypes to compare, then performs EMD (Earth Movers Distance) and maximum distance calculations between the two distributions. Results are permutated at a selected number of permutations and plotted for each feature in a '..._CDF_EMD.pdf' (Cumulative Distribution Function and Earth Movers test results).

2- Raster plot printing - this tool gives the users the ability to print raster plots with network spikes and bursting info. The tool asks the user for an R object that is produced automatically under Analysis/R_Objects when a pipeline is run for an experiment. Using the raster plot function is easy: just choose the relevant DIV’s R object file (.RData), choose well and time range to present. Other options allow presentation of identified bursts(horizontal red lines) and network spikes (vertical green lines). 


WHAT’S IN THE ZIP FILE:

The zip opens to an MEA_analysis directory, the directory holds the following: 
1- IGM_MEA_Analysis.jar - the file that executes the IGM MEA analysis tool.
2- lib/ and Code/ directories with R and Java scripts that are used by the java UI.  
3- this README.txt file

				    
How to install:

1- Install R if not installed already. R version 3.2.4 and up is recommended.
2- Install Java if not installed already. Java version 1.8 and up is recommended.
3- extract IGM_MEA_Analysis.zip in a selected location.
4- Run IGM_MEA_Analysis.jar. When the tool runs for the first time it will ask the user for the location of Rscript.exe, usually this file is located either in 
‘/Library/Frameworks/R.framework/Resources’ for mac OS X or ‘c:/program files/R/R-3.2.4/bin/x64/‘ for windows systems. Once you locate Rscript, the installation gets the latest meaRtools R package from CRAN (current is 0.3.1) and installs it with all other dependent packages. Installation output can be found under the Code/ directory in IGMMEA_install_log.txt which you might need to send us to understand why it’s not actually installing :-)
Once installed, a message stating the current installed version will be printed to screen and the tool will load and be ready for analysis.

Good luck !

MEA Team
Institute for Genomic Medicine
Columbia University Medical Center