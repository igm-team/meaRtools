\name{read.spikelist}
\alias{read.spikelist}
\title{
Axion convert spkList to Robject
}
\description{
Converts the Axion spikList file to a Rdata object and initializes it with all spike and plate info
}
\usage{
read.spikelist(key, spkListFile, chem.info ,Robject.dir) 
}
\arguments{
  \item{key}{
base name of spkList file
}
  \item{spkListFile}{
  The full spkList file name (including path)
}
  \item{chem.info}{
  plate layout information list as loaded using function chem.info.2 
}
  \item{Robject.dir}{
Directory of robject files
}
}
\value{
\item{save.file }{Full path of the saved Robject data file}

}

\seealso{
chem.info.2}
\examples{

##masterChemFile<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_expLog.csv" )
      
##spike.list.file<-paste0( system.file(package = "meaRtools"),
#"/data",
#"/exampleRecording_1012016_plate1_DIV1_spike_list.csv" )

##title<-strsplit(basename(spike.list.file), ".csv")[[1]][1]
# get plate chemical info for each file in the list

##plate.chem.info<-chem.info.2( file=spike.list.file, masterChemFile = masterChemFile )

##RobjectFileName<-read.spikelist(key=title, 
#                                                  spkListFile=plate.chem.info, 
#                                                  chem.info=plate.chem.info,Robject.dir="/") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spike-list }
\keyword{ Robject }
\keyword{ Axion }
