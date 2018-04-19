\name{read_spikelist}
\alias{read_spikelist}
\title{
Axion convert spk_list to r_object
}
\description{
Converts the Axion spk_list file to a Rdata object and initializes it with all spike and plate info
}
\usage{
read_spikelist(key, spk_list_file, chem_info ,r_object_dir) 
}
\arguments{
  \item{key}{
base name of spk_list file
}
  \item{spk_list_file}{
  The full spk_list file name (including path)
}
  \item{chem_info}{
  plate layout information list as loaded using function chem_info_2
}
  \item{r_object_dir}{
Directory of r_object files
}
}
\value{
\item{save_file }{Full path of the saved r_object data file}

}

\seealso{
chem.info.2}
\examples{
\donttest{
temp_path=tempdir()
master_chem_file<-paste0( temp_path,"/data",
   "/exampleRecording_1012016_plate1_expLog.csv" )
      
spike_list_file<-paste0( temp_path,"/data",
"/exampleRecording_1012016_plate1_DIV1_spike_list.csv" )

title<-strsplit(basename(spike_list_file), ".csv")[[1]][1]
# get plate chemical info for each file in the list

plate_chem_info<-chem_info_2( file=spike_list_file, master_chem_file = master_chem_file )

r_object_file_name<-read_spikelist(key=title, 
                  spk_list_file=plate_chem_info, 
                  chem_info=plate_chem_info,r_object_dir="/") 
}
}

\keyword{ spike_list }
\keyword{ r_object }
\keyword{ Axion }
