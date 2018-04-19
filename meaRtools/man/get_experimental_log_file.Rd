\name{get_experimental_log_file}
\alias{get_experimental_log_file}
\title{
get_experimental_log_file
}
\description{
Extract data from experimental log file: a csv file with columns for well, treatment, dose, size and units. 
}
\usage{
get_experimental_log_file(file, master_chem_file = master_chem_file)
}
\arguments{
  \item{file}{
 spike-list csv file, one of the possible plate recording file formats available from Axion.   Format: one spike and corresponding electrode name per row. See Axion biosystems website for details.
}
  \item{master_chem_file}{
  A csv file containing the following columns:
  "Project", "Experiment.Date", "Plate.SN",  "DIV", "Well",  "Treatment",
  "Size", "Dose", and  "Units".  Empty wells must still be represented in file.  If column is irrelavent to a given data set, then 'NA' or blank is sufficient.  "Project" columm must match the first character string preceeding "_" in spike-list file name. e.g. exampleRecording_1012016_plate1_DIV1_spike_list.csv". Similarly, "Experiment.Data" and "Plate.SN" must match second and third character strings as separated by "_" in spike-list file name. "DIV" column does not need to be matched.
}
}

\value{
list containing character vector of experimental log information.
\item{well }{well name e.g. "A4""}
\item{treatment }{treatment on well e.g. 'WT' }
\item{size }{size information of chemical treatment}
\item{dose }{dose information for treatment}
\item{units }{units of dosage e.g. uL/g }
}
\references{
See http://www.axionbiosystems.com/products/software/ for details on spike-list csv file format
}
\examples{
\donttest{
temp_path=tempdir()
master_chem_file<-paste0( temp_path,"/data",
   "/exampleRecording_1012016_plate1_expLog.csv" )
      
spike_list_file<-paste0( temp_path,"/data",
"/exampleRecording_1012016_plate1_DIV1_spike_list.csv" )

plate.data<-getxperimental.log.file( file=spike.list.file, master_chem_file = master_chem_file )
}
}
\keyword{ experimental log }