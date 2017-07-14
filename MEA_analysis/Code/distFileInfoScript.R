# check for distribution file for info
# DIana Hall
# 1/30/2017 

distFileInfo<-function(distFilePath="no path sent"){
  #distFilePath="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/exampleRecording/Analysis/outputPerDIV/distributionFiles/exampleRecording_1012016_plate1_01-12-17_16_42_24_ISI_distributions.csv"
  cat("\n")
  cat(distFilePath)
  cat("\n")
  # check csv
   length(unlist(strsplit(distFilePath, split="distributions", fixed=T)) )>1 
  file.extension=unlist(strsplit(distFilePath, split=".", fixed=T))[2]
  if( !is.element(file.extension, c("csv","csv ")) ){
    cat("file is not a csv")
    cat("\n")
    cat("Need new file.")
    cat("\n")
    return
  } else if ( (!length(unlist(strsplit(distFilePath, split="distributions", fixed=T)) )>1  )){
    #check for distributions file
    cat("file is not a distributions file")
    cat("\n")
    cat("Need new file.")
    cat("\n")
    return
  }
  
  t<-read.csv(distFilePath, stringsAsFactors = F)
  trts<-unique( t[,1])
  if ( !length(trts)>0 ){
    cat("No trt files")
    cat("\n")
    cat("Need new file.")
    cat("\n")
    return
  } else{
     for (i in 1: length(trts)){
       cat("treatments=", trts[i]) 
       cat("\n")
     }
    cat("*") 
    cat("\n")
    
  }
  
  
  
  
  
  
}
args <- commandArgs(trailingOnly = FALSE)
distFilePath=strsplit(args[6], "=")[[1]][2]
#distFilePath="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/exampleRecording/Analysis/outputPerDIV/distributionFiles/exampleRecording_1012016_plate1_01-12-17_16_42_24_ISI_distributions.csv"
distFileInfo(distFilePath=distFilePath)
cat("\n")


