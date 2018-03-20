# check for distribution file for info
# DIana Hall
# 1/30/2017 

normFileInfo<-function(normFilePath="no path sent"){
  # normFilePath = "/Users/dh2744/Dropbox/Columbia/Software/normalization_Chris/test_folder_normalization/Grin2a_20170720_501423A_mean_dur.csv"
  cat("\n")
  cat(normFilePath)
  cat("\n")
  # check csv
  file.extension=unlist(strsplit(normFilePath, split=".", fixed=T))[2]
  if( !is.element(file.extension, c("csv","csv ")) ){
    cat("file is not a csv")
    cat("\n")
    cat("Need new file.")
    cat("\n")
    return
  } 
  
  t<-read.csv(normFilePath, stringsAsFactors = F)
  
  #get treatment names
  trts<-unique( t[,2])
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
  
  
  #get DIVs
  divs.temp<-colnames( t )
  divs.temp2<-sapply( tolower( divs.temp ) , strsplit, split="div", fixed=T )
  divs.ind<-which( unlist( lapply(divs.temp2, length) )>1  )
  if (length(divs.ind)>0){
    divs<-divs.temp[divs.ind]
    divs.num<-as.numeric( unlist( lapply(divs.temp2[divs.ind], "[",2)) )
    
    for (i in 1: length(divs.num)){
      cat("DIV=", divs.num[i]) 
      cat("\n")
    }
    cat("*") 
    cat("\n")
    
    
    
  } else if ( !length(divs.num)>0 ){
    cat("No DIV in column header in files")
    cat("\n")
    cat("Need new file.")
    cat("\n")
    return
  } 
    
    
  

  
  
}

# normFilePath = "/Users/dh2744/Dropbox/Columbia/Software/normalization_Chris/test_folder_normalization/Grin2a_20170720_501423A_mean_dur.csv"
args <- commandArgs(trailingOnly = FALSE)
normFilePath=strsplit(args[6], "=")[[1]][2]

#cat( "args[6]= "); cat(args[6]); cat("\n")

normFileInfo(normFilePath=normFilePath)
cat("\n")


