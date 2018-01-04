# Note: Tcl/Tk is part of the standard R 3.1.2 installation.
library(tcltk)

tk_messageBox("ok", message=paste("Please wait while R packages install" ) )



if (!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)==0) {
    
    stop("At least one argument must be supplied (input file).\n", call.=FALSE)
  } else {
    cat(args[1])
    cat("\n")
    # default output file
    lib=args[1] 
  }
  
} else{

  lib <- file.path(dirname(getwd()),"MEA_analysis/packages")

}
dir.create(lib)

#check for java installation
#check for java installation
java.r<-system2("java", args="-version", stdout = T, stderr=T )
if ( !substring(java.r[1],1,12)=="java version" ){
  tk_messageBox(message="Your java may be out of date")
} else {
  ver.n=unlist(strsplit(java.r[1], split='"'))
  if ( length(ver.n)>1 ){
    num=as.numeric( substring( ver.n[2], 1, 3) )
    if (! num>=1.8){
      tk_messageBox(message="Please update your java to version 1.8 or above. https://java.com")
    
    }
  }
}

  
    
    
    options(download.file.method="libcurl", url.method="libcurl")
    
        
    #.libPaths(c(lib,lib0))
    .libPaths(lib)
    
    cat("The libraries will be installed at ", lib, "\n")
    fileConn<-file(".Rprofile")
    writeLines(paste('.libPaths("', lib, '")',sep = ""), fileConn)
    close(fileConn)
    
    install.packages( "meaRtools",
                      repos="http://cran.us.r-project.org", lib= lib, quiet = TRUE)
    
    if (is.element(list.files(lib), "meaRtools" ) ){
      if ( any( is.element( list.files( file.path(lib,"meaRtools") ),"DESCRIPTION") ) ){
        cat("prepare.igm.mea message-> Installed meaRtools.", "\n")
      } else {
        cat("prepare.igm.mea message-> meaRtools Error", "\n")
      }
    } else {
      cat("prepare.igm.mea message-> meaRtools Error", "\n")
      
    }
    
    
    source("https://bioconductor.org/biocLite.R")
    
    
    cat("prepare.igm.mea message-> sourced biocLite.", "\n")
    
    cat (paste("user lib is:", lib,sep=""), "\n")
    cat(paste0("lib ", lib, "\n"))
    biocLite("tkWidgets", ask=F, lib= lib, lib.loc=lib )
    
    
    cat("prepare.igm.mea message-> Installed tkWidgets.", "\n")
    
    a<-installed.packages()
    if (is.element( "meaRtools", a[,1] ) ){
      version<- packageVersion("meaRtools")
      tk_messageBox("ok", message=paste("you're using meaRtools v",version ) )
    } else {
      tk_messageBox("error", message="meaRtools failed to install properly")
    }
    
  
    
    
    cat("prepare.igm.mea message-> Installation complete.", "\n")
    
  



