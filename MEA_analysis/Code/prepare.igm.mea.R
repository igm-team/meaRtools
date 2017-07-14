# Note: Tcl/Tk is part of the standard R 3.1.2 installation.
library(tcltk)

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

  
    tk_messageBox("ok", message=paste("Please wait while R packages install" ) )
    
    options(download.file.method="libcurl", url.method="libcurl")
    
    #lib0<-.libPaths()
    lib <- file.path(getwd(),"packages")
    suppressWarnings( dir.create(lib) )
    #.libPaths(c(lib,lib0))
    .libPaths(lib)
    
    cat("The libraries will be installed at ", lib, "\n")
    fileConn<-file(".Rprofile")
    writeLines(paste('.libPaths("', lib, '")',sep = ""), fileConn)
    close(fileConn)
    
    install.packages( "IGM.MEA",
                      repos="http://cran.us.r-project.org", lib= lib, quiet = TRUE)
    
    if (is.element(list.files(lib), "IGM.MEA" ) ){
      if (is.element( file.path(lib,"IGM.MEA"),"DESCRIPTION") ){
        cat("prepare.igm.mea message-> Installed IGM.MEA.", "\n")
      } else {
        cat("prepare.igm.mea message-> IGM.MEA Error", "\n")
      }
    } else {
      cat("prepare.igm.mea message-> IGM.MEA Error", "\n")
      
    }
    
    
    source("https://bioconductor.org/biocLite.R")
    
    
    cat("prepare.igm.mea message-> sourced biocLite.", "\n")
    
    cat (paste("user lib is:", lib,sep=""), "\n")
    cat(paste0("lib ", lib, "\n"))
    biocLite("tkWidgets", ask=F, lib= lib, lib.loc=lib )
    
    
    cat("prepare.igm.mea message-> Installed tkWidgets.", "\n")
    
    a<-installed.packages()
    if (is.element( "IGM.MEA", a[,1] ) ){
      version<- packageVersion("IGM.MEA")
      tk_messageBox("ok", message=paste("you're using IGM.MEA v",version ) )
    } else {
      tk_messageBox("error", message="IGM.MEA failed to install properly")
    }
    
  
    
    
    cat("prepare.igm.mea message-> Installation complete.", "\n")
    
  
    
  
