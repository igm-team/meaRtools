# check_IGM_MEA_version.R
# to run a quick check on where R is stored.
# Diana Hall
getVersion<-function( javaPath=NULL ){
  
  file.want=paste0(javaPath,"/.Rprofile") 
  if ( file.exists( file.want ) ) {
    A <- readLines(con <- file(file.want) ) 
    close(con)
    temp=unlist(strsplit(A, split=.Platform$file.sep, fixed=T ))
    sub=sapply( temp, substring, 1,8)
    if ( any(sub=="packages") ){
      lib=paste(.Platform$file.sep, paste( temp[2:(length(temp)-1 ) ], collapse=.Platform$file.sep ),
                .Platform$file.sep,"packages", sep="" )
    } else{
      lib=.libPaths()
    }
    
    
  } else {
    lib=.libPaths()
  }
    
    inst.pk=installed.packages(lib=lib )
    if ( any("IGM.MEA"==inst.pk[,1]) ){
      ind=which( "IGM.MEA"==inst.pk[,1] )
      v.string=paste("Using IGM.MEA v.", inst.pk[ind,"Version"], 
                     "stored in libPath: ", lib )
    } else {
      v.string= paste("IGM.MEA has NOT installed properly in lib", lib )
    }
    
    
    v.string
}


loadSessionInfo<-function(){
  options(warn=-1)
  suppressWarnings(suppressMessages(library(ggplot2)))
  suppressWarnings(suppressMessages(library(plyr)))
  suppressWarnings(suppressMessages(library(IGM.MEA)))
  session<-sessionInfo() 
  pk<-cbind.data.frame( version=c( sapply(session$otherPkgs, function(x) x$Version),
     sapply(session$loadedOnly, function(x) x$Version)),
     package= c(names(session$otherPkgs), names(session$loadedOnly) ) )
  pk
}

args <- commandArgs(trailingOnly = FALSE)
javaPath=dirname( dirname(strsplit(args[4], "=")[[1]][2]) )


cat( getVersion( javaPath=javaPath ) )


