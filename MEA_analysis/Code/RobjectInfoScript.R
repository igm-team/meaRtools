# check for items in an Robject
# DIana Hall
# 10/18/2016 


library( meaRtools )
rObjectInfo<-function(RobjectPath="no path sent"){
  #RobjectPath="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/Many_treatments/Analysis/R_Objects/WT_20160707_A32PEI_DIV8.RData"
  cat("\n")
  cat(RobjectPath)
  cat("\n")
  t<-load(RobjectPath)
  S<-get(t)
  
  #layout
  if (is.element("layout", names(S))){
    S$layout$array
    cat(paste0(S$layout$array, "\n"))
  }
  
  #rec.time
  if (is.element("rec.time", names(S))){
    cat(paste0("start time=", round(S$rec.time)[1],"\n"))
    cat(paste0("end time=", round(S$rec.time)[2],"\n"))
  }
  
  
  #DIV & wells
  if (is.element("file" ,names(S) )){
    
    temp<-unlist(strsplit(unlist( strsplit(basename( S$file ), fixed=T, split=".RData" )), split="_"))
    cat(paste0(temp[length(temp)],"\n") )
    
  }
  
  #treatment
  if (is.element("treatment" ,names(S)) ){
    
    trt<-unique(S$treatment)
    trt<-trt[trt!=""]
    cat(paste0("treatment =", trt,"\n") )
    
  }
  
  
  
  #wells available
  if (is.element("cw", names(S) ) ){
    cat( paste0("wells available=", unique(S$cw), "\n") )
  }
  
  #ns
  cat( paste("ns_all=", is.element("ns_all", names(S) ) ,"\n"  ) )
  if ( is.element("nb.all", names(S) ) ){
    
    has.ns<-unlist( lapply(S$ns_all, function(x) x$brief["n"]>0) )
    names(has.ns)<-names(S$ns_all)
    cat(paste0("ns wells=", names(has.ns)[has.ns>0],"\n") )
    
  } 
  
  # nb
  cat( paste0("nb.all=", is.element("nb.all", names(S) ) ,"\n"  ) )
  if (is.element("nb.all", names(S) ) ){
    
    #get window sizes
    window.sizes<-names(S$nb.all[[1]])
    cat(paste0("window sizes=", window.sizes,"\n"))
    has.nb<- unlist( lapply( S$nb.all, function(x) nrow(x[[1]] ) ) ) 
    cat(paste0("nb wells=", names(has.nb)[has.nb>0],"\n") )
  } 
  
  
  
  
  
}

args <- commandArgs(trailingOnly = FALSE)
RobjectPath=strsplit(args[6], "=")[[1]][2]
rObjectInfo(RobjectPath=RobjectPath)
cat("\n")


