#Diana Hall
# 1/31/2016
# get distribution parameters and plot

make.distribution.plot<-function( distFiles=NULL, 
                                  type=NULL, kotype=NULL, 
                                  typecol=NULL, kotypecol=NULL, np=100){
  
  if( is.null(distFiles) ){
    cat("distFiles is empty, exiting")
    cat("/n")
    return
  }
  
  # get trt
  if ( is.null(type)&& is.null(kotype)  ){
    cat("Missing treatment, exiting")
    cat("/n")
    return
  } else if ( any(is.null(type), is.null(kotype) ) ){
    if (is.null(type)){ type=kotype}else{ kotype=type }
  }
  
  # color
  if ( is.null(type)&& is.null(kotype)  ){
    typecol="blue"; kotypecol="red"
  } else if ( any(is.null(typecol), is.null(kotypecol) ) ){
    if (is.null(typecol)){ typecol=kotypecol
    }else{ kotypecol=typecol }
  }
  # np, check that it's numeric
  np<-as.numeric(np)


  
  if (length(distFiles)>0) {
    for (i in 1:length(distFiles) ) {
      cat(paste("Analysing ",distFiles[i],"\n",sep=""))
      root.dir<-dirname(distFiles[i])
      if (tools::file_ext(basename(distFiles[i])) != "csv")
      {    print(paste("Skipping ",distFiles[i]," - not a csv",sep=""))
        next}
      basename <- strsplit(basename(distFiles[i]),".csv")
      plotPath = paste(root.dir,"/",basename,"_",np,"perm_",type,"_",kotype,"_CDF_EMD.pdf",sep="")
      pdf(file=plotPath) 
      ptm <- proc.time()
      result <- dist.perm(distFiles[i],np,type,kotype)
      max=max(result$data.wt)
      firstT=result$data.wt; firtstCol=typecol
      secondT=result$data.ko;secondCol=kotypecol
      if (max(result$data.ko)>max)
      {
        secondT=result$data.wt; secondCol=typecol
        firstT=result$data.ko;firtstCol=kotypecol
      }
      plot(firstT,col=firtstCol,main=basename,type="l",lwd=3,xlab="Bin number (x.limit x bins.in.seg in burst properties)")
      points(secondT,col=secondCol,type="l",lwd=3)
      par(mfrow=c(1,1))  
      mtext(side = 1, at = 0, line = 4,
            text = paste("P.value Max distance after ",np," permutations: ",format((1-result$perm.p), digits = 3),sep=""),col = "black",cex= 0.9,adj=0)    
      max=max(result$data.wt.Original)
      firstT=result$data.wt.Original; firtstCol=typecol
      secondT=result$data.ko.Original;secondCol=kotypecol   
      if (max(result$data.ko.Original)>max)
      {
        secondT=result$data.wt.Original; secondCol=typecol
        firstT=result$data.ko.Original;firtstCol=kotypecol
      }
      plot(firstT,col=firtstCol,main=basename,type="l",lwd=3,xlab="Bin number (x.limit x bins.in.seg in burst properties)")
      points(secondT,col=secondCol,type="l",lwd=3)
      mtext(side = 1, at = 0, line = 4,
            text = paste("P.value EMD after ",np," permutations: ",format((1-result$perm.EMD), digits = 2),sep=""),col = "black",cex= 0.9,adj=0)    
      graphics.off()
      proc.time() - ptm
      cat(paste("Permutation scheme finished successfully.\nOutput file printed to ",plotPath,"\n",sep=""))
    }}
  
}


#parameters
options(warn=-1)
suppressWarnings(suppressMessages(library(tcltk)))
suppressWarnings(suppressMessages(library(IGM.MEA)))

args <- commandArgs(trailingOnly = FALSE)
Path=dirname(strsplit(args[4], "=")[[1]][2])
distFiles=distFilePath=strsplit(args[6], "=")[[1]][2]
kotype<-strsplit(args[7], "=")[[1]][2]
type<-strsplit(args[8], "=")[[1]][2]
kotypecol<-strsplit(args[9], "=")[[1]][2]
typecol<-strsplit(args[10], "=")[[1]][2]
np<-strsplit(args[11], "=")[[1]][2]

cat("Path= ");cat(Path);cat("\n")
cat("distFiles= ");cat(distFiles); cat("\n")
cat("kotype= ");cat(paste0("'",kotype,"'"));cat("\n");cat("type= ");cat(type);cat("\n");
cat("typecol= ");cat(paste0("'",typecol,"'"));cat("\n");cat("kotypecol= ");cat(kotypecol);cat("\n");
cat("np= ");cat(np); cat("\n")


# distFiles="/Users/dh2744/Dropbox/Columbia/Software/github/mea/test/testData/exampleRecording/Analysis/outputPerDIV/distributionFiles/exampleRecording_1012016_plate1_01-12-17_16_42_24_ISI_distributions.csv"
# kotype="treatX"; type="treatY"; typecol="blue"; kotypecol="red"; np=100
source(paste(Path, "/distribution_permutation_functions.R",sep="") , echo=F)
setwd( dirname(Path) )

make.distribution.plot( distFiles=distFiles, 
                        type=type,kotype=kotype, 
                        typecol=typecol, kotypecol=kotypecol, np=np )
