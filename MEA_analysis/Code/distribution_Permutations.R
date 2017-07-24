options(warn=-1)
suppressWarnings(suppressMessages(library(tcltk)))
suppressWarnings(suppressMessages(library(meaRtools)))

# source files
# Diana add May 13, 2015
if (!interactive()){
  # only run this if submitting through shell
  args <- commandArgs(trailingOnly = FALSE)
  Path=dirname(strsplit(args[4], "=")[[1]][2])
} else { #add these just for running code in R 
  Path = paste(dirname(getwd()),"/MEA_analysis/Code",sep="")
  setwd(dirname(Path))
}


source(paste(Path, "/distribution_permutation_functions.R",sep="") , echo=F)
setwd( dirname(Path) )

trt<-"Insufficent"
while ( trt[1]=="Insufficent" ){
  distFiles<-sort(tk_choose.files(caption="Choose distribution files") )
  trt<-get.trt(distFiles)
}

getDistributionToolParams(trt, 
                          color.choices=c("red", "blue", "green", "black", "grey"))
clean.user.input(trt,
                 color.choices=c("red", "blue", "green", "black", "grey") )

# end Diana add May 13, 2015


if (length(distFiles)>0) {
  for (i in 1:length(distFiles)) {
    cat(paste("Analysing ",distFiles[i],"\n",sep=""))
    root.dir<-dirname(distFiles[i])
    if (tools::file_ext(basename(distFiles[i])) != "csv")
    {    print(paste("Skipping ",distFiles[i]," - not a csv",sep=""))
      next}
    basename <- strsplit(basename(distFiles[i]),".csv")
    plotPath = paste(root.dir,"/",basename,"_",np,"perm_",type,"_",kotype,"_CDF_EMD.pdf",sep="")
    pdf(file=plotPath) 
    ptm <- proc.time()
    result <- dist_perm(distFiles[i],np,type,kotype)
    max=max(result$data_wt)
    firstT=result$data_wt; firtstCol=typecol
    secondT=result$data_ko;secondCol=kotypecol
    if (max(result$data_ko)>max)
    {
      secondT=result$data_wt; secondCol=typecol
      firstT=result$data_ko;firtstCol=kotypecol
    }
    plot(firstT,col=firtstCol,main=basename,type="l",lwd=3,xlab="Bin number (x.limit x bins.in.seg in burst properties)")
    points(secondT,col=secondCol,type="l",lwd=3)
    par(mfrow=c(1,1))  
    mtext(side = 1, at = 0, line = 4,
          text = paste("P.value Max distance after ",np," permutations: ",format((1-result$perm.p), digits = 3),sep=""),col = "black",cex= 0.9,adj=0)    
    max=max(result$data_wt_original)
    firstT=result$data_wt_original; firtstCol=typecol
    secondT=result$data_ko_original;secondCol=kotypecol   
    if (max(result$data_ko_original)>max)
    {
      secondT=result$data_wt_original; secondCol=typecol
      firstT=result$data_ko_original;firtstCol=kotypecol
    }
    plot(firstT,col=firtstCol,main=basename,type="l",lwd=3,xlab="Bin number (x.limit x bins.in.seg in burst properties)")
    points(secondT,col=secondCol,type="l",lwd=3)
    mtext(side = 1, at = 0, line = 4,
          text = paste("P.value EMD after ",np," permutations: ",format((1-result$perm_emd), digits = 2),sep=""),col = "black",cex= 0.9,adj=0)    
    graphics.off()
    proc.time() - ptm
    cat(paste("Permutation scheme finished successfully.\nOutput file printed to ",plotPath,"\n",sep=""))
  }}
