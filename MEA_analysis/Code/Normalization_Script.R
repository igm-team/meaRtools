###############################################################################
# Purpose: To normalize data across MEA plates by Maximum firing rate in any  #
#well across all DIVS                                                         #
# Author:   Chris Bostick / Ryan Dhindsa                                      #
###############################################################################

# Imports ---------------------------------------------------------------------
library(data.table)
library(gtools)
library(plyr)
library(ggplot2)

# Functions -------------------------------------------------------------------
Analyze <- function(in.dir, out.dir, treatments=c(), feature_name, norm.method) {
  # extract all filenames from a given directory
  #
  # Args:
  #   in.dir = input directory containing all relevant .csv files 
  #   out.dir = output directory for all files analyzed
  #   treatments = selected genotypes/conditions to be analyzed
  #   feature name = relevant paramter analyzed
  #   norm.method = normalization method used
  # Returns:
  #   a list of file names
  
  filenames <- list.files(in.dir, pattern = '.csv', full.names = TRUE)
  all.files.dfs <- lapply(filenames, ReadFile)
  
  
  # choose normalization method
  # WT = normalize each plate by WT avg/DIV/plate and then average
  if (norm.method == "WT") {
    normalized.dfs <- lapply(all.files.dfs, NormalizebyTreatment, "WT")
    all.norm.data.df <- rbindlist(normalized.dfs, fill = TRUE)
  }
  #ALL_WT = normalize each plate by WT avg/DIV combining WT from all plates
  if (norm.method == "ALL_WT") {
    merged.dfs <- MergeDfs(all.files.dfs)
    all.norm.data.df <- NormalizebyTreatment(merged.dfs, "WT")
    all.norm.data.df <- data.table(all.norm.data.df)
    }
  # all = normalize each plate by avg firing/DIV then average
  if (norm.method == "all") {
    normalized.dfs <- lapply(all.files.dfs, NormalizebyTreatment, "all")
    all.norm.data.df <- rbindlist(normalized.dfs, fill = TRUE)
  }
  #max = normalize each plate by highest value found on any given DIV and then average all plates
  if (norm.method == "max") {
    normalized.dfs <- lapply(all.files.dfs, NormalizeByMax)
    all.norm.data.df <- rbindlist(normalized.dfs, fill = TRUE)
  }
  #none = average values per DIV per treatment for all plates with no normalization
  if (norm.method == "none") {
    normalized.dfs <- lapply(all.files.dfs, NormalizeByNone)
    all.norm.data.df <- rbindlist(normalized.dfs, fill = TRUE)
  }

  
  
  # the concatenated DF has columns out of order. Use mixedsort()
  # to reorder them
  
  order.colnames <- mixedsort(colnames(all.norm.data.df))

  all.norm.data.df = all.norm.data.df[,..order.colnames]
  
  # reorders df so that well, genotype, plate are first 3 columns
  all.norm.data.df = all.norm.data.df[, c(ncol(all.norm.data.df),
                                          ncol(all.norm.data.df) - 1, 
                                          ncol(all.norm.data.df) - 2, 
                                          1:(ncol(all.norm.data.df)-3)), 
                                      with = F]
  
  # write out summary statistics based on which normalization method chosen
  outdir.name <- file.path(out.dir, "analysis")
  dir.create(outdir.name)
 
   if (norm.method == "WT") {
     summary.file = file.path(outdir.name, "WT_summary.csv")
     summary.df <- summary(all.norm.data.df)
     norm.file = file.path(outdir.name, "WT_normalized.csv")
     
   }
  if (norm.method == "ALL_WT") {
    summary.file = file.path(outdir.name, "ALL_WT_summary.csv")
    summary.df <- summary(all.norm.data.df)
    norm.file = file.path(outdir.name, "ALL_WT_normalized.csv")
  }
  if (norm.method == "all") {
    summary.file = file.path(outdir.name, "all_summary.csv")
    summary.df <- summary(all.norm.data.df)
    norm.file = file.path(outdir.name, "all_normalized.csv")
  }
  
  if (norm.method == "max") {
    summary.file = file.path(outdir.name, "max_summary.csv")
    summary.df <- summary(all.norm.data.df)
    norm.file = file.path(outdir.name, "max_normalized.csv")
  }
  
  if (norm.method == "none") {
    summary.file = file.path(outdir.name, "none_summary.csv")
    summary.df <- summary(all.norm.data.df)
    norm.file = file.path(outdir.name, "none_normalized.csv")
  }
  
  write.csv(summary.df, summary.file, row.names = FALSE)
  write.csv(all.norm.data.df, norm.file, row.names = FALSE)
  # plot the data
  plot.out <- file.path(outdir.name, "plot.pdf")
  PlotFeature(all.norm.data.df, feature_name, "combined_data", treatments, plot.out)
  
  # calculate permutation p-values
  
  
  return(all.norm.data.df)
  }
#Function to merge all files into one df
MergeDfs <- function(list.of.dfs) {
  merged.df <- rbindlist(list.of.dfs, fill=TRUE)
  # merged.df <- do.call("rbind", c(list.of.dfs, fill=TRUE))
  
  return(merged.df)
}

ExtractPlateID <- function(file.name) {
  file.base <- basename(file.name) 
  plate.id <- strsplit(file.base, "_")[[1]][3]
  return(plate.id)
}

ReadFile <- function(file.name) {
  df <- read.csv(file.name)
  df$plate <- ExtractPlateID(file.name)
  return(df)
}

NormalizeByMax <- function(df) {
  temp.df <- subset(df, select = -c(well, treatment, plate))
  max.val <- max(temp.df)
  div.cols <- grep("DIV", colnames(df))
  df[, div.cols] = df[, div.cols] / max.val
  return(df)
}

NormalizeByNone <- function(df) {
  temp.df <- subset(df, select = -c(well, treatment, plate))
  div.cols <- grep("DIV", colnames(df))
  df[, div.cols] = df[, div.cols]
  return(df)
}

NormalizebyTreatment <- function(df, norm.treatment){
   if (norm.treatment == "all") {
     temp.df <- subset(df, select=-c(well, treatment, plate))
     div.avgs <- colMeans(temp.df, na.rm = TRUE)
     } else {
       wt.df <- subset(df, treatment == norm.treatment)
       wt.temp.df <- subset(df, treatment == norm.treatment, select = -c(well, treatment, plate))
       div.avgs <- colMeans(wt.temp.df, na.rm = TRUE)
       }
  #wt.df creates a temporary df with only WT wells to allow column means to be obtained for normalization purposes
  all.temp.df <- subset(df, select=-c(well, treatment, plate))
  
  normalized.df <- sweep(all.temp.df, 2, div.avgs, '/')
  
  df <- data.frame(df)
  normalized.df[c("well", "plate", "treatment")] = df[c("well", "plate", "treatment")]
  return(normalized.df)
}

PlotFeature <- function(df, feature, platename, treatments, out.fname) {
  # Uses ggplot to plot feature data
  #
  # Args:
  #   df = a datafraem containing feature data
  #   feature = name of feature
  #   platename
  #           
  # Returns:
  #   plot
  
  df = data.frame(df)
  df$well = NULL
  df$plate = NULL
  
  df[,-1] = sapply(df[,-1], as.numeric)
  
  melted.df = melt(df, id.vars = "treatment")
  
  if(length(treatments) > 0){
    melted.df <- melted.df[melted.df$treatment %in% treatments, ] 
  }
  
  melted.df <- melted.df[!is.na(melted.df[,'value']),]
  
  melted.df = with(melted.df, {ddply(melted.df, c("treatment", "variable"), summarize, 
                                     mean=mean(value),
                                     sem = sqrt(var(value)/length(value)))})
  
  
  pd <- position_dodge(width = 0.1)
  title <- paste(platename, "_", feature, sep="")
  
  # no values for feature, return empty plot
  if (all(is.na(melted.df$mean))){
    x = with(melted.df, { ggplot()+
        ggtitle(paste0("\n", title,"\n"))+
        xlab("")+
        ylab(paste0("\n", feature, "\n"))})
  }else{
    x = with(melted.df, { ggplot(melted.df, aes(x=variable, y=mean, group = treatment))+
        geom_point(aes(color=factor(treatment)), position = pd)+
        geom_line(aes(color=factor(treatment)), position = pd)+
        geom_errorbar(aes(x=variable, ymin = mean-sem, ymax=mean+sem, 
                          color = factor(treatment)), width = 0.1, position = pd)+
        ggtitle(paste0("\n", title,"\n"))+
        xlab("")+
        ylab(paste0("\n", feature, "\n"))})
  }
  
  x+labs(color="Treatment")
  ggsave(out.fname, x, width=11, height=8.5, "pdf")
  return(x)
}


SubsetDIVs <- function(df, divs){
  # allows for selection of specific DIVs for analysis
  #
  # Args:
  #   df = a dataframe with featre data
  #   divs = the DIVs of interest
  
  #           
  # Returns:
  #   a df of only selected DIVs with treatment labels
  not.found <- div.subset[!div.subset %in% colnames(df)]
  if(length(not.found) > 0) warning(cat("Invalid DIVs specified: ", not.found))
  
  divs <- append(divs, c("treatment"), after=0)
  df.subsetted <- df[, ..divs]
  df.subsetted$well <- df$well
  df.subsetted$plate <- df$plate
  
  # reorder
  order.colnames <- mixedsort(colnames(df.subsetted))
  
  df.subsetted <- df.subsetted[,..order.colnames]
  
  # reorders df so that well, genotype, plate are first 3 columns
  df.subsetted <- df.subsetted[, c(ncol(df.subsetted),
                                          ncol(df.subsetted) - 1, 
                                          ncol(df.subsetted) - 2, 
                                          1:(ncol(df.subsetted)-3)), 
                                      with = F]
  return(df.subsetted)
}

extract.rows<-function(df, type){
  # Extracts rows for particular treatment 
  #
  # Args:
  #   df = a dataframe
  #   type = treatment
  #           
  # Returns:
  #   list of data frames containing spike data
  x = df[df$treatment == type&!is.na(df$treatment), ]
  x = with(df,df[treatment == type&!is.na(treatment), ])
  x$treatment = NULL
  
  return(x)
}

mann.whit.perm <- function(df, wt, trt, np){
  # Calculates mann-whit p-value and permutation test for a dataframe
  #     must specify wt and the treatment you are testing
  #
  # Args:
  #   df = a dataframe with featre data
  #   wt = the treatment that will be considred wt
  #   trt = the treatment that will be compared against wt (eg "HOM")
  #   np = number of permutations
  #           
  # Returns:
  #   a df that contains a permutation p-value and a mann whit p-value
  
  # if there is a plate column, delete it
  df$plate <- NULL
  df$well <- NULL
  
  wt.df <- extract.rows(df, wt)
  
  trt.df <- extract.rows(df, trt)

  if (is.na(trt) | is.na(wt) |nrow(wt.df)==0 | nrow(trt.df)==0 | all(is.na(trt.df)) | all(is.na(wt.df))){
    return (data.frame(perm.p=NA,data.p=NA))
  }
  
  #pool wt and trt data
  pool <- rbind(wt.df, trt.df)
  pool = as.matrix(pool)
  
  n.wt <- nrow(wt.df)
  n <- nrow(pool)
  
  # randomly sample the data, compute p-values and store in outp
  outp = matrix(0,np,1)
  for (i in 1:np) {
    wt.smpl = sample(n, n.wt)
    perm.wt = as.numeric(as.vector(pool[wt.smpl,]))
    perm.trt = as.numeric(as.vector(pool[-wt.smpl,]))
    outp[i] <- wilcox.test(perm.wt, perm.trt)$p.value
  }
  outp = sort(outp)
  
  # calculate actual p-val from data
  data.wt = as.numeric(unlist(wt.df))
  data.trt = as.numeric(unlist(trt.df))
  data.p <- wilcox.test(data.wt, data.trt)$p.value
  #  }
  
  if (data.p == "NaN"){
    perm.p <- "NaN"
  } else{
    perm.p <- length(which(outp<data.p)) / np
    if(perm.p == 0){
      perm.p = paste("<", 1/np)
    }
    data.p<-signif(data.p,3)
  }
  return(data.frame(perm.p=perm.p,mann_whit.p=data.p))
}

get.wt <- function(s){
  # Uses tcltk user input to specify which treatment should be considered wt
  #     later on, we calculate p-value for every other treatment vs. this wt specification
  #
  # Args:
  #   s object
  #           
  # Returns:
  #   wt
  choices = as.vector(unique(s[[1]]$treatment[!is.na(s[[1]]$treatment)&s[[1]]$treatment!=""]))
  wt = tk_select.list(choices, preselect = NULL, multiple = FALSE,
                      title = "Choose wildtype/reference for permutation test")
  return(wt)
}
# Main ------------------------------------------------------------------------
# will become customizable w/ argparse
treatments = c("WT", "HOM")
feature_name = "Mean Dur"

df = Analyze("/Users/dh2744/Dropbox/Columbia/Software/normalization_Chris/test_folder_normalization",
             "/Users/dh2744/Dropbox/Columbia/Software/normalization_Chris/test_folder_normalization",
             treatments,
             feature_name,
             norm.method = "ALL_WT")

div.subset <- c("div11",  "div13", "div15")
data.subsetted <- SubsetDIVs(df = df, divs = div.subset)


#mann.whit.perm(df, "WT" , "HOM" , 1000)
#mann.whit.perm(data.subsetted, "WT" , "HOM" , 1000)

