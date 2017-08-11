#include <Rcpp.h>
using namespace Rcpp;

double run_Pcpp(double dt,
             NumericVector spike_times_1,
             NumericVector spike_times_2){
  /* Calculate the term P_1. the fraction of spikes from train 1 that
   * are within +/- dt of train 2.
   */
  
  // this is similar to the Catherine's code
  
  // not sure if these are better left as input arguments or not
  int N1 = spike_times_1.size();
  int N2 = spike_times_2.size();
  
  int i, j, Nab;
  
  Nab=0;
  j=0;
  for(i=0;i<=(N1-1);i++){
    while(j<N2){	
      /* check every spike in train 1 to see if there's a spike in
       train 2 within dt  (don't count spike pairs)
       don't need to search all j each iteration */
      if(fabs(spike_times_1[i]-spike_times_2[j])<=dt){
        Nab=Nab+1;	
        break;				
      }
      else if(spike_times_2[j]>spike_times_1[i]){			
        break;
      }
      else{
        j=j+1;
      }		
    }
  }
  return Nab;
}

double heaviside_thetacpp(double x){
  
  /* returns x if x positive and 0 otherwise */
  double y = x;
  if (x < 0) y = 0;
  return y;
}

double run_Tcpp(double dt, double start, double end,
                NumericVector spike_times_1){
  
  /* Calculate T_A, the fraction of time 'tiled' by spikes with +/- dt.
  *
  * This calculation requires checks to see that (a) you don't count
  * time more than once (when two or more tiles overlap) and checking
  * beg/end of recording.
  */
  
  // Same as in the previous function, I've cut some of the input variables but 
  // not sure if this is beneficial
  int N1 = spike_times_1.size();
  double time_A;
  int i=0;
  double diff;
  double start_diff, end_diff;
  
  /* maximum */
  time_A=2*(double)N1*dt;
  
  /* Assume at least one spike in train! */
  
  // I've condensed Catherine's code here, using the 'Heaviside theta' function
  // The case where there is only one spike in a train can be absorbed into the general case
  
  while (i<(N1-1)){
    diff = heaviside_thetacpp(2*dt-spike_times_1[i+1]+spike_times_1[i]);
    time_A = time_A - diff;
    i++;
  }
  
  /* check if spikes are within dt of the start and/or end
  if so just need to subtract overlap of first and/or last spike as all
  within-train overlaps have been accounted for*/
  
  start_diff = heaviside_thetacpp(dt+start-spike_times_1[0]);
  end_diff = heaviside_thetacpp(dt+spike_times_1[N1-1]-end);
  
  time_A = time_A - start_diff - end_diff;
  
  return time_A;
}

// [[Rcpp::export]]
double run_TMcpp(double dt, double start, double end,
                 NumericVector spike_times_1, NumericVector spike_times_2) {
  
  /* Calculate the term P_1. the fraction of spikes from train 1 that 
  are within =/- dt of train 2.*/
  
  // I've changed the output from so the STTC is returned rather than assigned
  
  double TA, TB, PA, PB, T;
  int N1 = spike_times_1.size();
  int N2 = spike_times_2.size();
  double sttc;
  
  if(N1==0 || N2==0){
    sttc=NAN;
  }
  else{
    T=end-start;
    TA=run_Tcpp(dt,start,end,spike_times_1);
    TA=TA/T;
    TB=run_Tcpp(dt,start,end,spike_times_2);
    TB=TB/T;
    PA=run_Pcpp(dt,spike_times_1,spike_times_2);
    PA=PA/N1;
    PB=run_Pcpp(dt,spike_times_2,spike_times_1);
    PB=PB/N2;
    
    sttc=0.5*(PA-TB)/(1-TB*PA)+0.5*(PB-TA)/(1-TA*PB);
  }
  return(sttc);
}

// [[Rcpp::export]]
NumericVector tiling_correlogramcpp(NumericVector spikes,
                                    int n,
                                    IntegerVector nspikes,
                                    IntegerVector first_spike,
                                    double start,
                                    double end, /* recording time */
                                    double dt,
                                    double tau_sep,
                                    double tau_max) {
  
  /* Shift spikes in train 2 relative to spikes in train 1 by m*tau */
  
  /* Compute all pairwise interactions, include self. */
  /* Elements on lower diagonal are not touched, so those should remain NA. */
  int n1, n2, a, b, k, l, i;
  // to find the length of (using R syntax) seq(-tau_max,tau_max,tau_sep)
  // round (2*tau_max/tau_sep) + 1 
  // there may be a nicer way of rounding but this seems to work
  int tau_length = (int)((2*tau_max)/tau_sep+1.5);
  NumericVector corrs(tau_length*n*n);
  double start_tau, end_tau, tau;
  NumericVector spikes_tau(spikes.size());
  
  // I don't think the order of the loops matters particularly,
  // I initially had the loop for tau last but moved it first
  
  for (i=0; i<tau_length; i++) {
    
    tau = -tau_max + (i*tau_sep);
    
    // zero padding so must alter the recording time start and end values
    // again using the 'Heaviside theta' function 
    // (it took a bit of time to work my head around this)
    // for a negative tau value, e.g. -0.2 , start -> start + tau = start - 0.2, end remains the same
    // for a positive tau value, e.g. 0.2, start remains the same, end -> end + tau = end + 0.2
    
    start_tau = start - heaviside_thetacpp(tau);
    end_tau = end + heaviside_thetacpp(-tau);
    
    for (a=0; a<n; a++) {
      n1 = nspikes[a];
      NumericVector sa(n1); // this is where I think the function is limited in terms of speed
      
      // again this loop could probably be removed somehow
      for (k=0; k<n1; k++){
        sa[k] = spikes[first_spike[a]-1+k];
      }
      
      for (b=a; b<n; b++) {
        n2 = nspikes[b];
        NumericVector sb(n2);
        
        // as for sa loop
        for (l=0; l<n2; l++){
          sb[l] = spikes[first_spike[b]-1+l];
        }
        
        corrs[((b*n)+a)*tau_length+i] = run_TMcpp(dt,start_tau,end_tau,sa,sb-tau);
        
      }
    }
  }
  return(corrs);
}

/*** R
library(microbenchmark)
microbenchmark(tilingcpp<-tiling_correlogramcpp(unlist(s$spikes),length(s$channels),
                                                s$nspikes,cumsum(c(1,s$nspikes[-length(s$nspikes)])),
                                                s$rec.time[1],s$rec.time[2],0.05,0.1,5),
               times=1L)

## convert so in the same format as the R script
tilingcpp<-matrix(tilingcpp,nrow=length(s$channels)^2,byrow=TRUE)
tilingcpp<-tilingcpp[c(which(diag(length(s$channels))==1),which(upper.tri(array(1,rep(length(s$channels),2)))==1)),]
*/

// as above but specifying allowing only one value of i, a and b to be specified, for checking purposes
// [[Rcpp::export]]
NumericVector tiling_correlogramcpp_index(NumericVector spikes,
                                          int n,
                                          IntegerVector nspikes,
                                          IntegerVector first_spike,
                                          double start,
                                          double end,
                                          double dt,
                                          double tau_sep,
                                          double tau_max,
                                          int a,
                                          int b) {

  /* Shift spikes in train 2 relative to spikes in train 1 by m*tau */

  /* Compute all pairwise interactions, include self. */
  /* Elements on lower diagonal are not touched, so those should remain NA. */
  int n1, n2, k, l, i;
  int tau_length = (int)((2*tau_max)/tau_sep+1.5);
  NumericVector corrs(tau_length);
  double start_tau, end_tau, tau;
  NumericVector spikes_tau(spikes.size());
  
  for (i=0; i<tau_length; i++) {

    tau = -tau_max + (i*tau_sep);

    start_tau = start - heaviside_thetacpp(tau);
    end_tau = end + heaviside_thetacpp(-tau);

    n1 = nspikes[a-1];
    NumericVector sa(n1);
    for (k=0; k<n1; k++){
      sa[k] = spikes[first_spike[a-1]-1+k];
    }

    n2 = nspikes[b-1];
    NumericVector sb(n2);
    for (l=0; l<n2; l++){
      sb[l] = spikes[first_spike[b-1]-1+l];
    }

    corrs[i] = run_TMcpp(dt,start_tau,end_tau,sa,sb-tau);
  
  }

  return(corrs);
}

/***R
## THIS IS THE CODE TO PRODUCE THE MULTIPAGE PDF FOR NEWCASTLE, 
## commented out because it would take a while to run if this file is sourced 
## e.g. library(Rcpp), sourceCpp(tiling.cpp)

# library(R.matlab)
# 
# ch = readMat("../newcastle_data_i/channelNames_CTL.mat")
# 
# channels<-unlist(ch$channelNames[seq(from=1,by=6,len=length(ch$channelNames)/6)])
# channels_s<-gsub('Cluster0','cl_',channels)
# pos<-cbind(as.numeric(unlist(ch$channelNames[seq(from=2,by=6,len=length(ch$channelNames)/6)])),
#            as.numeric(unlist(ch$channelNames[seq(from=3,by=6,len=length(ch$channelNames)/6)])))
# 
# st<-readMat("../newcastle_data_i/spiketimestamps_basicStimulation_CTL.mat")
# 
# spikes<-sapply(st$spiketimestamps,function(l) as.vector(unlist(l[1])))
# names(spikes)<-channels_s
# nspikes<-sapply(spikes,function(l) length(l))
# NCells<-length(spikes)
# rec.time<-round(range(spikes))
# first_spike<-cumsum(c(1,nspikes[-length(nspikes)]))
# 
# s1 = spikes$cl_0004
# plot(s1)
# hist(diff(s1))
# 
# n.samples<-1200; s_pairs<-replicate(n.samples,sample(NCells,2,replace=FALSE))
# tiling_new<-t(simplify2array(
#   mclapply(1:n.samples,function(j){
#     tiling_correlogramcpp_index(unlist(spikes[s_pairs[,j]]),2,nspikes[s_pairs[,j]],
#                                 c(1,1+nspikes[s_pairs[1,j]]),rec.time[1],rec.time[2],0.05,0.1,5,1,2)},
#     mc.cores=detectCores()-1)
# )
# )
# pdf(file="newcastle_sample1.pdf",height=11,width=8)
# nc<-3; nr<-4; par(mfrow=c(nr,nc),mar=c(2,3.5,2.5,0.5)+0.1)
# #nc<-4; par(mfrow=c(ceiling(n.samples/nc),nc),mar=c(2,3.5,2.5,0.5)+0.1)
# for (i in 1:n.samples) {
#   plot(seq(-5,5,0.1),tiling_new[i,],type='l',lwd=2,col='darkblue',
#        ylim=c(-1,1),ylab='',xlab='',xaxt='n',yaxt='n',xaxs='i',yaxs='i')
#   title = sprintf("%d vs %d",s_pairs[1,i]-1,s_pairs[2,i]-1)
#   title(main=title,line=0.4)
#   lines(rep(0,2),c(-1.5,1.5),lwd=1)
#   grid()
#   if (i%%nc==1){axis(2); mtext('STTC',2,line=2,cex=0.7)}
#   if (((i-1)%/%nc)%%nr==0){axis(1); mtext(expression(tau),1,line=2,cex=0.7)}
# }
# dev.off()
*/



// SJE additions

// [[Rcpp::export]]
NumericMatrix sttc_allspikes1(List spikes, double dt, double beg, double end) {
  // Return a matrix M of STTC values where M[i,j] = STTC of spike train i and j.
  // Lower triangular matrix is NA; diagonal elements should be one.
  int n = spikes.size();	// number of spike trains
  NumericMatrix m(n,n);
  std::fill(m.begin(), m.end(), NA_REAL);
  int a, b;
  for (a=0; a<n; a++) {
    NumericVector sa = spikes[a];
    for(b=a; b<n; b++) {
      NumericVector sb = spikes[b];
      m(a,b) =run_TMcpp(dt, beg, end, sa, sb);
    }
  }
  return m;
}

// [[Rcpp::export]]
NumericVector sttcp_ab(NumericVector a,
		       NumericVector b,
		       double start,
		       double end,
		       double dt,
		       double tau_sep,
		       double tau_max) {

  int i;
  int tau_length = (int)((2*tau_max)/tau_sep+1.5);
  NumericVector corrs(tau_length);
  double start_tau, end_tau, tau;
  
  for (i=0; i<tau_length; i++) {

    tau = -tau_max + (i*tau_sep);

    start_tau = start - heaviside_thetacpp(tau);
    end_tau = end + heaviside_thetacpp(-tau);
    corrs[i] = run_TMcpp(dt,start_tau,end_tau,a,b-tau);
  
  }

  return(corrs);
}
