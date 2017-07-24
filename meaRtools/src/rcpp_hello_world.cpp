
#include <Rcpp.h>
using namespace Rcpp;

List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

// [[Rcpp::export]]
List rcpp_hello_world2() {

  CharacterVector x = CharacterVector::create( "apple", "foo", "bar" )  ;
  NumericVector y   = NumericVector::create( 2.0, 1.0 ) ;
  List z            = List::create( x, y ) ;

  return z ;
}

NumericMatrix make_mat(int r, int c) {
  NumericMatrix m(r, c);
  std::fill(m.begin(), m.end(), NA_REAL);
  return m;
}

// [[Rcpp::export]]
NumericMatrix frate_counts(List spikes, double beg, double end, double wid, int nbins)
{

  /* Compute the number of spikes firing in bins of width WID.
   * BEG, END = first and last spike time.
   * WID = duration of each network spike bin.
   */
  int i, nspikes, b, skip, train;
  int ntrains = spikes.size();
  NumericMatrix counts(nbins, ntrains);
  
  for (train=0; train<ntrains; train++) {
    NumericVector s = spikes[train];
    /* Count the spikes on electrode UNIT. */
    nspikes = s.size();
    for(i=0; i<nspikes; i++) {
      b = (int) ( (s[i] - beg)/wid); /* calc bin number; increment spike ptr */

      /* Check bin number is valid: shouldn't happen. */
      if ( (b <0 ) || (b >= nbins))
	/* Rprintf("bin number wrong %f %d\n", *(p-1), b); */
	skip++;
      else {
	/* Update count in relevant bin. */
	counts(b,train)++;
      }
    }
  }
  return counts;

}


NumericMatrix make_mat1(int r, int c) {
  NumericMatrix m(r, c);
  std::fill(m.begin(), m.end(), NA_REAL);
  return m;
}
