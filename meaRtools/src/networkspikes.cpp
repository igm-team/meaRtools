#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector count_ns(List spikes, double beg, double end, double wid,
		       double nbins) {
  NumericVector count(nbins);
  int nunits = spikes.size();
  int unit, n, i, b, last;

  for (unit=0; unit<nunits; unit++) {
    /* Count the spikes on electrode UNIT. */
    NumericVector train = spikes[unit];
    n = train.size();
    last = -1;			/* check to only increment bin once per unit. */

    for (i=0; i<n; i++) {
      b = (int) ( (train[i] - beg)/wid); /* calc bin number; increment spike ptr */
      /* Check bin number is valid: shouldn't happen. */
      if ( (b <0 ) || (b >= nbins))
	Rprintf("bin number wrong %f %d\n", train[i], b);
      else {
	/* Update count in relevant bin. */
	if (last != b) {
	  count[b]++;
	  last = b;		/* stop this bin being updated again for
				 * current unit. */
	}
      }
    }
  }
  return count;
}
  
