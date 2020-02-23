/******************************************************************************/

#include <bigstatsr/BMAcc-dispatcher.h>
#include <bigstatsr/colstats.hpp>

/******************************************************************************/

#define CALL_BIGCOLVARS(ACC) return bigstatsr::bigcolvars(ACC);

// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(Environment BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd) {

  DISPATCH_SUBMATACC(CALL_BIGCOLVARS)
}

/******************************************************************************/
