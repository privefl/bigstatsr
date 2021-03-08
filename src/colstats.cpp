/******************************************************************************/

#include <bigstatsr/BMAcc-dispatcher.h>
#include <bigstatsr/colstats.hpp>

/******************************************************************************/

#define CALL_BIGCOLVARS(ACC) return bigstatsr::bigcolvars(ACC, ncores);

// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(Environment BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd,
                                 int ncores) {

  DISPATCH_SUBMATACC(CALL_BIGCOLVARS)
}

/******************************************************************************/
