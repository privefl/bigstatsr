/******************************************************************************/

#include <bigstatsr/BMAcc-dispatcher.h>
#include <bigstatsr/prodMatVec.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_PMATVEC4(ACC) return bigstatsr::pMatVec4(ACC, x);

// Dispatch function for pMatVec4
// [[Rcpp::export]]
NumericVector pMatVec4(Environment BM,
                       const NumericVector& x,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd) {

  myassert_size(colInd.size(), x.size());

  DISPATCH_SUBMATACC(CALL_PMATVEC4)
}

/******************************************************************************/

#define CALL_CPMATVEC4(ACC) return bigstatsr::cpMatVec4(ACC, x);

// Dispatch function for cpMatVec4
// [[Rcpp::export]]
NumericVector cpMatVec4(Environment BM,
                        const NumericVector& x,
                        const IntegerVector& rowInd,
                        const IntegerVector& colInd) {

  myassert_size(rowInd.size(), x.size());

  DISPATCH_SUBMATACC(CALL_CPMATVEC4)
}

/******************************************************************************/
