/******************************************************************************/

#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/prodMatVec.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_PMATVEC4(ACC) return bigstatsr::pMatVec4(ACC, x);

// Dispatch function for pMatVec4
// [[Rcpp::export]]
NumericVector pMatVec4(const S4& BM,
                       const NumericVector& x,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd) {

  myassert(colInd.size() == x.size(), ERROR_DIM);

  DISPATCH_SUBMATACC(CALL_PMATVEC4)
}

/******************************************************************************/

#define CALL_CPMATVEC4(ACC) return bigstatsr::cpMatVec4(ACC, x);

// Dispatch function for cpMatVec4
// [[Rcpp::export]]
NumericVector cpMatVec4(const S4& BM,
                        const NumericVector& x,
                        const IntegerVector& rowInd,
                        const IntegerVector& colInd) {

  myassert(rowInd.size() == x.size(), ERROR_DIM);

  DISPATCH_SUBMATACC(CALL_CPMATVEC4)
}

/******************************************************************************/
