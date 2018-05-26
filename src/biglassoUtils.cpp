/******************************************************************************/

#include <bigstatsr/biglasso/utils.hpp>
#include <bigstatsr/SubMatCovAcc.h>

/******************************************************************************/

#define CALL_BIGSUMMARIES(ACC) {                                               \
return bigstatsr::biglassoUtils::get_summaries(ACC, y, which_set - 1, K);      \
}

// Dispatch function for get_summaries
// [[Rcpp::export]]
NumericVector bigsummaries(Environment BM,
                           const IntegerVector& row_idx,
                           const IntegerVector& col_idx,
                           const NumericMatrix& covar,
                           const NumericVector& y,
                           const IntegerVector& which_set,
                           int K) {

  DISPATCH_SUBMATCOVACC(CALL_BIGSUMMARIES)
}

/******************************************************************************/
