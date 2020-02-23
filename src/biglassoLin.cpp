/******************************************************************************/

#include <bigstatsr/BMAcc-dispatcher.h>
#include <bigstatsr/biglasso/linear.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_COPY_CDFIT_GAUSSIAN_HSR(ACC, ACC_VAL) {                           \
  return bigstatsr::biglassoLin::COPY_cdfit_gaussian_hsr(ACC, y,               \
    lambda, center, scale, pf, resid, alpha, eps, max_iter, dfmax,             \
    ACC_VAL, y_val, n_abort, nlam_min);                                        \
}

// Dispatch function for COPY_cdfit_gaussian_hsr
// [[Rcpp::export]]
List COPY_cdfit_gaussian_hsr(Environment BM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const IntegerVector& col_idx,
                             const NumericMatrix& covar,
                             const NumericVector& lambda,
                             const NumericVector& center,
                             const NumericVector& scale,
                             const NumericVector& pf,
                             NumericVector& resid,
                             double alpha,
                             double eps,
                             int max_iter,
                             int dfmax,
                             const IntegerVector& row_idx_val,
                             const NumericMatrix& covar_val,
                             const NumericVector& y_val,
                             int n_abort,
                             int nlam_min) {

  DISPATCH_SUBMATCOVACC_VAL(CALL_COPY_CDFIT_GAUSSIAN_HSR)
}

/******************************************************************************/
