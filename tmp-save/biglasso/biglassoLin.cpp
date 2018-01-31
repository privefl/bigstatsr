/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatCovAcc.h>
#include <bigstatsr/biglasso/linear.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_COPY_CDFIT_GAUSSIAN_HSR(ACC) {                                    \
  return bigstatsr::biglassoLin::COPY_cdfit_gaussian_hsr(                      \
    ACC, y, lambda, L, lam_scale, lambda_min, alpha,                           \
    user, eps, max_iter, m, dfmax, verbose                                     \
  );                                                                           \
}

// Dispatch function for COPY_cdfit_gaussian_hsr
// [[Rcpp::export]]
List COPY_cdfit_gaussian_hsr(Environment BM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const NumericMatrix& covar,
                             NumericVector& lambda,
                             int L,
                             bool lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector& m,
                             int dfmax,
                             bool verbose) {

  DISPATCH_SUBMATCOVACC(CALL_COPY_CDFIT_GAUSSIAN_HSR)
}

/******************************************************************************/
