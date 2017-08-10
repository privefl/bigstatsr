/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatCovAcc.h>
#include <bigstatsr/biglasso/logistic.hpp>

using namespace Rcpp;

/******************************************************************************/

#define CALL_COPY_CDFIT_BINOMIAL_HSR(ACC) {                                    \
  return bigstatsr::biglassoLog::COPY_cdfit_binomial_hsr(                      \
    ACC, y, lambda, L, lam_scale, lambda_min, alpha,                           \
    user, eps, max_iter, m, dfmax, warn, verbose                               \
  );                                                                           \
}

// Dispatch function for COPY_cdfit_binomial_hsr
// [[Rcpp::export]]
List COPY_cdfit_binomial_hsr(const S4& BM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const NumericMatrix& covar,
                             NumericVector& lambda,
                             int L,
                             int lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector& m,
                             int dfmax,
                             bool warn,
                             bool verbose) {

  DISPATCH_SUBMATCOVACC(CALL_COPY_CDFIT_BINOMIAL_HSR)
}

/******************************************************************************/
