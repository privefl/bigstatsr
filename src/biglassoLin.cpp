/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatCovAcc.h>
#include <bigstatsr/biglasso/linear.hpp>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define CALL_COPY_CDFIT_GAUSSIAN_HSR(ACC) {                                    \
  return bigstatsr::biglassoLin::COPY_cdfit_gaussian_hsr(                      \
    ACC, y, lambda, L, lam_scale, lambda_min, alpha,                           \
    user, eps, max_iter, m, dfmax, verbose                                     \
  );                                                                           \
}

// Dispatch function for COPY_cdfit_gaussian_hsr
// [[Rcpp::export]]
List COPY_cdfit_gaussian_hsr(Environment FBM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const NumericMatrix& covar,
                             NumericVector& lambda,
                             size_t L,
                             bool lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             size_t max_iter,
                             const NumericVector& m,
                             size_t dfmax,
                             bool verbose) {

  DISPATCH_SUBMATCOVACC(CALL_COPY_CDFIT_GAUSSIAN_HSR)
}

/******************************************************************************/
