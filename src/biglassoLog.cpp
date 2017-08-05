/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatCovAcc.h>
#include <bigstatsr/biglasso/logistic.hpp>

using namespace Rcpp;

/******************************************************************************/

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

  XPtr<BigMatrix> xpMat = BM.slot("address");

  if (Rf_inherits(BM, "BM.code")) {
    return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
      RawSubMatCovAcc(*xpMat, row_idx, covar, BM.slot("code")),
      y, lambda, L, lam_scale, lambda_min,
      alpha, user, eps, max_iter, m,
      dfmax, warn, verbose);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
        SubMatCovAcc<char>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, warn, verbose);
    case 2:
      return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
        SubMatCovAcc<short>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, warn, verbose);
    case 4:
      return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
        SubMatCovAcc<int>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, warn, verbose);
    case 6:
      return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
        SubMatCovAcc<float>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, warn, verbose);
    case 8:
      return  bigstatsr::biglassoLog:: COPY_cdfit_binomial_hsr(
        SubMatCovAcc<double>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, warn, verbose);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
