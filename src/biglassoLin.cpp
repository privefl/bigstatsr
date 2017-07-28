/******************************************************************************/

#include <RcppArmadillo.h>
#include "../inst/include/bigstatsr/SubMatCovAcc.h"
#include "../inst/include/bigstatsr/biglassoLin.hpp"

/******************************************************************************/

// Dispatch function for COPY_cdfit_gaussian_hsr
// [[Rcpp::export]]
List COPY_cdfit_gaussian_hsr(const S4& BM,
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
                             bool verbose) {

  XPtr<BigMatrix> xpMat = BM.slot("address");

  if (Rf_inherits(BM, "BM.code")) {
    return COPY_cdfit_gaussian_hsr(
      RawSubMatCovAcc(*xpMat, row_idx, covar, BM.slot("code")),
      y, lambda, L, lam_scale, lambda_min,
      alpha, user, eps, max_iter, m, dfmax, verbose);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<char>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 2:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<short>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 4:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<int>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 6:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<float>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 8:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<double>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
