/******************************************************************************/

#include <RcppArmadillo.h>
#include "../inst/include/bigstatsr/SubMatAcc.h"
#include "../inst/include/bigstatsr/univLogReg.hpp"

/******************************************************************************/

// Dispatch function for IRLS
// [[Rcpp::export]]
List IRLS(const S4& BM,
          arma::mat& covar,
          const arma::vec& y,
          const arma::vec& z0,
          const arma::vec& w0,
          const IntegerVector& rowInd,
          const IntegerVector& colInd,
          double tol,
          int maxiter) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return IRLS(RawSubMatAcc(*xpMat, rows, cols, BM.slot("code")),
                covar, y, z0, w0, tol, maxiter);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return IRLS(SubMatAcc<char>(*xpMat,   rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 2:
      return IRLS(SubMatAcc<short>(*xpMat,  rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 4:
      return IRLS(SubMatAcc<int>(*xpMat,    rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 6:
      return IRLS(SubMatAcc<float>(*xpMat,  rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 8:
      return IRLS(SubMatAcc<double>(*xpMat, rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
