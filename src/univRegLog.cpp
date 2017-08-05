/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/univLogReg.hpp>

using namespace Rcpp;

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
    return bigstatsr::IRLS(RawSubMatAcc(*xpMat, rows, cols, BM.slot("code")),
                           covar, y, z0, w0, tol, maxiter);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigstatsr::IRLS(SubMatAcc<char>(*xpMat,   rows, cols),
                             covar, y, z0, w0, tol, maxiter);
    case 2:
      return bigstatsr::IRLS(SubMatAcc<short>(*xpMat,  rows, cols),
                             covar, y, z0, w0, tol, maxiter);
    case 4:
      return bigstatsr::IRLS(SubMatAcc<int>(*xpMat,    rows, cols),
                             covar, y, z0, w0, tol, maxiter);
    case 6:
      return bigstatsr::IRLS(SubMatAcc<float>(*xpMat,  rows, cols),
                             covar, y, z0, w0, tol, maxiter);
    case 8:
      return bigstatsr::IRLS(SubMatAcc<double>(*xpMat, rows, cols),
                             covar, y, z0, w0, tol, maxiter);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
