/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/univLinReg.hpp>

using namespace Rcpp;

/******************************************************************************/

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(const S4& BM,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd,
                 const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigstatsr::univLinReg5(RawSubMatAcc(*xpMat, rows, cols,
                                               BM.slot("code")),
                                  covar_U, y);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigstatsr::univLinReg5(SubMatAcc<char>(*xpMat,   rows, cols),
                                    covar_U, y);
    case 2:
      return bigstatsr::univLinReg5(SubMatAcc<short>(*xpMat,  rows, cols),
                                    covar_U, y);
    case 4:
      return bigstatsr::univLinReg5(SubMatAcc<int>(*xpMat,    rows, cols),
                                    covar_U, y);
    case 6:
      return bigstatsr::univLinReg5(SubMatAcc<float>(*xpMat,  rows, cols),
                                    covar_U, y);
    case 8:
      return bigstatsr::univLinReg5(SubMatAcc<double>(*xpMat, rows, cols),
                                    covar_U, y);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
