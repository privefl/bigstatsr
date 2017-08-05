/******************************************************************************/

#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/prodMatVec.hpp>

using namespace Rcpp;

/******************************************************************************/

// Dispatch function for pMatVec4
// [[Rcpp::export]]
NumericVector pMatVec4(const S4& BM,
                       const NumericVector& x,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd) {

  myassert(colInd.size() == x.size(), ERROR_DIM);

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigstatsr::pMatVec4(RawSubMatAcc(*xpMat, rows, cols,
                                            BM.slot("code")), x);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigstatsr::pMatVec4(SubMatAcc<char>(*xpMat,   rows, cols), x);
    case 2:
      return bigstatsr::pMatVec4(SubMatAcc<short>(*xpMat,  rows, cols), x);
    case 4:
      return bigstatsr::pMatVec4(SubMatAcc<int>(*xpMat,    rows, cols), x);
    case 6:
      return bigstatsr::pMatVec4(SubMatAcc<float>(*xpMat,  rows, cols), x);
    case 8:
      return bigstatsr::pMatVec4(SubMatAcc<double>(*xpMat, rows, cols), x);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/

// Dispatch function for cpMatVec4
// [[Rcpp::export]]
NumericVector cpMatVec4(const S4& BM,
                        const NumericVector& x,
                        const IntegerVector& rowInd,
                        const IntegerVector& colInd) {

  myassert(rowInd.size() == x.size(), ERROR_DIM);

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigstatsr::cpMatVec4(RawSubMatAcc(*xpMat, rows, cols,
                                             BM.slot("code")), x);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigstatsr::cpMatVec4(SubMatAcc<char>(*xpMat, rows, cols),   x);
    case 2:
      return bigstatsr::cpMatVec4(SubMatAcc<short>(*xpMat, rows, cols),  x);
    case 4:
      return bigstatsr::cpMatVec4(SubMatAcc<int>(*xpMat, rows, cols),    x);
    case 6:
      return bigstatsr::cpMatVec4(SubMatAcc<float>(*xpMat, rows, cols),  x);
    case 8:
      return bigstatsr::cpMatVec4(SubMatAcc<double>(*xpMat, rows, cols), x);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
