/******************************************************************************/

#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/colstats.hpp>

/******************************************************************************/

// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(const S4& BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigstatsr::bigcolvars(RawSubMatAcc(*xpMat, rows, cols,
                                              BM.slot("code")));
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigstatsr::bigcolvars(SubMatAcc<char>(*xpMat,   rows, cols));
    case 2:
      return bigstatsr::bigcolvars(SubMatAcc<short>(*xpMat,  rows, cols));
    case 4:
      return bigstatsr::bigcolvars(SubMatAcc<int>(*xpMat,    rows, cols));
    case 6:
      return bigstatsr::bigcolvars(SubMatAcc<float>(*xpMat,  rows, cols));
    case 8:
      return bigstatsr::bigcolvars(SubMatAcc<double>(*xpMat, rows, cols));
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
