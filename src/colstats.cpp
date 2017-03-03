/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

template <class C>
ListOf<NumericVector> bigcolvars(C macc) {
  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(m), res2(m);
  double x, xSum, xxSum;
  int i, j;

  for (j = 0; j < m; j++) {
    xSum = xxSum = 0;
    for (i = 0; i < n; i++) {
      x = macc(i, j);
      xSum += x;
      xxSum += x*x;
    }
    res[j] = xxSum - xSum * xSum / n;
    res2[j] = xSum;
  }

  return List::create(_["sum"] = res2,
                      _["var"] = res/(n-1));
}

// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(const S4& BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigcolvars(RawSubMatAcc(*xpMat, rows, cols, BM.slot("code")));
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return bigcolvars(SubMatAcc<char>(*xpMat,   rows, cols));
    case 2:
      return bigcolvars(SubMatAcc<short>(*xpMat,  rows, cols));
    case 4:
      return bigcolvars(SubMatAcc<int>(*xpMat,    rows, cols));
    case 6:
      return bigcolvars(SubMatAcc<float>(*xpMat,  rows, cols));
    case 8:
      return bigcolvars(SubMatAcc<double>(*xpMat, rows, cols));
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
