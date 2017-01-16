/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

template <typename T>
ListOf<NumericVector> bigcolvars(SubMatrixAccessor<T> macc) {
  double n = macc.nrow();
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

  return(List::create(_["sum"] = res2,
                      _["var"] = res/(n-1)));
}

// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(XPtr<BigMatrix> xpMat,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd) {
  switch(xpMat->matrix_type()) {
  case 1:
    return bigcolvars(SubMatrixAccessor<char>(*xpMat,   rowInd-1, colInd-1));
  case 2:
    return bigcolvars(SubMatrixAccessor<short>(*xpMat,  rowInd-1, colInd-1));
  case 4:
    return bigcolvars(SubMatrixAccessor<int>(*xpMat,    rowInd-1, colInd-1));
  case 6:
    return bigcolvars(SubMatrixAccessor<float>(*xpMat,  rowInd-1, colInd-1));
  case 8:
    return bigcolvars(SubMatrixAccessor<double>(*xpMat, rowInd-1, colInd-1));
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
