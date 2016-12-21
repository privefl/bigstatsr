// [[Rcpp::depends(bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <Rcpp.h>

using namespace Rcpp;


/******************************************************************************/

template <typename T>
ListOf<NumericVector> bigcolvars(XPtr<BigMatrix> xpMat,
                                 MatrixAccessor<T> macc,
                                 const IntegerVector& rowInd) {
  double n = rowInd.size();
  int m = xpMat->ncol();

  NumericVector res(m);
  NumericVector res2(m);
  double x, xSum, xxSum;

  for (int j = 0; j < m; j++) {
    xSum = xxSum = 0;
    for (int i = 0; i < n; i++) {
      x = macc[j][rowInd[i] - 1];
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
ListOf<NumericVector> bigcolvars(SEXP pBigMat,
                                 const IntegerVector& rowInd) {
  XPtr<BigMatrix> xpMat(pBigMat);
  switch(xpMat->matrix_type()) {
  case 1:
    return bigcolvars(xpMat, MatrixAccessor<char>(*xpMat),   rowInd);
  case 2:
    return bigcolvars(xpMat, MatrixAccessor<short>(*xpMat),  rowInd);
  case 4:
    return bigcolvars(xpMat, MatrixAccessor<int>(*xpMat),    rowInd);
  case 6:
    return bigcolvars(xpMat, MatrixAccessor<float>(*xpMat),  rowInd);
  case 8:
    return bigcolvars(xpMat, MatrixAccessor<double>(*xpMat), rowInd);
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/
