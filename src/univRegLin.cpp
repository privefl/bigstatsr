// [[Rcpp::depends(bigmemory, BH)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>
#include "utils.h"

using namespace Rcpp;


/******************************************************************************/

template <typename T>
NumericMatrix univRegLin(XPtr<BigMatrix> xpMat,
                         MatrixAccessor<T> macc,
                         const NumericVector& y,
                         const IntegerVector& rowInd) {
  int n = rowInd.size();
  double nd = (double)n;
  int m = xpMat->ncol();

  NumericMatrix res(3, m);

  double ySum = 0, yySum = 0;
  double tmpY;
  int ind;

  for (int i = 0; i < n; i++) {
    ind = rowInd[i] - 1;
    tmpY = y[ind];
    ySum += tmpY;
    yySum += tmpY * tmpY;
  }
  double denoY = yySum - ySum * ySum / nd;

  double xSum, xySum, xxSum;
  double tmp, tmpB;
  double num, denoX;

  for (int j = 0; j < m; j++) {
    xSum = xySum = xxSum = 0;
    for (int i = 0; i < n; i++) {
      ind = rowInd[i] - 1;
      tmp = macc[j][ind];
      xSum += tmp;
      xySum += tmp * y[ind];
      xxSum += tmp * tmp;
    }
    num = xySum - xSum * ySum / nd;
    denoX = xxSum - xSum * xSum / nd;
    tmpB = num / denoX;
    res(1, j) = tmpB;
    res(0, j) = (ySum - tmpB * xSum) / nd;
    res(2, j) = num * num / (denoX * denoY);
  }

  return(res);
}

/******************************************************************************/

// Dispatch function for univRegLin
// [[Rcpp::export]]
NumericMatrix univRegLin(SEXP pBigMat,
                         const NumericVector& y,
                         const IntegerVector& rowInd) {
  XPtr<BigMatrix> xpMat(pBigMat);

  switch(xpMat->matrix_type()) {
  case 1:
    return univRegLin(xpMat, MatrixAccessor<char>(*xpMat),   y, rowInd);
  case 2:
    return univRegLin(xpMat, MatrixAccessor<short>(*xpMat),  y, rowInd);
  case 4:
    return univRegLin(xpMat, MatrixAccessor<int>(*xpMat),    y, rowInd);
  case 6:
    return univRegLin(xpMat, MatrixAccessor<float>(*xpMat),  y, rowInd);
  case 8:
    return univRegLin(xpMat, MatrixAccessor<double>(*xpMat), y, rowInd);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
