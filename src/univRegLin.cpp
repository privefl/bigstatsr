// [[Rcpp::depends(bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <Rcpp.h>

using namespace Rcpp;


/******************************************************************************/

template <typename T>
ListOf<SEXP> univRegLin(XPtr<BigMatrix> xpMat,
                        MatrixAccessor<T> macc,
                        const NumericVector& y,
                        const IntegerVector& rowInd) {
  int n = rowInd.size();
  double nd = (double)n;
  int m = xpMat->ncol();

  NumericVector res(m);
  NumericVector std(m);

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
  double tmp, tmpB, tmpR;
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
    res[j] = tmpB;
    tmpR = num / sqrt(denoX * denoY);
    std[j] = tmpB / tmpR * sqrt((1 - tmpR * tmpR) / (n - 2));
  }

  return(List::create(_["estim"] = res,
                      _["std.err"] = std));
}

/******************************************************************************/

// Dispatch function for univRegLin
// [[Rcpp::export]]
ListOf<SEXP> univRegLin(SEXP pBigMat,
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
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/
