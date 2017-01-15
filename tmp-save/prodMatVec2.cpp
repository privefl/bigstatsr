// [[Rcpp::depends(bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <Rcpp.h>

using namespace Rcpp;


/******************************************************************************/

template <typename T>
NumericVector pMatVec4(MatrixAccessor<T> macc,
                       const NumericVector &x,
                       const IntegerVector &rowInd,
                       const IntegerVector &colInd) {
  int n = rowInd.size();
  int m = colInd.size();

  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  NumericVector res(n);
  int i, j, i2, j2;

  for (j = 0; j <= m - 4; j += 4) { // unrolling optimization
    for (i = 0; i < n; i++) {
      i2 = rows[i];
      res[i] += (x[j] * macc[cols[j]][i2] + x[j+1] * macc[cols[j+1]][i2]) +
        (x[j+2] * macc[cols[j+2]][i2] + x[j+3] * macc[cols[j+3]][i2]);
    } // The parentheses are somehow important.
  }
  for (; j < m; j++) {
    j2 = cols[j];
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc[j2][rows[i]];
    }
  }

  return res;
}

// Dispatch function for pMatVec4
// [[Rcpp::export]]
NumericVector pMatVec4(XPtr<BigMatrix> xpMat,
                       const NumericVector &x,
                       const IntegerVector &rowInd,
                       const IntegerVector &colInd) {
  switch(xpMat->matrix_type()) {
  case 1:
    return pMatVec4(MatrixAccessor<char>(*xpMat),   x, rowInd, colInd);
  case 2:
    return pMatVec4(MatrixAccessor<short>(*xpMat),  x, rowInd, colInd);
  case 4:
    return pMatVec4(MatrixAccessor<int>(*xpMat),    x, rowInd, colInd);
  case 6:
    return pMatVec4(MatrixAccessor<float>(*xpMat),  x, rowInd, colInd);
  case 8:
    return pMatVec4(MatrixAccessor<double>(*xpMat), x, rowInd, colInd);
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/

template <typename T>
NumericVector cpMatVec4(MatrixAccessor<T> macc,
                        const NumericVector &x,
                        const IntegerVector &rowInd,
                        const IntegerVector &colInd) {
  int n = rowInd.size();
  int m = colInd.size();

  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  NumericVector res(m);
  double tmp;
  int i, j, j2;

  for (j = 0; j < m; j++) {
    j2 = cols[j];
    tmp = 0;
    for (i = 0; i <= n - 4; i += 4) { // unrolling optimization
      tmp += (macc[j2][rows[i]] * x[i] + macc[j2][rows[i+1]] * x[i+1]) +
        (macc[j2][rows[i+2]] * x[i+2] + macc[j2][rows[i+3]] * x[i+3]);
    }
    for (; i < n; i++) {
      tmp += macc[j2][rows[i]] * x[i];
    }
    res[j] = tmp;
  }

  return res;
}

// Dispatch function for cpMatVec4
// [[Rcpp::export]]
NumericVector cpMatVec4(XPtr<BigMatrix> xpMat,
                        const NumericVector &x,
                        const IntegerVector &rowInd,
                        const IntegerVector &colInd) {
  switch(xpMat->matrix_type()) {
  case 1:
    return cpMatVec4(MatrixAccessor<char>(*xpMat),   x, rowInd, colInd);
  case 2:
    return cpMatVec4(MatrixAccessor<short>(*xpMat),  x, rowInd, colInd);
  case 4:
    return cpMatVec4(MatrixAccessor<int>(*xpMat),    x, rowInd, colInd);
  case 6:
    return cpMatVec4(MatrixAccessor<float>(*xpMat),  x, rowInd, colInd);
  case 8:
    return cpMatVec4(MatrixAccessor<double>(*xpMat), x, rowInd, colInd);
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/
