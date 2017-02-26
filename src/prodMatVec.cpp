/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

template <typename T>
NumericVector pMatVec4(SubMatrixAccessor<T> macc,
                       const NumericVector &x) {
  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(n);
  int i, j;

  for (j = 0; j <= m - 4; j += 4) { // unrolling optimization
    for (i = 0; i < n; i++) {
      res[i] += (x[j] * macc(i, j) + x[j+1] * macc(i, j+1)) +
        (x[j+2] * macc(i, j+2) + x[j+3] * macc(i, j+3));
    } // The parentheses are somehow important.
  }
  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc(i, j);
    }
  }

  return res;
}

NumericVector pMatVec4_RAW(RawSubMatrixAccessor macc,
                          const NumericVector &x) {
  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(n);
  int i, j;

  for (j = 0; j <= m - 4; j += 4) { // unrolling optimization
    for (i = 0; i < n; i++) {
      res[i] += (x[j] * macc(i, j) + x[j+1] * macc(i, j+1)) +
        (x[j+2] * macc(i, j+2) + x[j+3] * macc(i, j+3));
    } // The parentheses are somehow important.
  }
  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc(i, j);
    }
  }

  return res;
}

// Dispatch function for pMatVec4
// [[Rcpp::export]]
NumericVector pMatVec4(XPtr<BigMatrix> xpMat,
                       const NumericVector &x,
                       const IntegerVector &rowInd,
                       const IntegerVector &colInd,
                       const NumericVector& lookup) {
  myassert(colInd.size() == x.size(), ERROR_DIM);

  switch(xpMat->matrix_type()) {
  case 1:
    return pMatVec4(SubMatrixAccessor<char>(*xpMat, rowInd-1, colInd-1),   x);
  case 2:
    return pMatVec4(SubMatrixAccessor<short>(*xpMat, rowInd-1, colInd-1),  x);
  case 4:
    return pMatVec4(SubMatrixAccessor<int>(*xpMat, rowInd-1, colInd-1),    x);
  case 6:
    return pMatVec4(SubMatrixAccessor<float>(*xpMat, rowInd-1, colInd-1),  x);
  case 8:
    return pMatVec4(SubMatrixAccessor<double>(*xpMat, rowInd-1, colInd-1), x);
  case 3:
    return pMatVec4_RAW(RawSubMatrixAccessor(*xpMat, rowInd-1, colInd-1, lookup), x);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/

template <typename T>
NumericVector cpMatVec4(SubMatrixAccessor<T> macc,
                        const NumericVector &x) {
  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(m);
  double tmp;
  int i, j;

  for (j = 0; j < m; j++) {
    tmp = 0;
    for (i = 0; i <= n - 4; i += 4) { // unrolling optimization
      tmp += (macc(i, j) * x[i] + macc(i+1, j) * x[i+1]) +
        (macc(i+2, j) * x[i+2] + macc(i+3, j) * x[i+3]);
    }
    for (; i < n; i++) {
      tmp += macc(i, j) * x[i];
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
  myassert(rowInd.size() == x.size(), ERROR_DIM);

  switch(xpMat->matrix_type()) {
  case 1:
    return cpMatVec4(SubMatrixAccessor<char>(*xpMat, rowInd-1, colInd-1),   x);
  case 2:
    return cpMatVec4(SubMatrixAccessor<short>(*xpMat, rowInd-1, colInd-1),  x);
  case 4:
    return cpMatVec4(SubMatrixAccessor<int>(*xpMat, rowInd-1, colInd-1),    x);
  case 6:
    return cpMatVec4(SubMatrixAccessor<float>(*xpMat, rowInd-1, colInd-1),  x);
  case 8:
    return cpMatVec4(SubMatrixAccessor<double>(*xpMat, rowInd-1, colInd-1), x);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
