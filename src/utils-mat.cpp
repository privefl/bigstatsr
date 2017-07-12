/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& scaling(NumericMatrix& source,
                       const NumericVector& mean,
                       const NumericVector& sd) {
  int n = source.rows();
  int m = source.cols();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      source(i,j) -= mean[j];
      source(i,j) /= sd[j];
    }
  }

  return(source);
}

/******************************************************************************/

// For a squared matrix
// [[Rcpp::export]]
NumericMatrix& complete2(NumericMatrix& mat) {
  int m = mat.ncol();
  int i, j;

  for (j = 0; j < m; j++)
    for (i = j+1; i < m; i++)
      mat(i, j) = mat(j, i);

  return mat;
}

/******************************************************************************/

// For a squared matrix
// [[Rcpp::export]]
NumericMatrix& incrSup2(NumericMatrix& mat, const NumericMatrix& source) {
  int m = mat.ncol();
  int i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i <= j; i++)
      mat(i, j) += source(i,j);

  return mat;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& incrMat(NumericMatrix& dest, const NumericMatrix& source) {
  dest += source;

  return dest;
}

/******************************************************************************/
