/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& scaling(NumericMatrix& source,
                       const NumericVector& mean,
                       const NumericVector& sd) {

  size_t n = source.rows();
  size_t m = source.cols();
  size_t i, j;

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      source(i, j) -= mean[j];
      source(i, j) /= sd[j];
    }
  }

  return source;
}

/******************************************************************************/

// For a squared matrix
// [[Rcpp::export]]
NumericMatrix& complete2(NumericMatrix& mat) {

  size_t m = mat.ncol();
  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = j+1; i < m; i++)
      mat(i, j) = mat(j, i);

  return mat;
}

/******************************************************************************/

// For a square matrix
// [[Rcpp::export]]
NumericMatrix& incrSup2(NumericMatrix& mat, const NumericMatrix& source) {

  size_t m = mat.ncol();
  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i <= j; i++)
      mat(i, j) += source(i,j);

  return mat;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& correlize(NumericMatrix& mat,
                         const NumericVector& shift,
                         const NumericVector& scale) {

  size_t n = mat.nrow();
  size_t i, j;

  for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      // corresponds to "- \frac{1}{n} s_X * s_X^T"
      mat(i, j) -= shift(i) * shift(j);
      // corresponds to "S^T (...) S"
      mat(i, j) /= scale(i) * scale(j);
    }
  }

  return mat;
}

/******************************************************************************/
