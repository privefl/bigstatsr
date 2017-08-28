/******************************************************************************/

#include <bigstatsr/BMAcc.h>
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
      mat(i, j) += source(i, j);

  return mat;
}

/******************************************************************************/

// [[Rcpp::export]]
void scaleK(Environment BM,
                      const NumericVector& sums,
                      const NumericVector& mu,
                      const NumericVector& delta,
                      int nrow) {

  XPtr<FBM> xpBM = BM["address"];
  BMAcc<double> K(xpBM);
  size_t n = K.nrow();
  size_t i, j;

  for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      K(i, j) -= sums[i] * mu[j] + mu[i] * sums[j];
      K(i, j) += nrow * mu[i] * mu[j];
      K(i, j) /= delta(i) * delta(j);
    }
  }
}

/******************************************************************************/
