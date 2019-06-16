/******************************************************************************/

#include <bigstatsr/BMAcc.h>

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

// [[Rcpp::export]]
NumericMatrix& centering(NumericMatrix& source,
                         const NumericVector& mean) {

  size_t n = source.rows();
  size_t m = source.cols();
  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      source(i, j) -= mean[j];

  return source;
}

/******************************************************************************/

// For a squared FBM
// [[Rcpp::export]]
void complete2(Environment BM) {

  XPtr<FBM> xpBM = BM["address"];
  BMAcc<double> K(xpBM);
  size_t m = K.ncol();
  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = j + 1; i < m; i++)
      K(i, j) = K(j, i);
}

/******************************************************************************/

// For a square FBM
// [[Rcpp::export]]
void incrSup2(Environment BM,
              const NumericMatrix& source) {

  XPtr<FBM> xpBM = BM["address"];
  BMAcc<double> K(xpBM);
  size_t m = K.ncol();
  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i <= j; i++)
      K(i, j) += source(i, j);
}

/******************************************************************************/

// For a square FBM
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
