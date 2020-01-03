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

// [[Rcpp::export]]
void incr_FBM_mat(Environment BM,
                  const NumericMatrix& mat) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  if (xpBM->matrix_type() != 8)
    Rcpp::stop("'big_increment()' works with 'double' FBMs only.");

  BMAcc_RW<double> macc(xpBM);

  size_t n = macc.nrow();
  size_t m = macc.ncol();

  myassert_size(mat.rows(), n);
  myassert_size(mat.cols(), m);

  for (size_t j = 0; j < m; j++)
    for (size_t i = 0; i < n; i++)
      macc(i, j) += mat(i, j);
}

// [[Rcpp::export]]
void incr_FBM_vec(Environment BM,
                  const NumericVector& vec) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  if (xpBM->matrix_type() != 8)
    Rcpp::stop("'big_increment()' works with 'double' FBMs only.");

  BMAcc_RW<double> macc(xpBM);

  size_t n = macc.size();
  myassert_size(vec.size(), n);

  for (size_t i = 0; i < n; i++)
    macc[i] += vec[i];
}

/******************************************************************************/

// For a square FBM
// [[Rcpp::export]]
void scaleK(Environment BM,
            const NumericVector& sums,
            const NumericVector& mu,
            const NumericVector& delta,
            int nrow) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  BMAcc_RW<double> K(xpBM);

  size_t n = K.nrow();
  myassert_size(K.ncol(), n);

  for (size_t j = 0; j < n; j++) {
    for (size_t i = 0; i < n; i++) {
      K(i, j) -= sums[i] * mu[j] + mu[i] * sums[j];
      K(i, j) += nrow * mu[i] * mu[j];
      K(i, j) /= delta(i) * delta(j);
    }
  }
}

/******************************************************************************/
