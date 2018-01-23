// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat testProdW(const arma::mat& x, const arma::vec& w) {
  return x.t() * (x.each_col() % w);
}

// [[Rcpp::export]]
arma::mat testProdW3(const arma::mat& x, const arma::vec& w) {
  arma::mat tmp = x.each_col() % sqrt(w);
  return tmp.t() * tmp;
}

// [[Rcpp::export]]
arma::mat testProdW2(const arma::mat& x, const arma::vec& w) {

  int n = x.n_rows;
  int K = x.n_cols;
  arma::mat res(K, K);
  double tmp;
  for (int k = 0; k < K; k++) {
    for (int j = k; j < K; j++) {
      tmp = 0;
      for (int i = 0; i < n; i++) {
        tmp += x(i, j) * w[i] * x(i, k);
      }
      res(j, k) = tmp;
    }
  }

  for (int k = 0; k < K; k++) {
    for (int j = 0; j < k; j++) {
      res(j, k) = res(k, j);
    }
  }

  return res;
}

/*** R
mat <- matrix(0, 10e3, 10); mat[] <- rnorm(length(mat))
w <- abs(rnorm(nrow(mat)))

true <- crossprod(mat, mat * w)
all.equal(testProdW(mat, w), true)
all.equal(testProdW2(mat, w), true)
all.equal(testProdW3(mat, w), true)

microbenchmark::microbenchmark(
  testProdW(mat, w),
  testProdW2(mat, w),
  testProdW3(mat, w),
  crossprod(mat, mat * w),
  times = 500
)
*/
