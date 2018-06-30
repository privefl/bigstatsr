// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]
#include <RcppArmadillo.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace Eigen;


// [[Rcpp::export]]
arma::vec test_UUt_arma(const arma::mat& U,
                        const arma::vec& x) {

  return U * (U.t() * x);
}

// [[Rcpp::export]]
VectorXd test_UUt_eigen(const Map<MatrixXd> U,
                        const Map<VectorXd> x) {

  return U * (U.adjoint() * x);
}

// [[Rcpp::export]]
NumericVector test_UUt_loop(const NumericMatrix& U,
                            const NumericVector& x) {

  int n = U.nrow();
  int K = U.ncol();
  int i, j, k;
  double cp, cp2;

  NumericVector res(n);
  NumericVector y(K);

  for (j = 0; j < K; j++) {
    cp2 = 0;
    for (k = 0; k < n; k++) {
      cp2 += U(k, j) * x[k];
    }
    y[j] = cp2;
  }

  for (i = 0; i < n; i++) {
    cp = 0;
    for (j = 0; j < K; j++) {
      cp += U(i, j) * y[j];
    }
    res[i] = cp;
  }

  return res;
}


/*** R
N <- 1000
K <- 20
U <- matrix(rnorm(N * K), N)
x <- runif(N)
stopifnot(all.equal(drop(test_UUt_arma(U, x)), test_UUt_loop(U, x)))
stopifnot(all.equal(test_UUt_eigen(U, x), test_UUt_loop(U, x)))
microbenchmark::microbenchmark(
  test_UUt_arma(U, x),
  test_UUt_loop(U, x),
  test_UUt_eigen(U, x)
)
*/
