/******************************************************************************/

  // [[Rcpp::depends(RcppArmadillo, RcppEigen)]]
#include <RcppArmadillo.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace Eigen;


// [[Rcpp::export]]
arma::mat test_cprod_arma(const arma::mat& X,
                          const arma::vec& p) {

  int K = X.n_cols;
  arma::mat cprod(K, K);

  cprod = X.t() * (X.each_col() % (p % (1 - p)));

return cprod;
}

// [[Rcpp::export]]
MatrixXd test_cprod_eigen(const Map<MatrixXd> X,
                          const Map<VectorXd> p) {

  int K = X.cols();
  MatrixXd cprod(K, K);

  VectorXd w = (p.array() * (1 - p.array())).sqrt();

  MatrixXd X2 = X.array().colwise() * w.array();

  cprod = MatrixXd(K, K).setZero()
  .selfadjointView<Lower>()
  .rankUpdate(X2.adjoint());

  return cprod;
}

// [[Rcpp::export]]
MatrixXd test_cprod_eigen2(const Map<MatrixXd> X,
                           const Map<VectorXd> p) {

  int K = X.cols();
  MatrixXd cprod(K, K);
  MatrixXd I = MatrixXd::Identity(K, K);

  VectorXd w = (p.array() * (1 - p.array()));

  DiagonalMatrix<double, Dynamic> W = w.asDiagonal();

  cprod = X.adjoint() * W * X;

  return cprod;
}


// [[Rcpp::export]]
NumericMatrix test_cprod_loop(const NumericMatrix& X,
                              const NumericVector& p) {

  int n = X.nrow();
  int K = X.ncol();
  int i, j, k;
  NumericMatrix cprod(K, K);
  double cp;

  NumericVector w = p * (1 - p);

  for (j = 0; j < K; j++) {

    cp = 0;
    for (i = 0; i < n; i++) {
      cp += X(i, j) * X(i, j) * w[i];
    }
    cprod(j, j) = cp;

    for (k = j + 1; k < K; k++) {
      cp = 0;
      for (i = 0; i < n; i++) {
        cp += X(i, j) * X(i, k) * w[i];
      }
      cprod(k, j) = cprod(j, k) = cp;
    }
  }

  return cprod;
}

// [[Rcpp::export]]
arma::mat test_cprod_arma_inv(const arma::mat& X,
                              const arma::vec& p) {
  return inv(test_cprod_arma(X, p));
}

// [[Rcpp::export]]
MatrixXd test_cprod_eigen_inv(const Map<MatrixXd> X,
                              const Map<VectorXd> p) {

  int K = X.cols();
  MatrixXd cprod(K, K);
  MatrixXd I = MatrixXd::Identity(K, K);

  VectorXd w = (p.array() * (1 - p.array())).sqrt();

  MatrixXd X2 = X.array().colwise() * w.array();

  cprod.setZero()
  .selfadjointView<Lower>()
  .rankUpdate(X2.adjoint());

  return cprod.ldlt().solve(I);
}

// [[Rcpp::export]]
MatrixXd test_cprod_eigen_inv2(const Map<MatrixXd> X,
                               const Map<VectorXd> p) {

  int K = X.cols();
  MatrixXd cprod(K, K);
  MatrixXd I = MatrixXd::Identity(K, K);

  VectorXd w = (p.array() * (1 - p.array()));

  DiagonalMatrix<double, Dynamic> W = w.asDiagonal();

  cprod = X.adjoint() * W * X;

  return cprod.ldlt().solve(I);
}

/*** R
N <- 1e4
K <- 20
X <- matrix(rnorm(N * K), N)
p <- runif(N)
stopifnot(all.equal(test_cprod_arma(X, p), test_cprod_loop(X, p)))
stopifnot(all.equal(test_cprod_eigen(X, p), test_cprod_loop(X, p)))
stopifnot(all.equal(test_cprod_eigen2(X, p), test_cprod_loop(X, p)))
microbenchmark::microbenchmark(
  test_cprod_arma(X, p),
  test_cprod_loop(X, p),
  test_cprod_eigen(X, p),
  test_cprod_eigen2(X, p)
)

stopifnot(all.equal(test_cprod_arma_inv(X, p), solve(test_cprod_loop(X, p))))
stopifnot(all.equal(test_cprod_eigen_inv(X, p), solve(test_cprod_loop(X, p))))
# stopifnot(all.equal(test_cprod_eigen_inv2(X, p), solve(test_cprod_loop(X, p))))
microbenchmark::microbenchmark(
  test_cprod_arma_inv(X, p),
  solve(test_cprod_loop(X, p)),
  test_cprod_eigen_inv(X, p),
  test_cprod_eigen_inv2(X, p)
)
*/
