// [[Rcpp::depends(RcppEigen, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat multArma(const arma::mat& X,
                   const arma::mat& Y) {
  return X * Y;
}

// [[Rcpp::export]]
Eigen::MatrixXd multEigen(const Eigen::Map<Eigen::MatrixXd> X,
                          const Eigen::Map<Eigen::MatrixXd> Y) {
  return X * Y;
}

/*** R
X <- matrix(1, 400, 5000)
Y <- matrix(1, 5000, 2000)
all.equal(multArma(X, Y), multEigen(X, Y))
microbenchmark::microbenchmark(
  multArma(X, Y),
  multEigen(X, Y),
  times = 10
)
*/
