// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH, bigstatsr, RcppEigen)]]
#include <RcppEigen.h>
#include <bigstatsr/BMAcc.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test(const Eigen::Map<Eigen::MatrixXd> X, Environment fbm) {
  XPtr<FBM> xpMat = fbm["address"];
  BMAcc<double> macc(xpMat);
  macc(1, 4) = X(4, 1);
}

/*** R
A <- bigstatsr::FBM(5,5)
X <- matrix(1,5,5)
X[5, 2] <- 2
X[]
test(X = X, fbm = A)
A[]
*/
