// [[Rcpp::depends(RcppArmadillo)]]

#define STRICT_R_HEADERS

#include <RcppArmadillo.h>
#include <bigstatsr/BMAcc.h>
using namespace Rcpp;


// [[Rcpp::export]]
arma::mat mult_int_dbl(const arma::Mat<int>& x, const arma::mat& y) {
  return x * y;
}

// [[Rcpp::export]]
arma::mat mult_raw_dbl(const arma::Mat<unsigned char>& x, const arma::mat& y) {
  return x * y;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl(Environment fbm,
                           const arma::mat& y,
                           const IntegerVector& rowInd,
                           const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  BMAcc<int> macc(xpBM);

  arma::Mat<int> x(rowInd.size(), colInd.size());
  macc.extract_submat(x, rowInd, colInd, 1);

  return x * y;
}

/*** R
A <- matrix(1:4, 2)
B <- matrix(rnorm(20), 2)
mult_int_dbl(A, B)
A %*% B

C <- A; storage.mode(C) <- "raw"
mult_raw_dbl(C, B)
# C %*% B  # error

X <- FBM(10, 20, type = "integer", init = 1:200)
mult_sub_int_dbl(X, B, 1:5, 1:2)
X[1:5, 1:2] %*% B
*/
