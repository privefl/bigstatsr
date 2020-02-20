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

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl2(Environment fbm,
                            const arma::mat& y,
                            const IntegerVector& rowInd,
                            const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  BMAcc<int> macc(xpBM);

  arma::Mat<double> x(rowInd.size(), colInd.size());
  macc.extract_submat(x, rowInd, colInd, 1);

  return x * y;
}

arma::urowvec vec_int_to_arma(const IntegerVector& vec_ind) {
  int n = vec_ind.size();
  arma::urowvec res(n);
  for (int i = 0; i < n; i++) res[i] = vec_ind[i] - 1;
  return res;
}

// [[Rcpp::export]]
arma::mat mult_sub_int_dbl_arma(Environment fbm,
                                const arma::mat& y,
                                const IntegerVector& rowInd,
                                const IntegerVector& colInd) {

  XPtr<FBM> xpBM = fbm["address"];
  arma::Mat<int> x((int*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);

  return x.submat(vec_int_to_arma(rowInd), vec_int_to_arma(colInd)) * y;
}


/*** R
A <- matrix(1:4, 2)
B <- matrix(rnorm(20), 2)
mult_int_dbl(A, B)
A %*% B

C <- A; storage.mode(C) <- "raw"
mult_raw_dbl(C, B)
# C %*% B  # error

X <- bigstatsr::FBM(10, 20, type = "integer", init = 1:200)
mult_sub_int_dbl(X, B, 1:5, 1:2)
mult_sub_int_dbl2(X, B, 1:5, 1:2)
X[1:5, 1:2] %*% B
mult_sub_int_dbl_arma(X, B, 1:5, 1:2)
*/
