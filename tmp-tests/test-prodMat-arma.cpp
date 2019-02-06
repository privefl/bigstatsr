/******************************************************************************/

#include <RcppArmadillo.h>
#include <bigstatsr/FBM.h>
#include <bigstatsr/utils.h>

/******************************************************************************/

inline arma::mat FBM2arma(Rcpp::Environment BM) {

  Rcpp::XPtr<FBM> xpBM = BM["address"];
  myassert(xpBM->matrix_type() == 8,
           "Mapping to arma::mat is available for 'double' FBMs only.");

  return arma::mat((double*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);
}

/******************************************************************************/

// [[Rcpp::export]]
arma::mat prod_FBM_mat(Rcpp::Environment BM, const arma::mat& A) {
  return FBM2arma(BM) * A;
}

// [[Rcpp::export]]
arma::mat prod_mat_FBM(const arma::mat& A, Rcpp::Environment BM) {
  return A * FBM2arma(BM);
}

/******************************************************************************/

// [[Rcpp::export]]
arma::mat crossprod_FBM_mat(Rcpp::Environment BM, const arma::mat& A) {
  return FBM2arma(BM).t() * A;
}

// [[Rcpp::export]]
arma::mat crossprod_FBM_mat2(Rcpp::Environment BM, const arma::mat& A) {
  return (A.t() * FBM2arma(BM)).t();
}

// [[Rcpp::export]]
arma::mat crossprodprod_mat_FBM(const arma::mat& A, Rcpp::Environment BM) {
  return A.t() * FBM2arma(BM);
}

/******************************************************************************/

// [[Rcpp::export]]
arma::mat tcrossprod_FBM_mat(Rcpp::Environment BM, const arma::mat& A) {
  return FBM2arma(BM) * A.t();
}

// [[Rcpp::export]]
arma::mat tcrossprod_FBM_mat2(Rcpp::Environment BM, const arma::mat& A) {
  return (A * FBM2arma(BM).t()).t();
}

// [[Rcpp::export]]
arma::mat tcrossprodprod_mat_FBM(const arma::mat& A, Rcpp::Environment BM) {
  return A * FBM2arma(BM).t();
}

/******************************************************************************/

/*** R
library(bigstatsr)
X <- big_copy(big_attachExtdata(), type = "double")
mat <- matrix(rnorm(ncol(X) * 2), ncol = 2)
prod_FBM_mat(X, mat)[1:5, ]
mat2 <- matrix(rnorm(2 * nrow(X)), nrow = 2)
prod_mat_FBM(mat2, X)[, 1:5]
# prod_mat_FBM(1:5, X)[, 1:5]

mat3 <- t(mat2)
stopifnot(all.equal(crossprod_FBM_mat(X, mat3), crossprod_FBM_mat2(X, mat3)))
microbenchmark::microbenchmark(
  crossprod_FBM_mat(X, mat3),
  crossprod_FBM_mat2(X, mat3)
)

mat4 <- t(mat)
stopifnot(all.equal(tcrossprod_FBM_mat(X, mat4), tcrossprod_FBM_mat2(X, mat4)))
microbenchmark::microbenchmark(
  tcrossprod_FBM_mat(X, mat4),
  tcrossprod_FBM_mat2(X, mat4)
)
*/
