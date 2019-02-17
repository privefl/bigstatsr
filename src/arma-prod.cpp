/******************************************************************************/

#include <bigstatsr/FBM.h>

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
arma::mat crossprod_FBM(Rcpp::Environment BM) {
  arma::mat fbm = FBM2arma(BM);
  return fbm.t() * fbm;
}

// [[Rcpp::export]]
arma::mat crossprod_FBM_mat(Rcpp::Environment BM, const arma::mat& A) {
  return FBM2arma(BM).t() * A;
}

// [[Rcpp::export]]
arma::mat crossprod_mat_FBM(const arma::mat& A, Rcpp::Environment BM) {
  return A.t() * FBM2arma(BM);
}

/******************************************************************************/

// [[Rcpp::export]]
arma::mat tcrossprod_FBM(Rcpp::Environment BM) {
  arma::mat fbm = FBM2arma(BM);
  return fbm * fbm.t();
}

// [[Rcpp::export]]
arma::mat tcrossprod_FBM_mat(Rcpp::Environment BM, const arma::mat& A) {
  return FBM2arma(BM) * A.t();
}

// [[Rcpp::export]]
arma::mat tcrossprod_mat_FBM(const arma::mat& A, Rcpp::Environment BM) {
  return A * FBM2arma(BM).t();
}

/******************************************************************************/
