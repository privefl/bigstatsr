#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::sp_mat testSpArma() {

  arma::sp_mat beta = arma::sp_mat(2, 3);

  beta(0, 0) = 1;

  return beta;
}

/*** R
library(Matrix)
test <- testSpArma()
test
*/
