// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
ListOf<NumericMatrix> testLU(arma::mat A) {
  arma::mat L, U, P;
  lu(L, U, P, A);
  return(List::create(_["L"] = L,
                      _["U"] = U,
                      _["P"] = P));
}

