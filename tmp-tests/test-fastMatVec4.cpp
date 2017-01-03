// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector prod1(XPtr<BigMatrix> bMPtr, const NumericVector& x) {

  MatrixAccessor<char> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(n);
  int i, j;

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += macc[j][i] * x[j];
    }
  }

  return res;
}

/******************************************************************************/

// [[Rcpp::export]]
arma::vec prodArma(XPtr<BigMatrix> xpA, const arma::vec& x) {

  arma::Mat<char> Am((char*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am * x;
}

/******************************************************************************/

// [[Rcpp::export]]
arma::vec prodArmaSub(XPtr<BigMatrix> xpA, const arma::vec& x,
                      const arma::Row<uint32_t>& ind) {
  arma::Mat<char> Am((char *) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am.rows(ind) * x;
}

/******************************************************************************/
