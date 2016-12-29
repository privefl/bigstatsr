// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <RcppArmadillo.h>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
arma::vec armaProdVec(XPtr<BigMatrix> xpMat, const arma::vec& x,
                      const arma::Row<uint32_t>& ind) {

  XPtr<BigMatrix> xpA(xpMat);

  // won't work with a sub.big.matrix
  arma::mat Am((double*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am.rows(ind) * x;
}

// [[Rcpp::export]]
arma::mat armaProdMat(XPtr<BigMatrix> xpMat, const arma::mat& x) {

  XPtr<BigMatrix> xpA(xpMat);

  // won't work with a sub.big.matrix
  arma::mat Am((double*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am * x;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericVector rcppProdVec(SEXP xpMat, const NumericVector& x) {

  XPtr<BigMatrix> bMPtr(xpMat);
  MatrixAccessor<double> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(n);
  int i, j;

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc[j][i];
    }
  }

  return res;
}
