// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <RcppArmadillo.h>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
arma::vec test(XPtr<BigMatrix> xpMat,
               const arma::vec& x,
               const arma::Row<uint32_t>& rowInd) {

  XPtr<BigMatrix> xpA(xpMat);

  // won't work with a sub.big.matrix
  arma::Mat<char> Am((char*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am.rows(rowInd-1) * x;
}

// [[Rcpp::export]]
arma::vec test2(XPtr<BigMatrix> xpMat,
               const arma::vec& x) {

  XPtr<BigMatrix> xpA(xpMat);

  // won't work with a sub.big.matrix
  arma::Mat<char> Am((char*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am * x;
}

// [[Rcpp::export]]
arma::mat test3(XPtr<BigMatrix> xpMat,
               const arma::mat& x,
               const arma::Row<uint32_t>& rowInd) {

  XPtr<BigMatrix> xpA(xpMat);

  // won't work with a sub.big.matrix
  arma::Mat<char> Am((char*) xpA->matrix(), xpA->nrow(), xpA->ncol(), false);

  return Am.rows(rowInd-1) * x;
}
