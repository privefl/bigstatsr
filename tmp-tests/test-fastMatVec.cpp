// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;

typedef Eigen::Matrix<char, Eigen::Dynamic, Eigen::Dynamic> MatrixXchar;

/******************************************************************************/

// [[Rcpp::export]]
Eigen::VectorXd produ2(SEXP xpMat, const Eigen::Map<Eigen::VectorXd> x) {

  XPtr<BigMatrix> bMPtr(xpMat);

  int data[] = {1,2,3,5,6,7,8,9};

  // won't work with a sub.big.matrix
  Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(),
                             bMPtr->nrow(), bMPtr->ncol());

  return bM(data, Eigen::all).cast<double>() * x;
}

// [[Rcpp::export]]
Eigen::VectorXd produ(SEXP xpMat, const Eigen::Map<Eigen::VectorXd> x) {

  XPtr<BigMatrix> bMPtr(xpMat);

  // won't work with a sub.big.matrix
  Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(),
                             bMPtr->nrow(), bMPtr->ncol());

  return bM.cast<double>() * x;
}

/******************************************************************************/

// [[Rcpp::export]]
Eigen::VectorXd crossprodu(SEXP xpMat, const Eigen::Map<Eigen::VectorXd> x) {

  XPtr<BigMatrix> bMPtr(xpMat);

  // won't work with a sub.big.matrix
  Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(),
                             bMPtr->nrow(), bMPtr->ncol());

  return bM.transpose().cast<double>() * x;
}
