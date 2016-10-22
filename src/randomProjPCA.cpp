// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>
#include "utils.h"

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
void scaled(SEXP pBigMat,
            const NumericVector& mean,
            const NumericVector& sd) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> macc(*xpMat);

  int n = xpMat->nrow();
  int m = xpMat->ncol();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      macc[j][i] -= mean[j];
      macc[j][i] /= sd[j];
    }
  }

  return;
}

/******************************************************************************/

// [[Rcpp::export]]
Eigen::MatrixXd multEigen(const Eigen::Map<Eigen::MatrixXd> X,
                          const Eigen::Map<Eigen::MatrixXd> Y) {
  return X * Y;
}

/******************************************************************************/

// [[Rcpp::export]]
Eigen::MatrixXd crossprodEigen5(const Eigen::Map<Eigen::MatrixXd> X,
                                const Eigen::Map<Eigen::MatrixXd> Y) {
  return X.adjoint() * Y;
}

/******************************************************************************/
