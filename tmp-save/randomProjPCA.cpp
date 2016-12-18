// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>
#include "utils.h"

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
void scaled(SEXP pBigMat2,
            SEXP pBigMat,
            const NumericVector& mean,
            const NumericVector& sd) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<char> macc(*xpMat);
  XPtr<BigMatrix> xpMat2(pBigMat2);
  MatrixAccessor<double> macc2(*xpMat2);

  int n = xpMat->nrow();
  int m = xpMat->ncol();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      macc2[j][i] = macc[j][i];
      macc2[j][i] -= mean[j];
      macc2[j][i] /= sd[j];
    }
  }

  return;
}

/******************************************************************************/
