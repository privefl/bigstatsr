#include "../inst/include/bigstatsr.h"

// [[Rcpp::depends(bigmemory, BH, RcppArmadillo)]]
#include <RcppArmadillo.h> // Sys.setenv("PKG_LIBS" = "-llapack")
#include <bigmemory/MatrixAccessor.hpp>
using namespace Rcpp;

/******************************************************************************/

inline arma::vec y_UUty(const arma::mat& U, const arma::vec& y) {
  // printf("address: %p\n", &Ut);
  return y - U * (U.t() * y);
}

/******************************************************************************/

template <typename T>
List univRegLin5(MatrixAccessor<T> macc,
                 const arma::mat& U,
                 const arma::vec& y,
                 const IntegerVector& rowInd) {
  int n = rowInd.size();
  int m = macc.ncol();
  int K = U.n_cols;
  arma::vec x2, eps;
  arma::vec x(n);
  arma::vec y2 = y_UUty(U, y); // (4.5)
  double d, beta;
  int i, j;

  // indices begin at 1 in R and 0 in C++
  IntegerVector trains = rowInd - 1;

  NumericVector res(m);
  NumericVector var(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      x(i) = macc[j][trains[i]];
    }
    x2 = y_UUty(U, x); // (4.4)
    d = 1 / dot(x, x2);
    beta = d * dot(x, y2);
    eps = y2 - beta * x2;
    res[j] = beta;
    var[j] = d * dot(eps, eps) / (n - K - 1);
  }

  return(List::create(_["estim"] = res,
                      _["std.err"] = sqrt(var)));
}

/******************************************************************************/

// Dispatch function for univRegLin5
// [[Rcpp::export]]
List univRegLin5(XPtr<BigMatrix> xpMat,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd) {

  switch(xpMat->matrix_type()) {
  case 1:
    return univRegLin5(MatrixAccessor<char>(*xpMat),   covar_U, y, rowInd);
  case 2:
    return univRegLin5(MatrixAccessor<short>(*xpMat),  covar_U, y, rowInd);
  case 4:
    return univRegLin5(MatrixAccessor<int>(*xpMat),    covar_U, y, rowInd);
  case 6:
    return univRegLin5(MatrixAccessor<float>(*xpMat),  covar_U, y, rowInd);
  case 8:
    return univRegLin5(MatrixAccessor<double>(*xpMat), covar_U, y, rowInd);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
