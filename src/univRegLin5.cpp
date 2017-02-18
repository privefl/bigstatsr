#include "bigstatsr.h"

// [[Rcpp::depends(bigmemory, BH, RcppArmadillo)]]
#include <RcppArmadillo.h> // Sys.setenv("PKG_LIBS" = "-llapack")
#include <bigmemory/MatrixAccessor.hpp>
using namespace Rcpp;

/******************************************************************************/

inline arma::vec UUty(const arma::mat& U, const arma::vec& y) {
  // printf("address: %p\n", &Ut);
  return U * (U.t() * y);
}

/******************************************************************************/

template <typename T>
List univLinReg5(MatrixAccessor<T> macc,
                 const arma::mat& U,
                 const arma::vec& y,
                 const IntegerVector& rowInd) {
  int n = rowInd.size();
  int m = macc.ncol();
  int K = U.n_cols;
  arma::vec x(n), x2(n);
  arma::vec y2 = y - UUty(U, y);
  double y2_sumSq = dot(y2, y2);
  double beta, x_tmp, x_diff, x2_sumSq, beta_num, beta_deno, RSS;
  int i, j;

  // indices begin at 1 in R and 0 in C++
  IntegerVector trains = rowInd - 1;

  NumericVector res(m);
  NumericVector var(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      x[i] = macc[j][trains[i]];
    }
    x2 = UUty(U, x);

    beta_num = beta_deno = x2_sumSq = 0;
    for (i = 0; i < n; i++) {
      x_tmp = x[i];
      x_diff = x_tmp - x2[i];
      beta_num += x_tmp * y2[i];
      beta_deno += x_tmp * x_diff;
      x2_sumSq += x_diff * x_diff;
    }
    beta = beta_num / beta_deno;
    RSS = y2_sumSq - beta * beta * x2_sumSq;
    res[j] = beta;
    var[j] = RSS / (beta_deno * (n - K - 1));
  }

  return(List::create(_["estim"] = res,
                      _["std.err"] = sqrt(var)));
}

/******************************************************************************/

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(XPtr<BigMatrix> xpMat,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd) {

  switch(xpMat->matrix_type()) {
  case 1:
    return univLinReg5(MatrixAccessor<char>(*xpMat),   covar_U, y, rowInd);
  case 2:
    return univLinReg5(MatrixAccessor<short>(*xpMat),  covar_U, y, rowInd);
  case 4:
    return univLinReg5(MatrixAccessor<int>(*xpMat),    covar_U, y, rowInd);
  case 6:
    return univLinReg5(MatrixAccessor<float>(*xpMat),  covar_U, y, rowInd);
  case 8:
    return univLinReg5(MatrixAccessor<double>(*xpMat), covar_U, y, rowInd);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
