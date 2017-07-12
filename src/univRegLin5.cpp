/******************************************************************************/

#include <RcppArmadillo.h>
#include "../inst/include/bigstatsr/SubMatAcc.h"

/******************************************************************************/

inline arma::vec UUty(const arma::mat& U, const arma::vec& y) {
  // printf("address: %p\n", &Ut);
  return U * (U.t() * y);
}

/******************************************************************************/

template <class C>
List univLinReg5(C macc, const arma::mat& U, const arma::vec& y) {
  int n = macc.nrow();
  int m = macc.ncol();
  int K = U.n_cols;
  myassert(U.n_rows == n, ERROR_DIM);
  myassert(y.n_elem == n, ERROR_DIM);

  arma::vec x(n), x2(n);
  arma::vec y2 = y - UUty(U, y);
  double y2_sumSq = dot(y2, y2);
  double beta, x_tmp, x_diff, x2_sumSq, beta_num, beta_deno, RSS;
  int i, j;

  NumericVector betas(m), var(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      x[i] = macc(i, j);
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
    betas[j] = beta;
    var[j] = RSS / (beta_deno * (n - K - 1));
  }

  return List::create(_["estim"] = betas,
                      _["std.err"] = sqrt(var));
}

/******************************************************************************/

// Dispatch function for univLinReg5
// [[Rcpp::export]]
List univLinReg5(const S4& BM,
                 const arma::mat& covar_U,
                 const arma::vec& y,
                 const IntegerVector& rowInd,
                 const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return univLinReg5(RawSubMatAcc(*xpMat, rows, cols, BM.slot("code")),
                       covar_U, y);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return univLinReg5(SubMatAcc<char>(*xpMat,   rows, cols), covar_U, y);
    case 2:
      return univLinReg5(SubMatAcc<short>(*xpMat,  rows, cols), covar_U, y);
    case 4:
      return univLinReg5(SubMatAcc<int>(*xpMat,    rows, cols), covar_U, y);
    case 6:
      return univLinReg5(SubMatAcc<float>(*xpMat,  rows, cols), covar_U, y);
    case 8:
      return univLinReg5(SubMatAcc<double>(*xpMat, rows, cols), covar_U, y);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
