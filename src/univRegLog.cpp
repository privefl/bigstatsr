/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

arma::mat& getXtW(const arma::mat& covar,
                  const arma::vec& w,
                  arma::mat& tcovar,
                  int n, int K) {
  int i, k;

  for (i = 0; i < n; i++) {
    for (k = 0; k < K; k++) {
      tcovar(k, i) = covar(i, k) * w(i);
    }
  }

  return tcovar;
}

/******************************************************************************/

template <class C>
List IRLS(C macc,
          arma::mat &covar,
          const arma::vec &y,
          const arma::vec &z0,
          const arma::vec &w0,
          double tol,
          int maxiter) {

  int n = macc.nrow();
  int m = macc.ncol();
  int K = covar.n_cols;
  myassert(covar.n_rows == n, ERROR_DIM);
  myassert(y.n_elem == n, ERROR_DIM);

  arma::mat tcovar(K, n), tmp(K, K);
  arma::vec Xb(n), p(n), w(n), z(n), betas_old(K), betas_new(K);
  double diff;
  int i, j, c;

  NumericVector beta(m), var(m);
  IntegerVector niter(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      covar(i, 0) = macc(i, j);
    }

    c = 1;
    z = z0;
    w = w0;
    tcovar = getXtW(covar, w, tcovar, n, K);
    betas_new = solve(tcovar * covar, tcovar * z);

    do {
      c++;
      betas_old = betas_new;

      Xb = covar * betas_old;
      p = 1 / (1 + exp(-Xb));
      w = p % (1 - p);
      z = Xb + (y - p) / w;

      tcovar = getXtW(covar, w, tcovar, n, K);
      betas_new = solve(tcovar * covar, tcovar * z);

      diff = 2 * max(abs(betas_old - betas_new)
                       / (abs(betas_old) + abs(betas_new)));
    } while (diff > tol && c < maxiter);

    beta[j] = betas_new(0);
    tmp = inv(tcovar * covar);
    var[j] = tmp(0, 0);
    niter[j] = c;
  }

  return List::create(_["estim"] = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"] = niter);
}

/******************************************************************************/

// Dispatch function for IRLS
// [[Rcpp::export]]
List IRLS(const S4& BM,
          arma::mat& covar,
          const arma::vec& y,
          const arma::vec& z0,
          const arma::vec& w0,
          const IntegerVector& rowInd,
          const IntegerVector& colInd,
          double tol,
          int maxiter) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return IRLS(RawSubMatAcc(*xpMat, rows, cols, BM.slot("code")),
                covar, y, z0, w0, tol, maxiter);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return IRLS(SubMatAcc<char>(*xpMat,   rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 2:
      return IRLS(SubMatAcc<short>(*xpMat,  rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 4:
      return IRLS(SubMatAcc<int>(*xpMat,    rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 6:
      return IRLS(SubMatAcc<float>(*xpMat,  rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    case 8:
      return IRLS(SubMatAcc<double>(*xpMat, rows, cols),
                  covar, y, z0, w0, tol, maxiter);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
