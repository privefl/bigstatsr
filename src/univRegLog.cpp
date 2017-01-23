// [[Rcpp::depends(bigmemory, BH, RcppArmadillo)]]
#include <RcppArmadillo.h> // Sys.setenv("PKG_LIBS" = "-llapack")
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

arma::mat& getXtW(const arma::mat& covar, const arma::vec& w,
                  arma::mat& tcovar, int n, int K) {
  int i, j;

  for (i = 0; i < n; i++) {
    for (j = 0; j < K; j++) {
      tcovar(j, i) = covar(i, j) * w(i);
    }
  }

  return tcovar;
}

/******************************************************************************/

template <typename T>
List IRLS(MatrixAccessor<T> macc,
          arma::mat &covar,
          const arma::vec &y,
          const arma::vec &z0,
          const arma::vec &w0,
          const IntegerVector &rowInd,
          double tol,
          int maxiter) {
  int n = rowInd.size();
  int m = macc.ncol();
  int K = covar.n_cols;
  arma::mat tcovar(K, n);
  arma::mat tmp;
  arma::vec p, w, z, betas_old, betas_new, Xb;
  double diff;
  int i, j, c;

  // indices begin at 1 in R and 0 in C++
  IntegerVector trains = rowInd - 1;

  NumericVector res(m);
  NumericVector var(m);
  IntegerVector conv(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      covar(i, 0) = macc[j][trains[i]];
    }
    z = z0;
    w = w0;
    tcovar = getXtW(covar, w, tcovar, n, K);
    betas_new = solve(tcovar * covar, tcovar * z);
    c = 1;

    do {
      c++;
      betas_old = betas_new;

      Xb = covar * betas_old;
      // 6 / 23
      p = 1 / (1 + exp(-Xb));
      w = p % (1 - p);
      z = Xb + (y - p) / w;

      tcovar = getXtW(covar, w, tcovar, n, K);
      betas_new = solve(tcovar * covar, tcovar * z);

      diff = 2 * max(abs(betas_old - betas_new)
                       / (abs(betas_old) + abs(betas_new)));
    } while (diff > tol && c < maxiter);

    res[j] = betas_new(0);
    tmp = inv(tcovar * covar); // 1/23 sec
    var[j] = tmp(0, 0);
    conv[j] = c;
  }

  return(List::create(_["betas"] = res,
                      _["std"] = sqrt(var),
                      _["conv"] = conv));
}

/******************************************************************************/

// Dispatch function for IRLS
// [[Rcpp::export]]
List IRLS(XPtr<BigMatrix> xpMat,
          arma::mat& covar,
          const arma::vec& y,
          const arma::vec& z0,
          const arma::vec& w0,
          const IntegerVector& rowInd,
          double tol,
          int maxiter) {

  switch(xpMat->matrix_type()) {
  case 1:
    return IRLS(MatrixAccessor<char>(*xpMat),   covar, y,
                z0, w0, rowInd, tol, maxiter);
  case 2:
    return IRLS(MatrixAccessor<short>(*xpMat),  covar, y,
                z0, w0, rowInd, tol, maxiter);
  case 4:
    return IRLS(MatrixAccessor<int>(*xpMat),    covar, y,
                z0, w0, rowInd, tol, maxiter);
  case 6:
    return IRLS(MatrixAccessor<float>(*xpMat),  covar, y,
                z0, w0, rowInd, tol, maxiter);
  case 8:
    return IRLS(MatrixAccessor<double>(*xpMat), covar, y,
                z0, w0, rowInd, tol, maxiter);
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/
