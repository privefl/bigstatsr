// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

/* Brand, M. (2003, May).
 * Fast online svd revisions for lightweight recommender systems.
 * In Proceedings of the 2003 SIAM International Conference on Data Mining
 * (pp. 37-46). Society for Industrial and Applied Mathematics.
 */
template <typename T2>
ListOf<NumericVector> univRegLin4(MatrixAccessor<T2> macc,
                                  const arma::vec &y,
                                  const List &covar_svd,
                                  const IntegerVector& rowInd) {
  // get SVD of covar
  arma::vec d = covar_svd["d"];
  arma::mat U = covar_svd["u"];
  arma::mat V = covar_svd["v"];

  int n = rowInd.size();
  int m = macc.ncol();
  int K = d.size();
  int i, j, k;

  arma::vec d2(K+1);
  arma::mat U2(K+1, K+1);
  arma::mat V2(K+1, K+1);

  // arma::mat Ut = U.t();
  arma::vec pUty(K+1);
  pUty.subvec(1, K) = U.t() * y;
  // Rcout << pUty << endl;
  arma::vec z(K+1), z2(K+1), z3(n);
  double pty, p_norm, w, w_norm, beta, tmp, eps, eps_norm;

  arma::vec a(n);
  arma::vec mv(K);
  arma::vec p(n);
  // eq. (1.8)
  arma::mat T(K+1, K+1, arma::fill::zeros);
  for (k = 0; k < K; k++) {
    T(k+1, k+1) = d[k];
  }
  // Rcout << T << endl;

  // indices begin at 1 in R and 0 in C++
  IntegerVector trains = rowInd - 1;

  NumericVector res(m);
  NumericVector res2(m);

  for (j = 0; j < m; j++) {
    // get a from jth column of X
    for (i = 0; i < n; i++) {
      a[i] = macc[j][trains[i]];
    }

    mv = U.t() * a;
    // Rcout << mv << endl;
    p = a - U * mv;
    pty = p_norm = 0;
    for (i = 0; i < n; i++) {
      pty += p[i] * y[i];
      p_norm += p[i] * p[i];
    }
    p_norm = sqrt(p_norm);
    // Rcout << p_norm << endl;
    pUty[0] = pty / p_norm;
    // Rcout << pUty << endl;

    // eq. (1.8)
    T(0, 0) = p_norm;
    for (k = 0; k < K; k++) {
      T(k+1, 0) = mv[k];
    }
    // Rcout << T << endl;
    arma::svd(U2, d2, V2, T); // "dc" seems as fast
    // printf("address: %p\n", &U2);

    z = U2.t() * pUty;
    // Rcout << z << endl;
    z2 = U2 * z;
    // printf("addresses: %p - %p\n", &z, &z2);
    w_norm = beta = 0;
    for (k = 0; k < (K+1); k++) {
      w = V2(0, k) / d2[k];
      // Rcout << w << endl;
      w_norm += w * w;
      beta += w * z[k];
    }
    // Rcout << w_norm << endl;

    // compute eps
    tmp = z2[0] / p_norm;
    z3 = U * z2.subvec(1, K);
    eps_norm = 0;
    for (i = 0; i < n; i++) {
      eps = y[i] - (p[i] * tmp + z3[i]);
      eps_norm += eps * eps;
    }
    // Rcout << eps_norm << endl;

    res[j] = beta;
    res2[j] = sqrt(eps_norm * w_norm / (n - K - 1));
  }

  return(List::create(_["estim"] = res,
                      _["std.err"] = res2));
}

/******************************************************************************/

// Dispatch function for univRegLin4
// [[Rcpp::export]]
ListOf<NumericVector> univRegLin4(XPtr<BigMatrix> xpMat,
                                  const arma::vec &y,
                                  const List &covar_svd,
                                  const IntegerVector &rowInd) {

  switch(xpMat->matrix_type()) {
  case 1:
    return univRegLin4(MatrixAccessor<char>(*xpMat),   y, covar_svd, rowInd);
  case 2:
    return univRegLin4(MatrixAccessor<short>(*xpMat),  y, covar_svd, rowInd);
  case 4:
    return univRegLin4(MatrixAccessor<int>(*xpMat),    y, covar_svd, rowInd);
  case 6:
    return univRegLin4(MatrixAccessor<float>(*xpMat),  y, covar_svd, rowInd);
  case 8:
    return univRegLin4(MatrixAccessor<double>(*xpMat), y, covar_svd, rowInd);
  default:
    throw Rcpp::exception("unknown type detected for big.matrix object!");
  }
}

/******************************************************************************/
