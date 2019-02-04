/******************************************************************************/

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// [[Rcpp::export]]
List BFGS(const NumericMatrix& macc,
          arma::mat& covar,
          const arma::vec& y,
          double tol = 1e-5,
          size_t maxiter = 100) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = covar.n_cols;
  // myassert(covar.n_rows == n, ERROR_DIM);
  // myassert(y.n_elem == n, ERROR_DIM);

  arma::mat Hm1(K, K);
  arma::vec Xu(n), Xw(n), s1(n), a(n), p(n);
  arma::vec g_old(K), g_new(K), dg(K), u(K), w_new(K), w_old(K), dw(K), Hm1_dg(K);
  double diff, dwdg, b;
  size_t i, j, c;

  arma::mat X = covar.each_col() % y;

  NumericVector beta(m), var(m);
  IntegerVector niter(m);

  for (j = 0; j < m; j++) {

    for (i = 0; i < n; i++) {
      X(i, 0) = macc(i, j) * y[i];
    }

    try {

      // adapted from https://github.com/tminka/logreg/blob/master/train_bfgs.m
      Hm1.eye();
      w_new.zeros();
      c = 0;
      Xw = X * w_new;
      s1 = 1 / (1 + exp(Xw));
      Rcout << "s1: " << s1 << std::endl;
      g_old = g_new; g_new = X.t() * s1;
      Rcout << "g_new: " << g_new << std::endl;

      do {
        c++;
        // update w
        u = -Hm1 * g_new;
        Xu = X * u;
        a = s1 % (1 - s1);
        dw = (dot(g_new, u) / dot(a, Xu % Xu)) * u;
        w_old = w_new; w_new += dw;
        Rcout << "w_new: " << w_new << std::endl;
        // update g
        Xw = X * w_new;
        s1 = 1 / (1 + exp(Xw));
        Rcout << "s1: " << s1 << std::endl;
        g_old = g_new; g_new = X.t() * s1;
        Rcout << "g_new: " << g_new << std::endl;
        // update H^{-1}
        dg = g_new - g_old;
        dwdg = dot(dw, dg);
        Hm1_dg = Hm1 * dg;
        b = 1 + dot(dg, Hm1_dg) / dwdg;
        for (int k2 = 0; k2 < K; k2++) {
          for (int k1 = 0; k1 < K; k1++) {
            Hm1(k1, k2) += (b * dw[k1] * dw[k2] -
              Hm1_dg[k2] * dw[k1] - Hm1_dg[k1] * dw[k2]) / dwdg;
          }
        }
        Rcout << "Hm1: " << Hm1 << std::endl;
        // check for convergence
        diff = max(abs(dw) / (abs(dw) + abs(w_new)));
      } while (diff > tol && c < maxiter);

      niter[j] = c;
      beta[j] = w_new[0];
      var[j] = fabs(Hm1(0, 0));

    } catch (...) {
      beta[j] = NA_REAL;
      var[j]  = NA_REAL;
    }
  }

  return List::create(_["estim"]   = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"]   = niter);
}

/******************************************************************************/

/*** R
library(bigstatsr)
X <- big_attachExtdata()
y <- sample(0:1, nrow(X), TRUE)
covar <- cbind(X[, 1], 1, matrix(rnorm(nrow(X) * 3), ncol = 3))
BFGS(X[, 1, drop = FALSE], covar, 2 * y - 1)
summary(glm(y ~ covar - 1, family = "binomial"))$coef[1, ]
# => std dev not precise enough
*/
