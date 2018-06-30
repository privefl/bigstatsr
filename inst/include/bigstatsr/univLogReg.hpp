#ifndef BIGSTATSR_UNIV_LOG_HPP_INCLUDED
#define BIGSTATSR_UNIV_LOG_HPP_INCLUDED

/******************************************************************************/

#include <RcppArmadillo.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr {

template <class C>
List IRLS(C macc,
          arma::mat& covar,
          const arma::vec& y,
          const arma::vec& z0,
          const arma::vec& w0,
          double tol,
          size_t maxiter) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = covar.n_cols;
  myassert(covar.n_rows == n, ERROR_DIM);
  myassert(y.n_elem == n, ERROR_DIM);

  arma::mat cprod(K, K), cprod_inv(K, K);
  arma::vec Xb(n), p(n), w(n), coeffs(K), shift(K);
  double diff;
  size_t i, j, c;

  NumericVector beta(m), var(m);
  IntegerVector niter(m);

  for (j = 0; j < m; j++) {

    for (i = 0; i < n; i++) {
      covar(i, 0) = macc(i, j);
    }

    c = 1;
    coeffs = solve(covar.t() * (covar.each_col() % w0), covar.t() * z0);

    do {
      c++;

      Xb = covar * coeffs;
      p = 1 / (1 + exp(-Xb));
      w = p % (1 - p);

      cprod = covar.t() * (covar.each_col() % w);
      shift = solve(cprod, covar.t() * (y - p));
      coeffs += shift;

      diff = max(abs(shift) / (abs(shift) + abs(coeffs)));
    } while (diff > tol && c < maxiter);

    beta[j] = coeffs(0);
    cprod_inv = inv(cprod);
    var[j] = cprod_inv(0, 0);
    niter[j] = c;
  }

  return List::create(_["estim"]   = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"]   = niter);
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_UNIV_LOG_HPP_INCLUDED
