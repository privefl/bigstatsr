#ifndef BIGSTATSR_UNIV_LOG_HPP_INCLUDED
#define BIGSTATSR_UNIV_LOG_HPP_INCLUDED

/******************************************************************************/

#include <bigstatsr/arma-strict-R-headers.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;
using std::size_t;

#if defined(_OPENMP)
#include <omp.h>
#else
inline int omp_set_num_threads(int n) { return 0; }
#endif

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

  // make sure not to use two levels of parallelism
  omp_set_num_threads(1);

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = covar.n_cols;
  myassert_size(covar.n_rows, n);
  myassert_size(y.n_elem, n);

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

    try {

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

      niter[j] = c;
      cprod_inv = inv(cprod);
      beta[j] = coeffs(0);
      var[j] = cprod_inv(0, 0);

    } catch (...) {
      beta[j] = NA_REAL;
      var[j]  = NA_REAL;
    }
  }

  return List::create(_["estim"]   = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"]   = niter);
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_UNIV_LOG_HPP_INCLUDED
