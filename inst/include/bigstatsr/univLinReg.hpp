#ifndef BIGSTATSR_UNIV_LIN_HPP_INCLUDED
#define BIGSTATSR_UNIV_LIN_HPP_INCLUDED

/******************************************************************************/

#include <bigstatsr/arma-strict-R-headers.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr {

// Based on equations (4.4), (4.5) and (4.6) from Sikorska, K. (2014).
// Computationally fast approaches to genome-wide association studies.

template <class C>
ListOf<NumericVector> univLinReg5(C macc,
                                  const arma::mat& U,
                                  const arma::vec& y) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = U.n_cols;
  myassert_size(U.n_rows, n);
  myassert_size(y.n_elem, n);

  arma::vec x(n), x2(K);
  arma::vec y2 = y - U * (U.t() * y);
  double y2_sumSq = dot(y2, y2);
  double beta, beta_num, beta_deno, RSS;
  size_t i, j;

  NumericVector betas(m), var(m);

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      x[i] = macc(i, j);
    }
    x2 = U.t() * x;

    beta_num = beta_deno = 0;
    for (i = 0; i < n; i++) {
      beta_num  += x[i] * y2[i];
      beta_deno += x[i] * x[i];
    }
    beta_deno -= dot(x2, x2);
    beta = beta_num / beta_deno;
    RSS = y2_sumSq - beta_num * beta ;
    betas[j] = beta;
    var[j] = RSS / (beta_deno * (n - K - 1));
  }

  return List::create(_["estim"] = betas,
                      _["std.err"] = sqrt(var));
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_UNIV_LIN_HPP_INCLUDED
