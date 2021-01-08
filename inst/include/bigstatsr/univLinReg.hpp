#ifndef BIGSTATSR_UNIV_LIN_HPP_INCLUDED
#define BIGSTATSR_UNIV_LIN_HPP_INCLUDED

/******************************************************************************/

#include <bigstatsr/arma-strict-R-headers.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr {

// Equations from Appendix A.1 of https://tel.archives-ouvertes.fr/tel-02476202

template <class C>
ListOf<NumericVector> univLinReg5(C macc,
                                  const arma::mat& U,
                                  const arma::vec& y,
                                  int ncores = 1) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  int K = U.n_cols;
  myassert_size(U.n_rows, n);
  myassert_size(y.n_elem, n);

  arma::vec y2 = y - U * (U.t() * y);
  double y2_sumSq = dot(y2, y2);

  NumericVector betas(m), var(m);

  int chunk_size = ceil(m / (10.0 * ncores));

  #pragma omp parallel num_threads(ncores)
  {
    arma::vec x2(K);

    #pragma omp for schedule(dynamic, chunk_size)
    for (size_t j = 0; j < m; j++) {

      double beta_num = 0, beta_deno = 0;
      x2.zeros();

      for (size_t i = 0; i < n; i++) {
        double x_i = macc(i, j);
        beta_num  += x_i * y2[i];
        beta_deno += x_i * x_i;
        for (int k = 0; k < K; k++) x2[k] += U(i, k) * x_i; // x2 = U.t() * x
      }

      beta_deno -= dot(x2, x2);
      double beta = beta_num / beta_deno;
      double RSS = y2_sumSq - beta_num * beta;

      betas[j] = beta;
      var[j] = RSS / (beta_deno * (n - K - 1));
    }
  }

  return List::create(_["estim"]   = betas,
                      _["std.err"] = sqrt(var));
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_UNIV_LIN_HPP_INCLUDED
