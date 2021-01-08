#ifndef BIGSTATSR_PROD_HPP_INCLUDED
#define BIGSTATSR_PROD_HPP_INCLUDED

/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;

/******************************************************************************/

namespace bigstatsr {

template <class C>
NumericVector pMatVec4(C macc, const NumericVector& x) {

  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(n);

  // #pragma omp parallel num_threads(ncores)
  // {
  //   int id = omp_get_thread_num();
  //   int n2 = n;
  //   int m = macc.ncol();
  //   int m2 = m - 3;  // WARNING: do not use std::size_t because of `m - 3`
  //   int i, j;
  //
  //   // unrolling optimization
  //   #pragma omp for nowait
  //   for (j = 0; j < m2; j += 4) {
  //     for (i = 0; i < n2; i++) {
  //       res(i, id) += (x[j] * macc(i, j) + x[j+1] * macc(i, j+1)) +
  //         (x[j+2] * macc(i, j+2) + x[j+3] * macc(i, j+3));
  //     } // The parentheses are somehow important
  //   }
  //   #pragma omp for
  //   for (j = m - m % 4; j < m; j++) {
  //     for (i = 0; i < n2; i++) {
  //       res(i, id) += x[j] * macc(i, j);
  //     }
  //   }
  // }
  int i, j;

  // WARNING: do not use std::size_t because of `m - 4`
  for (j = 0; j <= m - 4; j += 4) { // unrolling optimization
    for (i = 0; i < n; i++) {
      res[i] += (x[j] * macc(i, j) + x[j+1] * macc(i, j+1)) +
        (x[j+2] * macc(i, j+2) + x[j+3] * macc(i, j+3));
    } // The parentheses are somehow important.
  }
  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc(i, j);
    }
  }

  return res;
}

/******************************************************************************/

template <class C>
NumericVector cpMatVec4(C macc, const NumericVector& x, int ncores = 1) {

  int m = macc.ncol();
  NumericVector res(m);

  int chunk_size = ceil(m / (10.0 * ncores));

  #pragma omp parallel num_threads(ncores)
  {
    int n = macc.nrow();
    int n2 = n - 3;  // WARNING: do not use std::size_t because of `n - 3`

    #pragma omp for schedule(dynamic, chunk_size)
    for (int j = 0; j < m; j++) {

      double cp = 0;
      int i = 0;
      // unrolling optimization
      for (; i < n2; i += 4) {
        cp += (macc(i, j) * x[i] + macc(i+1, j) * x[i+1]) +
          (macc(i+2, j) * x[i+2] + macc(i+3, j) * x[i+3]);
      }
      for (; i < n; i++) cp += macc(i, j) * x[i];

      res[j] = cp;
    }
  }

  return res;
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_PROD_HPP_INCLUDED
