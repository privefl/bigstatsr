#ifndef BIGSTATSR_COLSTATS_HPP_INCLUDED
#define BIGSTATSR_COLSTATS_HPP_INCLUDED

/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr {

template <class C>
ListOf<NumericVector> bigcolvars(C macc, int ncores = 1) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();

  NumericVector res(m), res2(m);

  int chunk_size = ceil(m / (10.0 * ncores));

  #pragma omp parallel for num_threads(ncores) schedule(dynamic, chunk_size)
  for (size_t j = 0; j < m; j++) {

    double xSum = 0, xxSum = 0;

    for (size_t i = 0; i < n; i++) {
      double x = macc(i, j);
      xSum  += x;
      xxSum += x*x;
    }

    res[j] = xxSum - xSum * xSum / n;
    res2[j] = xSum;
  }

  return List::create(_["sum"] = res2,
                      _["var"] = res/(n-1));
}

}

/******************************************************************************/

#endif // #ifndef BIGSTATSR_COLSTATS_HPP_INCLUDED
