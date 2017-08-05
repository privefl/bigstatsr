#ifndef BIGSTATSR_COLSTATS_HPP_INCLUDED
#define BIGSTATSR_COLSTATS_HPP_INCLUDED

/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;

/******************************************************************************/

namespace bigstatsr {

template <class C>
ListOf<NumericVector> bigcolvars(C macc) {
  int n = macc.nrow();
  int m = macc.ncol();

  NumericVector res(m), res2(m);
  double x, xSum, xxSum;
  int i, j;

  for (j = 0; j < m; j++) {
    xSum = xxSum = 0;
    for (i = 0; i < n; i++) {
      x = macc(i, j);
      xSum += x;
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
