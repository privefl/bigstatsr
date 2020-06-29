#ifndef BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED

/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr { namespace biglassoUtils {

// summaries
template <class C>
ListOf<NumericMatrix> get_summaries(C macc,
                                    const NumericVector& y,
                                    const IntegerVector& which_set,
                                    int K) {

  int n = macc.nrow();
  int m = macc.ncol();

  NumericMatrix sumX(K, m), sumXX(K, m), sumXY(K, m);

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      double x = macc(i, j);
      int k = which_set[i];
      sumX (k, j) += x;
      sumXX(k, j) += x * x;
      sumXY(k, j) += x * y[i];
    }
  }

  return List::create(_["sumX"]  = sumX,
                      _["sumXX"] = sumXX,
                      _["sumXY"] = sumXY);
}

/******************************************************************************/

template <class C>
NumericVector predict(C macc,
                      const NumericVector& beta,
                      const NumericVector& center,
                      const NumericVector& scale) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  NumericVector pred(n);
  double bj, shift = 0;

  for (size_t j = 0; j < m; j++) {
    bj = beta[j] / scale[j];
    if (bj != 0) {
      for (size_t i = 0; i < n; i++) {
        pred[i] += macc(i, j) * bj;
      }
      shift += center[j] * bj;
    }
  }

  return pred - shift;
}

/******************************************************************************/

inline double COPY_lasso(double z, double l1, double l2) {
  if (z > 0) {
    double num = z - l1;
    return (num > 0) ? num / (1 + l2) : 0;
  } else {
    double num = z + l1;
    return (num < 0) ? num / (1 + l2) : 0;
  }
}

inline double COPY_lasso(double z, double l1, double l2, double v) {
  if (z > 0) {
    double num = z - l1;
    return (num > 0) ? num / (v * (1 + l2)) : 0;
  } else {
    double num = z + l1;
    return (num < 0) ? num / (v * (1 + l2)) : 0;
  }
}

/******************************************************************************/

// check KKT conditions over features in the strong set
template <class C>
size_t COPY_check_strong_set(LogicalVector& in_A,
                             const LogicalVector& in_S,
                             NumericVector& z,
                             C macc,
                             const NumericVector& center,
                             const NumericVector& scale,
                             const NumericVector& pf,
                             const NumericVector& beta_old,
                             double l1, double l2,
                             const NumericVector& r,
                             double sumResid,
                             const NumericVector& weights) {

  size_t n = macc.nrow();
  size_t p = macc.ncol();
  size_t i, j, violations = 0;

  for (j = 0; j < p; j++) {
    if (in_S[j] && !in_A[j]) {
      double cpsum = 0;
      for (i = 0; i < n; i++) {
        cpsum += macc(i, j) * r[i] * weights[i];
      }
      z[j] = (cpsum - center[j] * sumResid) / (scale[j] * n);

      if (fabs(z[j] - beta_old[j] * l2 * pf[j]) > (l1 * pf[j])) {
        in_A[j] = true;
        violations++;
      }
    }
  }

  return violations;
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED
