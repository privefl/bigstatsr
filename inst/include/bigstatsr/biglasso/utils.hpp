#ifndef BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED

/******************************************************************************/

#include <RcppArmadillo.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr { namespace biglassoUtils {

// summaries
template <class C>
NumericVector get_summaries(C macc,
                            const NumericVector& y,
                            const IntegerVector& which_set,
                            int K) {

  int n = macc.nrow();
  int m = macc.ncol();

  arma::cube cubeArray(K, m, 3, arma::fill::zeros);

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      double x = macc(i, j);
      int k = which_set[i];
      cubeArray(k, j, 0) += x;
      cubeArray(k, j, 1) += x * x;
      cubeArray(k, j, 2) += x * y[i];
    }
  }

  return wrap(cubeArray);
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

inline double COPY_lasso(double z, double l1, double l2, double v) {
  double s = 0;
  if (z > 0) s = 1;
  else if (z < 0) s = -1;
  if (fabs(z) <= l1) return(0);
  else return(s * (fabs(z) - l1) / (v * (1 + l2)));
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
                             const NumericVector& beta_old,
                             double lambda,
                             double sumResid,
                             double alpha,
                             const NumericVector& r,
                             size_t n, size_t p) {

  double sum, l1, l2;
  size_t i, j, violations = 0;

  for (j = 0; j < p; j++) {
    if (in_S[j] && !in_A[j]) {
      sum = 0;
      for (i = 0; i < n; i++) {
        sum += macc(i, j) * r[i];
      }
      z[j] = (sum - center[j] * sumResid) / (scale[j] * n);

      l1 = lambda * alpha;
      l2 = lambda - l1;
      if (fabs(z[j] - beta_old[j] * l2) > l1) {
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
