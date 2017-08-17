#ifndef BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED

/******************************************************************************/

#include <time.h>
#include <Rcpp.h>

using namespace Rcpp;

/******************************************************************************/

namespace bigstatsr { namespace biglassoUtils {

void print_time(bool verbose, bool end = false, int l = -1) {
  if (verbose) {
    char buff[100];
    time_t now = time(0);
    strftime(buff, 100, "%Y-%m-%d %H:%M:%S", localtime(&now));
    if (l < 0) {
      if (end) {
        Rprintf("Preprocessing end: %s\n", buff);
        Rprintf("\n-----------------------------------------------\n");
      } else {
        Rprintf("\nPreprocessing start: %s\n", buff);
      }
    } else {
      Rprintf("Lambda %d. Now time: %s\n", l, buff);
    }
  }
}

// standardize
template <class C>
void COPY_standardize_and_get_residual(NumericVector& center,
                                       NumericVector& scale,
                                       size_t * p_keep_ptr,
                                       std::vector<size_t>& col_idx, //columns to keep, removing columns whose scale < 1e-6
                                       std::vector<double>& z,
                                       double * lambda_max_ptr,
                                       C xAcc,
                                       const NumericVector& y,
                                       double lambda_min,
                                       double alpha,
                                       size_t n, size_t p) {
  double tmp, sum_xy;
  double zmax = 0.0, zj = 0.0;
  size_t i, j;

  double sum_y = Rcpp::sum(y);

  for (j = 0; j < p; j++) {
    sum_xy = 0.0;

    for (i = 0; i < n; i++) {
      tmp = xAcc(i, j);
      center[j] += tmp;
      scale[j] += tmp * tmp;
      sum_xy += tmp * y[i];
    }

    center[j] /= n; //center
    scale[j] = sqrt(scale[j] / n - pow(center[j], 2)); //scale

    if (scale[j] > 1e-6) {
      col_idx.push_back(j);
      zj = (sum_xy - center[j] * sum_y) / (scale[j] * n); //residual
      if (fabs(zj) > zmax) {
        zmax = fabs(zj);
      }
      z.push_back(zj);
    }
  }
  *p_keep_ptr = col_idx.size();
  *lambda_max_ptr = zmax / alpha;
}

inline double COPY_lasso(double z, double l1, double l2, double v) {
  double s=0;
  if (z > 0) s = 1;
  else if (z < 0) s = -1;
  if (fabs(z) <= l1) return(0);
  else return(s*(fabs(z)-l1)/(v*(1+l2)));
}

/******************************************************************************/

// check KKT conditions over features in the strong set
template <class C>
size_t COPY_check_strong_set(LogicalVector& in_A,
                             const LogicalVector& in_S,
                             std::vector<double>& z,
                             C xAcc,
                             const NumericVector& beta_old,
                             const std::vector<size_t>& col_idx,
                             const NumericVector& center,
                             const NumericVector& scale,
                             double lambda, double sumResid, double alpha,
                             const NumericVector& r,
                             const NumericVector& m,
                             size_t n, size_t p) {
  double sum, l1, l2;
  size_t i, j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_A[j] && in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += xAcc(i, jj) * r[i];
      }
      z[j] = (sum - center[jj] * sumResid) / (scale[jj] * n);

      l1 = lambda * m[jj] * alpha;
      l2 = lambda * m[jj] * (1 - alpha);
      if(fabs(z[j] - beta_old[j] * l2) > l1) {
        in_A[j] = true;
        violations++;
      }
    }
  }
  return violations;
}

// check KKT conditions over features in the rest set
template <class C>
size_t COPY_check_rest_set(LogicalVector& in_A,
                           LogicalVector& in_S,
                           std::vector<double>& z,
                           C xAcc,
                           const NumericVector& beta_old,
                           const std::vector<size_t>& col_idx,
                           const NumericVector& center,
                           const NumericVector& scale,
                           double lambda, double sumResid, double alpha,
                           const NumericVector& r,
                           const NumericVector& m,
                           size_t n, size_t p) {
  double sum, l1, l2;
  size_t i, j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += xAcc(i, jj) * r[i];
      }
      z[j] = (sum - center[jj] * sumResid) / (scale[jj] * n);

      l1 = lambda * m[jj] * alpha;
      l2 = lambda * m[jj] * (1 - alpha);
      if(fabs(z[j] - beta_old[j] * l2) > l1) {
        in_A[j] = in_S[j] = true;
        violations++;
      }
    }
  }
  return violations;
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_UTILS_HPP_INCLUDED
