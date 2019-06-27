/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// // [[Rcpp::export]]
// double auc_cpp(const NumericVector& x_pos, const NumericVector& x_neg) {
//
//   size_t n_pos = x_pos.size();
//   size_t n_neg = x_neg.size();
//   if (n_pos == 0 || n_neg == 0) return NA_REAL;
//
//   double cg = 0, ce = 0;
//   size_t i, j;
//
//   for (j = 0; j < n_pos; j++) {
//     for (i = 0; i < n_neg; i++) {
//       if (x_pos[j] > x_neg[i]) {
//         cg++;
//       } else if (x_pos[j] == x_neg[i]) {
//         ce++;
//       }
//     }
//   }
//
//   double res = cg + ce / 2;
//   res /= n_neg;
//   res /= n_pos;
//
//   return res;
// }

/******************************************************************************/

// x and y must be sorted with respect to the initial x AND y
// [[Rcpp::export]]
double auc_sorted(const NumericVector& x,
                  const LogicalVector& y) {

  int n = y.length();
  double latest_control = R_NegInf;
  double count_control = 0, count_control_equal = 0;
  double add, total = 0;

  for (int i = 0; i < n; i++) {
    if (y[i]) {  // case
      add = count_control;
      if (x[i] == latest_control) add -= (count_control_equal + 1) / 2;
      total += add;
    } else {     // control
      count_control++;
      if (x[i] == latest_control) {
        count_control_equal++;
      } else {
        latest_control = x[i];
        count_control_equal = 0;
      }
    }
  }

  return total / (count_control * (n - count_control));
}

/******************************************************************************/

// x and y must be sorted with respect to the initial x AND y
// w count the number of time each index is used in the boostrap sample
// [[Rcpp::export]]
double auc_sorted_tab(const NumericVector& x,
                      const LogicalVector& y,
                      const IntegerVector& w) {

  int n = y.length();
  double latest_control = R_NegInf;
  double count_control = 0, count_control_equal = 0;
  double add, total = 0;

  for (int i = 0; i < n; i++) {
    if (w[i]) {
      if (y[i]) {  // case
        add = count_control;
        if (x[i] == latest_control) add -= (count_control_equal + 1) / 2;
        total += w[i] * add;
      } else {     // control
        count_control += w[i];
        if (x[i] == latest_control) {
          count_control_equal += w[i];
        } else {
          latest_control = x[i];
          count_control_equal = 0;
        }
      }
    }
  }

  return total / (count_control * (n - count_control));
}

/******************************************************************************/

// [[Rcpp::depends(dqrng, sitmo)]]
#include <dqrng_generator.h>
#include <convert_seed.h>
#include <R_randgen.h>

// repl <- replicate(nboot, {
//   w <- tabulate(sample(n, replace = TRUE), n)
//   auc_sorted_tab(pred, y, w)
// })
// x and y must be sorted with respect to the initial x AND y
// [[Rcpp::export]]
NumericVector boot_auc_sorted_tab(const NumericVector& x,
                                  const LogicalVector& y,
                                  int n_boot) {

  int n = y.length();
  IntegerVector tab(n);
  NumericVector res(n_boot);

  uint32_t u_n = n;
  IntegerVector seed(2, dqrng::R_random_int);
  auto rng = dqrng::generator(dqrng::convert_seed<uint64_t>(seed));

  for (int j = 0; j < n_boot; j++) {
    // clear
    for (int i = 0; i < n; i++) tab[i] = 0;
    // fill with counts of bootstrap
    for (int i = 0; i < n; i++) {
      uint32_t k = (*rng)(u_n);
      tab[k]++;
    }
    // compute AUC for bootstrap sample
    res[j] = auc_sorted_tab(x, y, tab);
  }

  return res;
}

/******************************************************************************/
