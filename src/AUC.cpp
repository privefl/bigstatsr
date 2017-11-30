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
  double n_case = Rcpp::sum(y);
  double n_control = n - n_case;
  double latest_control = R_NegInf;
  double count_control = 0;
  double count_control_equal = 0;
  double total = 0;

  for (int i = 0; i < n; i++) {
    if (y[i]) {  // case
      if (x[i] == latest_control) {
        total += count_control - (count_control_equal + 1) / 2;
      } else {
        total += count_control;
      }
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

  return total / (n_case * n_control);
}

/******************************************************************************/
