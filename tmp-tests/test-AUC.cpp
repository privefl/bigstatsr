#include <Rcpp.h>
using namespace Rcpp;

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


/*** R
x <- sample(100, size = 1e4, replace = TRUE)
y <- rep(1:0, each = 5e3)

AUC2 <- function(x, y) {
  ord <- order(x, y)
  x.ord <- x[ord]
  y.ord <- y[ord]
  auc_sorted(x.ord, y.ord)
}

microbenchmark::microbenchmark(
  bigstatsr::AUC(x, y),
  AUC2(x, y)
)
all.equal(bigstatsr::AUC(x, y), AUC2(x, y))
*/
