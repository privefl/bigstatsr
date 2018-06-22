#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test_acc(int n, int p, int K) {

  NumericMatrix X(n, p);
  for (int j = 0; j < p; j++)
    for (int k = 0; k < K; k++)
      X(0, j) = norm_rand();
}

// [[Rcpp::export]]
void test_sp_acc(int n, int p, int K) {

  arma::sp_mat X(n, p);
  for (int j = 0; j < p; j++)
    for (int k = 0; k < K; k++)
      X(0, j) = norm_rand();
}


/*** R
microbenchmark::microbenchmark(
  test_acc(1e3, 2e3, 200),
  test_sp_acc(1e3, 2e3, 200)
)
### RcppArmadillo v0.8.500.0 & g++ v5.4.0
# Unit: milliseconds
#                         expr      min       lq     mean   median       uq       max neval
#    test_acc(1000, 2000, 200) 16.43358 17.02553 19.13284 17.81334 18.15556 121.57572   100
# test_sp_acc(1000, 2000, 200) 24.13308 24.80901 25.53917 24.98361 25.24921  53.59381   100
*/
