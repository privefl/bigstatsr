#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix test_acc(int n, int p, int K) {

  NumericMatrix X(n, p);
  for (int k = 0; k < K; k++)
    for (int j = 0; j < p; j++)
      X(0, j) = norm_rand();

  return X;
}

// [[Rcpp::export]]
arma::sp_mat test_sp_acc(int n, int p, int K) {

  arma::sp_mat X(n, p);
  for (int k = 0; k < K; k++)
    for (int j = 0; j < p; j++)
      X(0, j) = norm_rand();

  return X;
}


/*** R
library(Matrix)
microbenchmark::microbenchmark(
  test_acc(100, 2e4, 20),
  test_sp_acc(100, 2e4, 20)
)
### RcppArmadillo v0.8.500.0 & g++ v5.4.0
# Unit: milliseconds
#                        expr      min       lq     mean   median       uq      max neval
#    test_acc(100, 2e+05, 20) 165.3366 167.2213 171.3717 168.4456 169.0396 255.5297   100
# test_sp_acc(100, 2e+05, 20) 537.5438 549.5372 554.5604 554.5927 560.0503 572.4653   100

### RcppArmadillo v0.7.900.2.0 & g++ v5.4.0
# Unit: milliseconds
#                          expr      min       lq     mean   median       uq      max neval
#    test_acc(1000, 2000, 2000) 144.7021 146.1826 147.7217 147.2922 147.9563 201.3577   100
# test_sp_acc(1000, 2000, 2000) 156.2088 158.1766 159.3457 159.3889 160.0716 166.6135   100

### RcppArmadillo v0.7.900.2.0 & g++ v5.4.0
# Unit: milliseconds
#                        expr       min         lq       mean     median         uq        max
#    test_acc(100, 2e+05, 20)   159.599   159.8301   164.3388   159.9664   161.7223   259.8329
# test_sp_acc(100, 2e+05, 20) 10830.379 10843.0464 10937.8474 10856.2121 10881.6838 11523.5797
*/
