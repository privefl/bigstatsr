// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector get_summaries(const NumericMatrix& macc,
                            const IntegerVector& y,
                            const IntegerVector& which_set,
                            int K) {

  int n = macc.nrow();
  int m = macc.ncol();

  arma::cube cubeArray(K, m, 4, arma::fill::zeros);

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      double x = macc(i, j);
      int k = which_set[i];
      cubeArray(k, j, 0) += x;
      cubeArray(k, j, 1) += x * x;
      cubeArray(k, j, 2) += x * y[i];
      cubeArray(k, j, 3) += y[i];
    }
  }

  return wrap(cubeArray);
}
