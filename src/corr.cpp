#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix& correlize(NumericMatrix& mat,
                         const NumericVector& shift,
                         const NumericVector& scale) {

  int n = mat.nrow();
  int i, j;

  for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      // corresponds to "- \frac{1}{n} s_X * s_X^T"
      mat(i, j) -= shift(i) * shift(j);
      // corresponds to "S^T (...) S"
      mat(i, j) /= scale(i) * scale(j);
    }
  }

  return mat;
}
