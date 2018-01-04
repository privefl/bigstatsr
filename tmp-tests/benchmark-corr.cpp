#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix normalize(const NumericMatrix& mat) {
  int n = mat.nrow();
  int m = mat.ncol();

  double x, xSum, xxSum, mean, norm;
  int i, j;

  NumericMatrix res = Rcpp::clone(mat);

  for (j = 0; j < m; j++) {
    // compute mean and sj of column j
    xSum = xxSum = 0;
    for (i = 0; i < n; i++) {
      x = mat(i, j);
      xSum += x;
      xxSum += x*x;
    }
    norm = sqrt((xxSum - xSum * xSum / n));
    mean = xSum / n;
    // normalize column j of output
    for (i = 0; i < n; i++) {
      res(i, j) -= mean;
      res(i, j) /= norm;
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericMatrix& correlize(NumericMatrix& mat,
                         const NumericVector& shift,
                         const NumericVector& scale) {

  int n = mat.nrow();
  int i, j;

  for (j = 0; j < n; j++) {
    for (i = 0; i < n; i++) {
      mat(i, j) -= shift(i) * shift(j);
      mat(i, j) /= scale(i) * scale(j);
    }
  }

  return mat;
}

/*** R
cor2 <- function(mat) {
  crossprod(normalize(mat))
}
a <- matrix(0, 10000, 500); a[] <- rnorm(length(a))
all.equal(cor2(a), cor(a))
sd(a[, 1]) # a has not been modified

cor3 <- function(mat) {
  sums <- colSums(mat) / sqrt(nrow(mat))
  corr <- crossprod(mat)
  diags <- sqrt(diag(corr) - sums^2)
  correlize(corr, shift = sums, scale = diags)
}
all.equal(cor3(a), cor(a))
sd(a[, 1]) # a has not been modified

library(microbenchmark)
print(microbenchmark(
  cor(a),
  cor2(a),
  cor3(a),
  times = 20
))
# b <- matrix(0, 20000, 2000); a[] <- rnorm(length(a))
# cor2(b)
*/
