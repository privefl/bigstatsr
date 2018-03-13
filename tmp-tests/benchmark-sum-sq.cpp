#include <Rcpp.h>
using namespace Rcpp;

// Gaussian loss
// [[Rcpp::export]]
double COPY_gLoss(const NumericVector& r) {
  double l = 0;
  for (int i = 0; i < r.size(); i++) {
    l += pow(r[i], 2);
  }
  return l;
}

// [[Rcpp::export]]
double COPY_gLoss2(const NumericVector& r) {
  return std::inner_product(r.begin(), r.end(), r.begin(), 0.0);
}


// Weighted sum
// [[Rcpp::export]]
double COPY_wsum(const NumericVector &r, const NumericVector &w, size_t n) {
  double rw_sum = 0;
  for (size_t i = 0; i < n; i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}

// [[Rcpp::export]]
double COPY_wsum2(const NumericVector &r, const NumericVector &w) {
  return std::inner_product(r.begin(), r.end(), w.begin(), 0.0);;
}


/*** R
x <- runif(1e4)
microbenchmark::microbenchmark(
  COPY_gLoss(x),
  COPY_gLoss2(x)
)
all.equal(COPY_gLoss(x), COPY_gLoss2(x))

x <- rnorm(1e5)
microbenchmark::microbenchmark(
  COPY_gLoss(x),
  COPY_gLoss2(x)
)
all.equal(COPY_gLoss(x), COPY_gLoss2(x))

n <- length(x)
w <- runif(n)
microbenchmark::microbenchmark(
  COPY_wsum(x, w, n),
  COPY_wsum2(x, w)
)
all.equal(COPY_wsum(x, w, n), COPY_wsum2(x, w))
*/
