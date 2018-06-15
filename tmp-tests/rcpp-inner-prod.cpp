#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double COPY_wsum(const NumericVector &r, const NumericVector &w, size_t n) {
  double rw_sum = 0;
  for (size_t i = 0; i < n; i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}

// [[Rcpp::export]]
double COPY_wsum3(const NumericVector &r, const NumericVector &w) {
  double rw_sum = 0;
  for (int i = 0; i < w.size(); i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}

// [[Rcpp::export]]
double COPY_wsum4(const NumericVector &r, const NumericVector &w) {
  double rw_sum = 0;
  int n = w.size();
  for (int i = 0; i < n; i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}

// [[Rcpp::export]]
double COPY_wsum2(const NumericVector &r, const NumericVector &w) {
  return std::inner_product(r.begin(), r.end(), w.begin(), 0.0);
}


/*** R
N <- 1e6; x <- rnorm(N); w <- runif(N)
all.equal(COPY_wsum(x, w, N), COPY_wsum2(x, w))
microbenchmark::microbenchmark(
  COPY_wsum(x, w, N),
  COPY_wsum3(x, w),
  COPY_wsum4(x, w),
  COPY_wsum2(x, w)
)
*/
