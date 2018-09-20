// [[Rcpp::depends(BH, bigstatsr, RcppParallel)]]
#include <bigstatsr/BMAcc.h>
#include <RcppParallel.h>
using namespace RcppParallel;

// [[Rcpp::export]]
NumericVector test_prod_sequential(SEXP xptr, NumericVector x) {

  XPtr<FBM> xpBM(xptr);
  BMAcc<double> macc(xpBM);
  int i, j, n = macc.nrow(), m = macc.ncol();

  NumericVector res(n);

  for (j = 0; j <= m - 4; j += 4) { // unrolling optimization
    for (i = 0; i < n; i++) {
      res[i] += (x[j] * macc(i, j) + x[j+1] * macc(i, j+1)) +
        (x[j+2] * macc(i, j+2) + x[j+3] * macc(i, j+3));
    } // The parentheses are somehow important.
  }
  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc(i, j);
    }
  }

  return res;
}

struct Prod : public Worker
{
  // source matrix
  BMAcc<double> macc;

  // destination vector
  const RVector<double> x;
  RVector<double> res_all;

  size_t n;

  // initialize with source and destination
  Prod(BMAcc<double> macc, const NumericVector x, RVector<double> res)
    : macc(macc), x(x), res_all(res) {
    n = macc.nrow();
  }
  Prod(const Prod& prod, Split)
    : macc(prod.macc), x(prod.x), res_all(prod.res_all) {
    n = macc.nrow();
  }

  // prod range
  void operator()(size_t begin, size_t end) {
    NumericVector res(n);
    for (size_t j = begin; j < end; j++)
      for (size_t i = 0; i < n; i++)
        res[i] += x[j] * macc(i, j);
    for (size_t i = 0; i < n; i++)
      res_all[i] = res[i];
  }

  // join my value with that of another prod
  void join(const Prod& rhs) {
    for (int i = 0; i < n; i++) {
      res_all[i] += rhs.res_all[i];
    }
  }
};

// [[Rcpp::export]]
NumericVector test_prod_parallel(SEXP xptr, NumericVector x) {

  XPtr<FBM> xpBM(xptr);
  BMAcc<double> macc(xpBM);

  NumericVector res(macc.nrow());   // allocate output vector
  RVector<double> output(res);      // create threadsafe wrapper to output
  Prod prod(macc, x, output);
  parallelReduce(0, macc.ncol(), prod, 100);

  return res;
}

/*** R
library(bigstatsr)
RcppParallel::setThreadOptions(numThreads = 1)
N <- 1000
X <- FBM(N, N, init = runif(N * N))
x <- runif(N)
microbenchmark::microbenchmark(
  prod1 <- test_prod_sequential(X$address, x),
  prod2 <- test_prod_parallel(X$address, x),
  times = 20
)
stopifnot(all.equal(prod1, prod2))

# N <- 15000
# X <- FBM(N, N, init = runif(N * N))
# system.time(
#   mat2 <- test_prod_parallel(X$address)
# )
# system.time(
#   mat1 <- test_prod_sequential(X$address)
# )
*/
