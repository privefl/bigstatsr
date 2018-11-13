// [[Rcpp::depends(RcppParallel)]]
#include <bigstatsr/BMAcc.h>
#include <RcppParallel.h>
using namespace RcppParallel;

// [[Rcpp::export]]
Matrix<REALSXP> test_extract_sequential(SEXP xptr) {

  XPtr<FBM> xpBM(xptr);
  BMAcc<double> macc(xpBM);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  Matrix<REALSXP> res(n, m);

  for (size_t j = 0; j < m; j++)
    for (size_t i = 0; i < n; i++)
      res(i, j) = macc(i, j);

  return res;
}

struct ExtractMat : public Worker
{
  // source matrix
  BMAcc<double> macc;

  // destination matrix
  RMatrix<double> res;

  size_t n;

  // initialize with source and destination
  ExtractMat(BMAcc<double> macc, NumericMatrix res)
    : macc(macc), res(res) {
    n = macc.nrow();
  }

  // extract range
  void operator()(std::size_t begin, std::size_t end) {
    for (size_t j = begin; j < end; j++)
      for (size_t i = 0; i < n; i++)
        res(i, j) = macc(i, j);
  }
};

// [[Rcpp::export]]
Matrix<REALSXP> test_extract_parallel(SEXP xptr) {

  XPtr<FBM> xpBM(xptr);
  BMAcc<double> macc(xpBM);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  Matrix<REALSXP> res(n, m);
  ExtractMat extract_mat(macc, res);

  parallelFor(0, m, extract_mat);

  return res;
}

/*** R
library(bigstatsr)
N <- 6000
X <- FBM(N, N, init = runif(N * N))
microbenchmark::microbenchmark(
  mat1 <- test_extract_sequential(X$address),
  mat2 <- test_extract_parallel(X$address),
  times = 20
)
stopifnot(identical(mat1, mat2))

N <- 15000
X <- FBM(N, N, init = runif(N * N))
system.time(
  mat2 <- test_extract_parallel(X$address)
)
system.time(
  mat1 <- test_extract_sequential(X$address)
)
*/
