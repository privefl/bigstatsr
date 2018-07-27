#include <bigstatsr/BMAcc.h>

// [[Rcpp::export]]
NumericMatrix extract0(Environment X) {

  XPtr<FBM> xptr = X["address"];
  BMAcc<double> macc(xptr);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  NumericMatrix res(n, m);
  size_t i, j;

  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      res(i, j) = macc(i, j);
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericMatrix extract1(Environment X, size_t min = 1000) {

  XPtr<FBM> xptr = X["address"];
  BMAcc<double> macc(xptr);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  NumericMatrix res(n, m);
  size_t i, j;

  for (j = 0; j < m; j++) {

    i = 0;

    if (n > 4) {
      for (; i <= n - 4; i += 4) {
        res(i, j) = macc(i, j);
        res(i + 1, j) = macc(i + 1, j);
        res(i + 2, j) = macc(i + 2, j);
        res(i + 3, j) = macc(i + 3, j);
      }
    }

    for (; i < n; i++) {
      res(i, j) = macc(i, j);
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericMatrix extract2(Environment X) {

  XPtr<FBM> xptr = X["address"];
  BMAcc<double> macc(xptr);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  NumericMatrix res(n, m);
  size_t i, j;

  j = 0;

  if (m > 4) {
    for (; j <= m - 4; j += 4) {
      for (i = 0; i < n; i++) {
        res(i, j) = macc(i, j);
        res(i, j + 1) = macc(i, j + 1);
        res(i, j + 2) = macc(i, j + 2);
        res(i, j + 3) = macc(i, j + 3);
      }
    }
  }

  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res(i, j) = macc(i, j);
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericMatrix extract3(Environment X) {

  XPtr<FBM> xptr = X["address"];
  BMAcc<double> macc(xptr);
  size_t n = macc.nrow();
  size_t m = macc.ncol();

  NumericMatrix res(n, m);

  double *beg = static_cast<double*>(xptr->matrix());
  double *end = beg + n * m;
  std::copy(beg, end, &(res(0, 0)));
  return res;
}


/*** R
X <- FBM(3000, 3000); X[] <- rnorm(length(X))
microbenchmark::microbenchmark(
  resX <- X[],
  res0 <- extract0(X),
  res1 <- extract1(X),
  res2 <- extract2(X),
  res3 <- extract3(X),
  times = 20
)
stopifnot(identical(resX, res0))
stopifnot(identical(res1, res0))
stopifnot(identical(res2, res0))
stopifnot(identical(res3, res0))
*/
