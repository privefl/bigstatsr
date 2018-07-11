#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix vec2mat(NumericVector x) {

  NumericMatrix x2(x);
  return x2;
}

// [[Rcpp::export]]
NumericVector vec2vec(SEXP x) {

  NumericVector x2(x);
  return x2;
}

// [[Rcpp::export]]
NumericVector vec2vec2(RObject x) {

  NumericVector x2(x);
  return x2;
}

// [[Rcpp::export]]
IntegerVector log2int(LogicalVector x) {

  IntegerVector x2(x);
  return x2;
}

// [[Rcpp::export]]
LogicalVector log2log(LogicalVector x) {

  LogicalVector x2(x);
  return x2;
}

/*** R
X <- matrix(runif(1e7), 1000)
microbenchmark::microbenchmark(
  X2 <- vec2mat(X),
  X3 <- vec2vec(X),
  X4 <- vec2vec2(X),
  times = 1000
)
all.equal(X2, X)
all.equal(X3, X)
all.equal(X4, X)

X <- sample(c(TRUE, FALSE, NA), 1e6, TRUE)
X2 <- log2int(X)

microbenchmark::microbenchmark(
  X2 <- log2int(X),
  X3 <- log2log(X),
  times = 1000
)
all.equal(X2, X)
all.equal(X3, X)
*/
