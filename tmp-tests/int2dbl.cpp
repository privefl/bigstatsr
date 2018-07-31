#include <Rcpp.h>
using namespace Rcpp;


class Int2NumVector : public Vector<INTSXP> {
public:
  Int2NumVector(SEXP x) : Vector<INTSXP>(x) {}

  inline double operator[](size_t k) {
    double x_k = Vector<INTSXP>::operator[](k);
    if (x_k == NA_INTEGER) x_k = NA_REAL;
    return x_k;
  }

  size_t size() const { return Vector<INTSXP>::size(); }

};

class Int2NumMatrix : public Matrix<INTSXP> {
public:
  Int2NumMatrix(SEXP x) : Matrix<INTSXP>(x) {}

  inline double operator()(size_t i, size_t j) {
    double x_ij = Matrix<INTSXP>::operator()(i, j);
    if (x_ij == NA_INTEGER) x_ij = NA_REAL;
    return x_ij;
  }

  inline double operator[](size_t k) {
    double x_k = Matrix<INTSXP>::operator[](k);
    if (x_k == NA_INTEGER) x_k = NA_REAL;
    return x_k;
  }

  size_t nrow() const { return Matrix<INTSXP>::nrow(); }
  size_t ncol() const { return Matrix<INTSXP>::ncol(); }
  size_t size() const { return Matrix<INTSXP>::size(); }

};

// [[Rcpp::export]]
NumericVector int2dbl1(SEXP x) {

  IntegerVector x1 = x;
  size_t n = x1.size();
  NumericVector res1(n);
  for (size_t i = 0; i < n; i++) res1[i] = x1[i];

  return res1;
}

// [[Rcpp::export]]
NumericVector int2dbl2(SEXP x) {

  Int2NumVector x2 = x;
  size_t n = x2.size();
  // Rcout << n << std::endl;
  NumericVector res2(n);
  for (size_t i = 0; i < n; i++) res2[i] = x2[i];

  return res2;
}

// [[Rcpp::export]]
NumericVector int2dbl3(SEXP x) {

  NumericVector x3 = x;
  size_t n = x3.size();
  NumericVector res3(n);
  for (size_t i = 0; i < n; i++) res3[i] = x3[i];

  return res3;
}

// [[Rcpp::export]]
ListOf<NumericVector> int2dbl(SEXP x) {
  return List::create(int2dbl1(x), int2dbl2(x), int2dbl3(x));
}

// [[Rcpp::export]]
NumericMatrix int2dbl22(SEXP x) {

  Int2NumMatrix x2 = x;
  size_t n = x2.nrow();
  size_t m = x2.ncol();
  // Rcout << n << std::endl;
  NumericMatrix res2(n, m);
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < n; i++)
      res2(i, j) = x2(i, j);

  return res2;
}

/*** R
x <- as.integer(c(0, 1, NA, -2^31+1))
int2dbl(x)
int2dbl(NA)
int2dbl(NA_integer_)
# FBM(1, 1, type = "double", init = NA_integer_)[]
# FBM(1, 1, type = "double", init = NA)[]
x2 <- sample(1e7)
microbenchmark::microbenchmark(
  int2dbl1(x2),
  int2dbl2(x2),
  int2dbl3(x2),
  times = 50
)

round(length(x2) * 8 / 1024^2)

tmp <- gc(reset = TRUE)
res1 <- int2dbl1(x2)
gc() - tmp

tmp <- gc(reset = TRUE)
res2 <- int2dbl2(x2)
gc() - tmp

tmp <- gc(reset = TRUE)
res3 <- int2dbl3(x2)
gc() - tmp

x3 <- matrix(x2, 1e3)
tmp <- gc(reset = TRUE)
res4 <- int2dbl22(x3)
gc() - tmp
dim(res4)
typeof(res4)
*/
