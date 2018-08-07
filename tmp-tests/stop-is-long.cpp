#include <Rcpp.h>
using namespace Rcpp;

template<int RTYPE>
double myfun(Vector<RTYPE> x) {
  return x[0];
}

double get_col(SEXP x) {
  int r_type = TYPEOF(x);
  switch(r_type) {
  case INTSXP:  return myfun(as<IntegerVector>(x));
  case REALSXP: return myfun(as<NumericVector>(x));
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));
  }
}

// [[Rcpp::export]]
NumericVector test(DataFrame x) {

  int L = x.size();
  NumericVector res(L);
  for (int l = 0; l < L; l++) {
    SEXP col = x[l];
    res[l] = get_col(col);
  }

  return res;
}

/*** R
iris <- datasets::iris
iris$Species <- as.character(iris$Species)
iris <- iris[rep(1:150, 2000), rep(1:5, 200)]
test(iris)

# iris$Species <- as.character(iris$Species)
# iris <- iris[rep(1:150, 100), rep(1:5, 100)]
# bigstatsr::as_FBM(iris)
*/
