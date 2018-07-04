#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE_IN, int RTYPE_OUT>
Vector<RTYPE_OUT> conv_vec(Vector<RTYPE_IN> nv) {

  int i, n = nv.size();
  Vector<RTYPE_OUT> res(n);
  for (i = 0; i < n; i++) {
    res[i] = nv[i];
    if (nv[i] != res[i]) {
      warning("At least one value changed (%s -> %s) %s",
              nv[i], res[i], "when converting from types.");
      break;
    }
  }
  for (; i < n; i++) res[i] = nv[i];

  return res;
}

// [[Rcpp::export]]
IntegerVector conv_int(SEXP x) {

  int rtype = TYPEOF(x);

  if (rtype == INTSXP || rtype == LGLSXP || rtype == RAWSXP) {
    // no possible loss of precision
    return(as<IntegerVector>(x));
  } else if (rtype == REALSXP)  {
    // possible loss of precision
    return(conv_vec<REALSXP, INTSXP>(x));
  } else {
    stop("This R type is not supported.");
  }
}

// [[Rcpp::export]]
IntegerVector conv_int2(const RObject& x) {
  return as<IntegerVector>(x);
}

// [[Rcpp::export]]
IntegerVector& conv_int3(IntegerVector& x) {
  return x;
}

/*** R
x <- seq_len(1e5)
x2 <- sample(as.raw(0:255), 1e5, TRUE)
x3 <- x + 0
x4 <- x3; x4[1] <- 1.5
microbenchmark::microbenchmark(
  test1 <- conv_int(x),
  test2 <- conv_int2(x),
  test3 <- conv_int3(x),
  test4 <- conv_int(x2),
  test5 <- conv_int2(x2),
  test6 <- conv_int(x3),
  test7 <- conv_int2(x3),
  test8 <- conv_int(x4),
  test9 <- conv_int2(x4)
)

x3[1] <- 1.5
head(conv_int2(x3))
head(conv_int(x3))
(x5 <- matrix(1:4, 2))
conv_int(x5)
conv_int(x5 + 0)
x6 <- x5
storage.mode(x6) <- "raw"
conv_int(x6)
*/
