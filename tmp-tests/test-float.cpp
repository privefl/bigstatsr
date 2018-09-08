#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double test_float(float x) {
  Rcout << x << std::endl;
  return x;
}

/*** R
test_float(0.5)
test_float(42)
test_float(NA)
test_float(NA_integer_)
test_float(NA_real_)
test_float(NaN)
test_float(Inf)
test_float(as.integer(2^23))
test_float(as.integer(2^31 - 1))
test_float(as.integer(2^31 - 2))
*/
