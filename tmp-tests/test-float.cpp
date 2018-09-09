#include <bigstatsr/utils.h>
using namespace Rcpp;

// [[Rcpp::export]]
double test_float(float x) {
  Rcout << x << std::endl;
  return x;
}

#define NA_FLOAT FLT_MIN

// [[Rcpp::export]]
double test_float2() {
  return NA_FLOAT;
}

// [[Rcpp::export]]
double test_float3() {
  return MIN_FLOAT;
}

// [[Rcpp::export]]
bool test_NA(double x) {
  return(x > 0);
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

test_float2()
test_float3()
FBM(2, 2, "float", test_float2())[]
FBM(2, 2, "float", test_float3())[]
FBM(2, 2, "float", 1.175494e-38)[]
FBM(2, 2, "float", 1.1754943e-38)[]
FBM(2, 2, "float", 2^-149)[]

FBM(2, 2, "float", NA)[]
FBM(2, 2, "float", NA_integer_)[]
FBM(2, 2, "float", c(NA_real_, Inf, -Inf, NaN))[]
FBM(2, 2, "float", 0.5)[]
FBM(2, 2, "float", 0.51)[]

test_NA(NA)
test_NA(NA_real_)
test_NA(NaN)
test_NA(0)
test_NA(0.1)
*/
