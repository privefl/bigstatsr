#include <Rcpp.h>
using namespace Rcpp;

inline double int2dbl(int x) {
  return (x == NA_INTEGER) ? NA_REAL : x;
}

template<typename T_IN, typename T_OUT>
inline T_OUT identity(T_IN x) {
  return x;
}

template<typename T_IN, typename T_OUT>
T_OUT conv(T_IN x, T_OUT (*func)(T_IN) = identity) {
  return func(x);
}

// [[Rcpp::export]]
double test(int x) {
  return conv(x, int2dbl);
}

// [[Rcpp::export]]
double test2(int x) {
  double (*func)(int) = identity;
  return func(x);
}


/*** R
test(42)
test(42.5)
test(NA)
test(NA_integer_)
test2(42)
test2(42.5)
test2(NA)
test2(NA_integer_)
*/
