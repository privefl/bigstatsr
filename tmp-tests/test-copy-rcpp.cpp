#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test_change(int n) {

  NumericVector x(n);
  NumericVector x2(n);
  x2 = x;
  x[0] = 100;
  return x2;
}

// [[Rcpp::export]]
NumericVector test_change2(int n) {

  NumericVector x(n);
  NumericVector x2(n);
  std::copy(x.begin(), x.end(), x2.begin());
  x[0] = 100;
  return x2;
}

/*** R
test_change(10)
test_change2(10)
*/
