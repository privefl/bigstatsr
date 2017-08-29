#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector fprive(const RObject & x) {
  NumericVector nv(x);
  IntegerVector iv(x);
  if (is_true(any(nv != NumericVector(iv)))) warning("Uh-oh");
  return(iv);
}

/*** R
fprive(c(1.5, 2))
fprive(c(1L, 2L))
*/
