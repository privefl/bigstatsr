#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector testFunction(NumericVector x,
                           IntegerVector y,
                           Function f) {
  return f(x, y);
}

/*** R
library(bigstatsr)
testFunction(x <- rnorm(10), y <- sample(0:1, 10, TRUE), AUC)
AUC(x, y)
*/
