#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int size_rcpp(NumericMatrix x) {
  return x.size();
}

/*** R
df <- datasets::mtcars
size_rcpp(as.matrix(df))
prod(dim(df))
*/
