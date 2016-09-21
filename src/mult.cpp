#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix& incrMat(NumericMatrix& dest, const NumericMatrix& source) {
  dest += source;

  return(dest);
}
