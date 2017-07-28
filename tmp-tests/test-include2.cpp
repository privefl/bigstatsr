// [[Rcpp::depends(bigstatsr, bigmemory, BH)]]
#include <bigstatsr.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
ListOf<NumericVector> timesTwo(const NumericMatrix& x) {
  return bigstatsr::bigcolvars(x);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(matrix(1:4, 2))
*/
