#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool test_bool(double x, double y) {
  return x > 0.5 * y;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
test_bool(0.5, 0.9)
*/
