#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
IntegerVector timesTwo(const NumericVector& x,
                       const Nullable<IntegerVector>& colInd = R_NilValue) {
  IntegerVector cols;
  if (colInd.isNull()) {
    cols = seq_len(x.size()) - 1;
  } else {
    cols = as<IntegerVector>(colInd) - 1;
  }
  return cols;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42:52)
*/
