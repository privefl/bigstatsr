#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector seq_len_cpp(const NumericVector& x) {
  
  return Rcpp::seq_len(x.size());;
}


/*** R
seq_len_cpp(42:45)
*/
