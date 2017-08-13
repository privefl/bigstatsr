#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix vecAsMat(NumericMatrix x) {
  NumericMatrix res(3, 5);
  for (int j = 0; j < 5; j++) {
    for (int i = 0; i < 3; i++) {
      res(i, j) = x(i, j);
    }
  }
  return res;
}


/*** R
vecAsMat(1:15) # not a matrix
*/
