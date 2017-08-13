#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List access3(const List& x) {
  
  List desc = x["description"];
  
  return desc;
}

// [[Rcpp::export]]
int access_nrow(const List& x) {
  
  int nrow = x["address"];
  
  return nrow;
}

/*** R
(tmp <- access3(test))
access_nrow(test)
*/
