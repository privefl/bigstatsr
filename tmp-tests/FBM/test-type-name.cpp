#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string testType(double x) {
  void* type = typeid(x);
  return type.name();
}


/*** R
testType(42)
*/
