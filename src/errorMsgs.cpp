#include <bigstatsr/utils.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
const char* const GET_ERROR_TYPE() {
  return ERROR_TYPE;
}


/*** R
GET_ERROR_TYPE()
*/
