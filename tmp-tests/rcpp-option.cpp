#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool test_option() {

  Environment base("package:base");
  Function get_option = base["getOption"];
  SEXP warn = get_option("bigstatsr.downcast.warning");

  if (TYPEOF(warn) == LGLSXP) {
    return as<LogicalVector>(warn)[0];
  } else {
    return true;  // but this shoud not happen
  }
}


/*** R
test_option()
library(bigstatsr)
test_option()
options(bigstatsr.downcast.warning = FALSE)
test_option()
*/
