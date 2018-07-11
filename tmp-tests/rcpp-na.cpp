#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void checkNA() {
  NumericVector v =
    NumericVector::create( 1, NA_REAL, R_NaN, R_PosInf, R_NegInf);
  LogicalVector l1 = is_na(v);
  LogicalVector l2 = is_nan(v);
  LogicalVector l3 = is_infinite(v);
  LogicalVector l4 = (v == NA_REAL);
  Rcout << l1 << "\n"; // 0 1 1 0 0
  Rcout << l2 << "\n"; // 0 0 1 0 0
  Rcout << l3 << "\n"; // 0 0 0 1 1;
  Rcout << l4 << "\n"; // NA;
}

// [[Rcpp::export]]
void rcpp_is_na2() {

  // Creating Vector object containing NA NaN Inf -Inf
  NumericVector v =
    NumericVector::create(1, NA_REAL, R_NaN, R_PosInf, R_NegInf);

  // Evaluating the value for each element of the vector
  int n = v.length();
  for (int i = 0; i < n; ++i) {
    if(R_IsNA(v[i]))
      Rprintf("v[%i] is R_NA.\n", i);
    if(std::isnan(v[i]))
      Rprintf("v[%i] is std::NaN.\n", i);
    if(NumericVector::is_na(v[i]))
      Rprintf("v[%i] is NA.\n", i);
    if(Rcpp::traits::is_nan<REALSXP>(v[i]))
      Rprintf("v[%i] is NaN.\n", i);
    if(Rcpp::traits::is_infinite<REALSXP>(v[i]))
      Rprintf("v[%i] is Inf or -Inf.\n", i);
  }
}

/*** R
checkNA()
rcpp_is_na2()
*/
