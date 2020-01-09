/******************************************************************************/

#include <bigstatsr/FBM.h>

/******************************************************************************/

// [[Rcpp::export]]
SEXP getXPtrFBM(std::string path, std::size_t n, std::size_t m, int type) {

  // http://gallery.rcpp.org/articles/intro-to-exceptions/
  try {
    // Create a pointer to an FBM object and wrap it as an external pointer
    Rcpp::XPtr<FBM> ptr(new FBM(path, n, m, type), true);
    // Return the external pointer to the R side
    return ptr;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) {
    ::Rf_error("C++ exception (unknown reason)");
  }

  return R_NilValue;
}

/******************************************************************************/
// [[Rcpp::export]]
SEXP getXPtrFBM_RW(std::string path, std::size_t n, std::size_t m, int type) {

  // http://gallery.rcpp.org/articles/intro-to-exceptions/
  try {
    // Create a pointer to an FBM object and wrap it as an external pointer
    Rcpp::XPtr<FBM_RW> ptr(new FBM_RW(path, n, m, type), true);
    // Return the external pointer to the R side
    return ptr;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) {
    ::Rf_error("C++ exception (unknown reason)");
  }

  return R_NilValue;
}

/******************************************************************************/
