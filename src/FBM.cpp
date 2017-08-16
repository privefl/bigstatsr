/******************************************************************************/
/* Inspired from
 * https://github.com/QuantGen/BEDMatrix/blob/master/src/BEDMatrix.cpp ********/

#include <bigstatsr/FBM.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;
using namespace boost::interprocess;

/******************************************************************************/

FBM::FBM(const List& desc)
  : n(desc["nrow"]), m(desc["ncol"]), type(desc["type"]) {

  std::string path = desc["backingfile"];
  try {
    this->file = file_mapping(path.c_str(), read_write);
  } catch(interprocess_exception& e) {
    throw std::runtime_error("File '" + path + "' not found.");
  }

  this->file_region = mapped_region(this->file, read_write);
  this->file_data = reinterpret_cast<void*>(this->file_region.get_address());

  // const size_t num_bytes = this->file_region.get_size();
  // Rcout << "FBM: " << (int)this->n << " " << (int)this->m << std::endl; //DEBUG
}

/******************************************************************************/

// [[Rcpp::export]]
SEXP getXPtrFBM(const List& desc) {

  // http://gallery.rcpp.org/articles/intro-to-exceptions/
  try {
    // Create a pointer to an FBM object and wrap it as an external pointer
    XPtr<FBM> ptr(new FBM(desc), true);
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

// // [[Rcpp::export]]
// void freeFBM(SEXP sexp) {
//   XPtr<FBM> xpBM(sexp);
//   delete(xpBM);
// }

/******************************************************************************/
