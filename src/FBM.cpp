#include <bigstatsr/FBM.h>

using namespace Rcpp;
using namespace boost::interprocess;

template <typename T>
FBM<T>::FBM(std::string path, std::size_t n, std::size_t p) : nrow(n), ncol(p) {
  try {
    this->file = file_mapping(path.c_str(), read_write);
  } catch(const interprocess_exception& e) {
    throw std::runtime_error("File '" + path + "' not found.");
  }
  this->file_region = mapped_region(this->file, read_write);
  this->file_data = static_cast<T*>(this->file_region.get_address());
  // const size_t num_bytes = this->file_region.get_size();
}

// [[Rcpp::export]]
SEXP new_FBM(std::string path, std::size_t n, std::size_t p) {

  try {
    // Create a pointer to a FBM object and wrap it as an external pointer
    XPtr< FBM<double> > ptr(new FBM<double>(path, n, p), true);
    // Return the external pointer to the R side
    return ptr;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) {
    ::Rf_error("C++ exception (unknown reason)");
  }

  return NULL;
};
