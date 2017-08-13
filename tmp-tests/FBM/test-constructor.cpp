// [[Rcpp::depends(BH)]]
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
using namespace boost::interprocess;

template <typename T> 
class FBM : private boost::noncopyable {
public:
  FBM(string path, size_t n, size_t p);
  // Rcpp::IntegerVector extract_vector(Rcpp::IntegerVector i);
  // Rcpp::IntegerMatrix extract_matrix(Rcpp::IntegerVector i, Rcpp::IntegerVector j);
private:
  file_mapping file;
  mapped_region file_region;
  T* file_data;
  size_t nrow;
  size_t ncol;
};

template <typename T>
FBM<T>::FBM(std::string path, std::size_t n, std::size_t p) : nrow(n), ncol(p) {
  try {
    this->file = file_mapping(path.c_str(), read_write);
  } catch(const interprocess_exception& e) {
    throw std::runtime_error("File not found.");
  }
  this->file_region = mapped_region(this->file, read_write);
  this->file_data = static_cast<T*>(this->file_region.get_address());
  // const std::size_t num_bytes = this->file_region.get_size();
}

// Export FBM::FBM
// [[Rcpp::export]]
SEXP new_FBM(string path, size_t n, size_t p) {
  
  try {
    // Create a pointer to a FBM object and wrap it as an external pointer
    XPtr<FBM<double> > ptr(new FBM<double>(path, n, p), true);
    // Return the external pointer to the R side
    return ptr;
  } catch(std::exception &ex) {
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("C++ exception (unknown reason)"); 
  }
  
  return NULL;
};


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
test <- new_FBM("test.bin", 10, 10)
*/
