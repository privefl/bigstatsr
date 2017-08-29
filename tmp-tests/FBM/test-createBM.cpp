// [[Rcpp::depends(BH)]]

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/exceptions.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <fstream>
#include <Rcpp.h>
using namespace Rcpp;
using namespace boost::interprocess;

void createFile(std::string fileName, size_t nrow, size_t ncol) {
  
  try {
    
    std::filebuf fbuf;
    
    fbuf.open(fileName.c_str(), std::ios_base::in | 
      std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
    fbuf.pubseekoff(nrow * ncol * sizeof(double) - 1, std::ios_base::beg);
    fbuf.sputc(0);
    fbuf.close();
    
  } catch(std::exception& ex) {
    throw std::runtime_error("Problem creating the backing file.");
  }
  
} // multiarch


double* connectFile(std::string fileName) {
  
  file_mapping mFile;
  
  try {
    mFile = file_mapping(fileName.c_str(), read_write);
  } catch(std::exception& ex) {
    throw std::runtime_error("File not found.");
  }
  
  mapped_region file_region(mFile, read_write);
  
  return reinterpret_cast<double*>(file_region.get_address());
}

// [[Rcpp::export]]
SEXP createBM(std::string fileName, size_t nrow, size_t ncol) {
  
  createFile(fileName, nrow, ncol);
  
  double * address = connectFile(fileName);
  
  XPtr<double*> ptr(&address, true);
  
  return ptr;
}



/*** R
test <- createBM("test.bin", 10, 10) 
# the object pointed by no longer exists after a time
*/