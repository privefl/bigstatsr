// [[Rcpp::depends(BH)]]

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/exceptions.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <fstream>
#include <Rcpp.h>
using namespace Rcpp;
using namespace boost::interprocess;

// [[Rcpp::export]]
bool createFile(const std::string &fileName,
                 size_t nrow, size_t ncol) {
  
  std::filebuf fbuf;
  if (!fbuf.open(fileName.c_str(), std::ios_base::in | 
      std::ios_base::out | std::ios_base::trunc | std::ios_base::binary ))
  {
    return false;
  }
  fbuf.pubseekoff(nrow*ncol*sizeof(double)-1, std::ios_base::beg);
  // I'm not sure if I need this next line
  fbuf.sputc(0);
  fbuf.close();
  return true;
} // multiarch

// [[Rcpp::export]]
void connectFile(const std::string &fileName) {
  
  file_mapping mFile;
  
  try {
    mFile = file_mapping(fileName.c_str(), read_write);
  } catch(const interprocess_exception& e) {
    throw std::runtime_error("File not found.");
  }
  
  mapped_region file_region(mFile, read_write);
  
  double* file_data = reinterpret_cast<double*>(file_region.get_address());
  
  // Modify first and third elements
  file_data[0] = 1;
  file_data[2] = 2;
}


/*** R
createFile("test.bin", 10, 10)
readBin("test.bin", "double", 200)

connectFile("test.bin")
readBin("test.bin", "double", 200)
*/