
#include <fstream>
#include <unistd.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool createFile(const std::string &fileName,
                size_t nrow, size_t ncol) {
  
  FILE *fp = fopen( fileName.c_str(), "wb");
  if (!fp)
  {
    fclose(fp);
    return false;
  }  
  off_t nbBytes = nrow*ncol*sizeof(double);
  Rprintf("Size: %ld - %d\n", nbBytes, sizeof(nbBytes));
  if (-1 == ftruncate( fileno(fp),  nrow*ncol*sizeof(double)) )
  {
    fclose(fp);
    return false;
  }
  fclose(fp);
  return true;
}

// [[Rcpp::export]]
bool createFile2(const std::string &fileName,
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


/*** R
createFile("test.bin", 10, 10)
readBin("test.bin", what = "double", n = 200)

system.time(
  test <- createFile("test2.bin", 50e3, 50e3)
)
test

createFile2("test3.bin", 10, 10)

system.time(
  test2 <- createFile2("test4.bin", 50e3, 50e3)
)
test2
*/
