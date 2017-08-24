// [[Rcpp::depends(bigstatsr, BH, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <bigstatsr/FBM.h>
using namespace Rcpp;


// [[Rcpp::export]]
void testCopy(XPtr<FBM> xpBM, const arma::Mat<unsigned char>& tab) {
  unsigned char* ptr = static_cast<unsigned char*>(xpBM->matrix());
  for (unsigned char i = 0; i < 4; i++) {
    std::copy(tab.colptr(i), tab.colptr(i + 1), ptr + 4 * i);
  }
  return ;
}


/*** R
tmp <- FBM(4, 4, type = "raw")
tmp[]
tab <- matrix(as.raw(1:16), 4)
tab[]
testCopy(tmp$address, tab)
tmp[]
*/
