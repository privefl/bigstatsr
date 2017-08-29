// [[Rcpp::depends(bigstatsr, BH)]]
#include <bigstatsr/BMCodeAcc.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double testCode256(XPtr<FBM> xpBM,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   const NumericVector& code256) {

  SubBMCode256Acc macc(xpBM, rowInd - 1, colInd - 1, code256);
  return macc(0, 0);
}

/*** R
tmp <- new_FBM(10, 10, type = "raw", init = 1:100)
tmp[]
code <- 1:256
testCode256(tmp$address, rows_along(tmp), cols_along(tmp), code)
*/
