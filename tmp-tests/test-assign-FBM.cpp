#include <bigstatsr/BMAcc.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List testAssign(XPtr<FBM> xpMat) {

  BMAcc<double> macc(xpMat);
  std::size_t n = macc.nrow();
  std::size_t m = macc.ncol();

  std::size_t i, j;

  double v = 0;
  Rcout << macc(0, 0) << std::endl; //DEBUG
  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      // macc[j][i] = v++;
      macc(i, j) = v++;
    }
  }
  return List::create((int)n, (int)m);
}

// [[Rcpp::export]]
List testSize_t(std::size_t x) {

  Rcout << sizeof(x) << std::endl;

  return List::create((int)x, (double)x);
}

// [[Rcpp::export]]
List testAssign2(XPtr<FBM> xpMat, double val) {

  BMAcc<double> macc(xpMat);
  std::size_t n = macc.nrow();
  std::size_t m = macc.ncol();

  std::fill_n((double*)macc[0], 10, val);
  // memset((double*)macc[0], (double)1, n * m);

  return List::create((int)n, (int)m);
}

/*** R
testSize_t(10)
testSize_t(10L)

file <- "../FBM/tmp-tests/test.bin"
if (!file.exists(file)) file <- paste0("../", file)
readBin(file, "double", 200)
test <- bigstatsr::FBM_class$new(file, 10, 10, "double")
testAssign(test$address)
readBin(file, "double", 200)
testAssign2(test$address, 2)
readBin(file, "double", 200)

# file <- "../FBM/tmp-tests/test.bin"
# test <- bigstatsr::FBM_class$new(file, 10, 10, "double")
# test$address
# test2 <- test
# rm(test); gc()
# rm(test2); gc()
*/
