// [[Rcpp::depends(BH, bigmemory)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix& incrMat(NumericMatrix& dest, const NumericMatrix& source) {
  dest += source;

  return(dest);
}

// [[Rcpp::export]]
void incrG(SEXP pBigMat, const NumericMatrix& source,
           double n, double L, double m) {
  XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> macc(*xpMat);

  for (int j = 0; j < L; j++) {
    for (int i = 0; i < n; i++) {
      macc[j][i] += source(i, j) / m;
    }
  }

  return;
}
