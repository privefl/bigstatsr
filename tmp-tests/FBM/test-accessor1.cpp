// [[Rcpp::depends(bigstatsr, bigmemory, BH)]]
#include <bigmemory/MatrixAccessor.hpp>
#include <bigstatsr/SubMatAcc.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector getVecCode(const S4& x,
                         const IntegerVector& i) {
  
  XPtr<BigMatrix> xpMat = x.slot("address");
  NumericVector    code = x.slot("code");
  
  int n = i.size();
  
  NumericVector res(n);
  
  unsigned char* pMat = reinterpret_cast<unsigned char*>(xpMat->matrix());
  
  for (int k = 0; k < n; k++)
    res[k] = code[pMat[i[k] - 1]];
  
  return res;
}

// [[Rcpp::export]]
NumericMatrix getMatCode(const S4& x,
                         const IntegerVector& i,
                         const IntegerVector& j) {
  
  XPtr<BigMatrix> xpMat = x.slot("address");
  RawSubMatAcc macc(*xpMat, i - 1, j - 1, x.slot("code"));
  
  int n = i.size();
  int m = j.size();
  NumericMatrix res(n, m);
  int k, l;
  
  for (l = 0; l < m; l++) 
    for (k = 0; k < n; k++)
      res(k, l) = macc(k, l);
  
  return res;
}