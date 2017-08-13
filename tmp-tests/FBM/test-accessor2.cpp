#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getVec(const NumericMatrix& x,
                     const IntegerVector& i) {
  
  int n = i.size();
  
  NumericVector res(n);
  
  for (int k = 0; k < n; k++)
    res[k] = x[i[k] - 1] + 1;
  
  return res;
}

// [[Rcpp::export]]
NumericMatrix getMat(const NumericMatrix& x,
                     const IntegerVector& i,
                     const IntegerVector& j) {
  
  int n = i.size();
  int m = j.size();
  
  IntegerVector i_ = i - 1;
  IntegerVector j_ = j - 1;
  
  NumericMatrix res(n, m);
  
  int k, l;
  
  for (l = 0; l < m; l++) 
    for (k = 0; k < n; k++)
      res(k, l) = x(i_[k], j_[l]) + 1;
  
  return res;
}

