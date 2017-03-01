/******************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix decodeMat(const RawMatrix& source, const NumericVector& code) {
  int n = source.nrow();
  int m = source.ncol();
  int i, j;

  NumericMatrix res(n, m);

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      res(i, j) = code[source(i, j)];

  return res;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericVector decodeVec(const RawVector& source, const NumericVector& code) {
  int n = source.size();

  NumericVector res(n);

  for (int i = 0; i < n; i++)
    res[i] = code[source[i]];

  return res;
}

/******************************************************************************/
