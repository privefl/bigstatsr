/******************************************************************************/

#include "bigstatsr.h"

/******************************************************************************/

// counts by rows
// [[Rcpp::export]]
IntegerMatrix mycount1(XPtr<BigMatrix> xpMat,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd,
                       const IntegerVector& codeInd) {

  SubMatAcc<unsigned char> macc(*xpMat, rowInd-1, colInd-1);

  int n = macc.nrow();
  int m = macc.ncol();
  int K = max(codeInd); // number of unique elements

  // indices begin at 1 in R and 0 in C++
  IntegerVector tabU = codeInd - 1;

  IntegerMatrix res(K, n);

  int i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      (res(tabU[macc(i, j)], i))++;

  return res;
}

/******************************************************************************/

// counts by columns
// [[Rcpp::export]]
IntegerMatrix mycount2(XPtr<BigMatrix> xpMat,
                      const IntegerVector& rowInd,
                      const IntegerVector& colInd,
                      const IntegerVector& codeInd) {

  SubMatAcc<unsigned char> macc(*xpMat, rowInd-1, colInd-1);

  int n = macc.nrow();
  int m = macc.ncol();
  int K = max(codeInd); // number of unique elements

  // indices begin at 1 in R and 0 in C++
  IntegerVector tabU = codeInd - 1;

  IntegerMatrix res(K, m);

  int i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      (res(tabU[macc(i, j)], j))++;

  return res;
}

/******************************************************************************/
