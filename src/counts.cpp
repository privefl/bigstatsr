/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// counts by rows
// [[Rcpp::export]]
IntegerMatrix mycount1(Environment BM,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd,
                       const IntegerVector& codeInd) {

  XPtr<FBM> xpBM = BM["address"];
  SubBMAcc<unsigned char> macc(xpBM, rowInd-1, colInd-1);

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = max(codeInd); // number of unique elements

  // indices begin at 1 in R and 0 in C++
  IntegerVector tabU = codeInd - 1;

  IntegerMatrix res(K, n);

  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      (res(tabU[macc(i, j)], i))++;

  return res;
}

/******************************************************************************/

// counts by columns
// [[Rcpp::export]]
IntegerMatrix mycount2(Environment BM,
                       const IntegerVector& rowInd,
                       const IntegerVector& colInd,
                       const IntegerVector& codeInd) {

  XPtr<FBM> xpBM = BM["address"];
  SubBMAcc<unsigned char> macc(xpBM, rowInd-1, colInd-1);

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = max(codeInd); // number of unique elements

  // indices begin at 1 in R and 0 in C++
  IntegerVector tabU = codeInd - 1;

  IntegerMatrix res(K, m);

  size_t i, j;

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      (res(tabU[macc(i, j)], j))++;

  return res;
}

/******************************************************************************/
