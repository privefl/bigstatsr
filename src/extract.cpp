/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

template <typename T, int RTYPE>
Vector<RTYPE> extractVec(VecBMAcc<T> macc) {

  size_t K = macc.nelem();

  Vector<RTYPE> res(K);

  for (size_t k = 0; k < K; k++)
    res[k] = macc[k];

  return res;
}

// [[Rcpp::export]]
RObject extractVec(RObject xpbm,
                   const NumericVector& elemInd) {

  XPtr<FBM> xpBM(xpbm);
  return extractVec<double, REALSXP>(VecBMAcc<double>(xpBM, elemInd - 1));
}

/******************************************************************************/

template <typename T, int RTYPE>
Vector<RTYPE> extractMat(SubBMAcc<T> macc) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();

  Matrix<RTYPE> res(n, m);

  for (size_t j = 0; j < m; j++)
    for (size_t i = 0; i < n; i++)
      res(i, j) = macc(i, j);

  return res;
}

// [[Rcpp::export]]
RObject extractMat(RObject xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd) {

  XPtr<FBM> xpBM(xpbm);
  return extractMat<double, REALSXP>(SubBMAcc<double>(xpBM, rowInd - 1,
                                                      colInd - 1));
}

/******************************************************************************/
