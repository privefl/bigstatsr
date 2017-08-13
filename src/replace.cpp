/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

template <class C, typename T>
void replaceVecOne(C macc, T val) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = val;
}

// [[Rcpp::export]]
void replaceVecOne(RObject xpbm,
                   const NumericVector& elemInd,
                   RObject val) {

  XPtr<FBM> xpBM(xpbm);
  return replaceVecOne(VecBMAcc<double>(xpBM, elemInd - 1),
                       as<double>(val));
}

/******************************************************************************/

template <class C, int RTYPE>
void replaceVec(C macc, const Vector<RTYPE>& vec) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = vec[k];
}

// [[Rcpp::export]]
void replaceVec(RObject xpbm,
                const NumericVector& elemInd,
                RObject vec) {

  XPtr<FBM> xpBM(xpbm);
  return replaceVec(VecBMAcc<double>(xpBM, elemInd - 1),
                    as<NumericVector>(vec));
}

/******************************************************************************/

template <class C, typename T>
void replaceMatOne(C macc, T val) {

  for (size_t j = 0; j < macc.ncol(); j++)
    for (size_t i = 0; i < macc.nrow(); i++)
      macc(i, j) = val;
}

// [[Rcpp::export]]
void replaceMatOne(RObject xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   RObject val) {

  XPtr<FBM> xpBM(xpbm);
  return replaceMatOne(SubBMAcc<double>(xpBM, rowInd - 1, colInd - 1),
                       as<double>(val));
}

/******************************************************************************/

template <class C, int RTYPE>
void replaceMat(C macc, const Matrix<RTYPE>& mat) {

  for (size_t j = 0; j < macc.ncol(); j++)
    for (size_t i = 0; i < macc.nrow(); i++)
      macc(i, j) = mat(i, j);
}

// [[Rcpp::export]]
void replaceMat(RObject xpbm,
                const IntegerVector& rowInd,
                const IntegerVector& colInd,
                RObject mat) {

  XPtr<FBM> xpBM(xpbm);
  return replaceMat(SubBMAcc<double>(xpBM, rowInd - 1, colInd - 1),
                    as<NumericMatrix>(mat));
}

/******************************************************************************/
