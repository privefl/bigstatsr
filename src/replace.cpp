/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/utils.h>
#include <bigstatsr/types.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define USHORT_MIN 0
#define USHORT_MAX 65535

const IntegerVector& checkShort(const IntegerVector& vec) {

  size_t K = vec.size();

  for (size_t k = 0; k < K; k++)
    myassert(vec[k] >= USHORT_MIN && vec[k] <= USHORT_MAX, ERROR_SHORT);

  return vec;
}

const IntegerMatrix& checkShort(const IntegerMatrix& mat) {

  size_t K = mat.size();

  for (size_t k = 0; k < K; k++)
    myassert(mat[k] >= USHORT_MIN && mat[k] <= USHORT_MAX, ERROR_SHORT);

  return mat;
}

/******************************************************************************/

template <class C, typename T>
void replaceVecOne(C macc, T val) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = val;
}

#define REPLACE_VEC_ONE(TYPE) {                                                \
  return replaceVecOne(VecBMAcc<TYPE>(xpBM, elemInd - 1), as<TYPE>(val));      \
}

// [[Rcpp::export]]
void replaceVecOne(RObject xpbm,
                   const NumericVector& elemInd,
                   RObject val) {

  XPtr<FBM> xpBM(xpbm);
  int type = xpBM->matrix_type();

  DISPATCH_TYPE(REPLACE_VEC_ONE)
}

/******************************************************************************/

template <class C, int RTYPE>
void replaceVec(C macc, const Vector<RTYPE>& vec) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = vec[k];
}

#define REPLACE_VEC(BM_TYPE, VEC) {                                            \
  return replaceVec(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1), VEC);                \
}

// [[Rcpp::export]]
void replaceVec(RObject xpbm,
                const NumericVector& elemInd,
                RObject vec) {

  XPtr<FBM> xpBM(xpbm);

  switch(xpBM->matrix_type()) {
  case 1:
    REPLACE_VEC(unsigned char,  as<RawVector>(vec))
  case 2:
    REPLACE_VEC(unsigned short, checkShort(as<IntegerVector>(vec)))
  case 4:
    REPLACE_VEC(int,            as<IntegerVector>(vec))
  case 8:
    REPLACE_VEC(double,         as<NumericVector>(vec))
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/

template <class C, typename T>
void replaceMatOne(C macc, T val) {

  for (size_t j = 0; j < macc.ncol(); j++)
    for (size_t i = 0; i < macc.nrow(); i++)
      macc(i, j) = val;
}

#define REPLACE_MAT_ONE(TYPE) {                                                \
  return replaceMatOne(SubBMAcc<TYPE>(xpBM, rowInd - 1, colInd - 1),           \
                       as<TYPE>(val));                                         \
}

// [[Rcpp::export]]
void replaceMatOne(RObject xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   RObject val) {

  XPtr<FBM> xpBM(xpbm);
  int type = xpBM->matrix_type();

  DISPATCH_TYPE(REPLACE_MAT_ONE)
}

/******************************************************************************/

template <typename T, int RTYPE>
void replaceMat(SubBMAcc<T> macc, const Matrix<RTYPE>& mat) {

  for (size_t j = 0; j < macc.ncol(); j++)
    for (size_t i = 0; i < macc.nrow(); i++)
      macc(i, j) = mat(i, j);
}

#define REPLACE_MAT(BM_TYPE, MAT) {                                            \
  return replaceMat(SubBMAcc<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), MAT);     \
}

// [[Rcpp::export]]
void replaceMat(RObject xpbm,
                const IntegerVector& rowInd,
                const IntegerVector& colInd,
                RObject mat) {

  XPtr<FBM> xpBM(xpbm);

  switch(xpBM->matrix_type()) {
  case 1:
    REPLACE_MAT(unsigned char,  as<RawMatrix>(mat))
  case 2:
    REPLACE_MAT(unsigned short, checkShort(as<IntegerMatrix>(mat)))
  case 4:
    REPLACE_MAT(int,            as<IntegerMatrix>(mat))
  case 8:
    REPLACE_MAT(double,         as<NumericMatrix>(mat))
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
