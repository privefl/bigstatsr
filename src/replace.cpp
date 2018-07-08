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
    myassert(vec[k] >= USHORT_MIN && vec[k] <= USHORT_MAX, ERROR_USHORT);

  return vec;
}

const IntegerMatrix& checkShort(const IntegerMatrix& mat) {

  size_t K = mat.size();

  for (size_t k = 0; k < K; k++)
    myassert(mat[k] >= USHORT_MIN && mat[k] <= USHORT_MAX, ERROR_USHORT);

  return mat;
}

/******************************************************************************/

std::string int2name(int RTYPE) {

  switch(RTYPE) {
  case 10: return "logical";
  case 13: return "integer";
  case 14: return "double";
  case 24: return "raw";
  default: return "unknown";
  }
}

template<typename CTYPE>
std::string type2name() {

  double x = CTYPE(1.5);
  if (x == 1.5) return "double";

  int y = CTYPE(-1);
  if (y == -1) return "integer";

  unsigned short z = CTYPE(256);
  if (z == 256) {
    return "unsigned short";
  } else {
    return "unsigned char (raw)";
  }
}

template <int RTYPE_IN, int RTYPE_OUT, typename CTYPE>
Vector<RTYPE_OUT> conv_vec(Vector<RTYPE_IN> nv) {

  int i, n = nv.size();
  Vector<RTYPE_OUT> res(n);
  res.attr("dim") = nv.attr("dim");

  for (i = 0; i < n; i++) {
    res[i] = CTYPE(nv[i]);
    if (nv[i] != res[i]) {
      std::string CTYPE_name = type2name<CTYPE>();
      double res_i = res[i];
      if (CTYPE_name == "unsigned char (raw)") res_i = int(res_i); // printing

      // warning("%s (%s -> %s)\n  %s from R type '%s' to C type '%s'.",
      //         "At least one value changed", nv[i], res_i,
      //         "while converting", int2name(RTYPE_IN), CTYPE_name);
      break;
    }
  }
  for (; i < n; i++) res[i] = CTYPE(nv[i]);

  return res;
}

/******************************************************************************/

template <class C, typename T>
void replaceVecOne(C macc, T val) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = val;
}

#define REPLACE_VEC_ONE(TYPE) {                                                \
return replaceVecOne(VecBMAcc<TYPE>(xpBM, elemInd - 1), as<TYPE>(val));        \
}

// [[Rcpp::export]]
void replaceVecOne(SEXP xpbm,
                   const NumericVector& elemInd,
                   SEXP val) {

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
return replaceVec(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1), VEC);                  \
}

// [[Rcpp::export]]
void replaceVec(SEXP xpbm,
                const NumericVector& elemInd,
                SEXP vec) {

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
return replaceMatOne(SubBMAcc<TYPE>(xpBM, rowInd - 1, colInd - 1),             \
                     as<TYPE>(val));                                           \
}

// [[Rcpp::export]]
void replaceMatOne(SEXP xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   SEXP val) {

  XPtr<FBM> xpBM(xpbm);
  int type = xpBM->matrix_type();

  DISPATCH_TYPE(REPLACE_MAT_ONE)
}

/******************************************************************************/

template <typename T, int RTYPE>
void replace_mat(SubBMAcc<T> macc, const Vector<RTYPE>& vec) {

  Matrix<RTYPE> mat(vec);

  for (size_t j = 0; j < macc.ncol(); j++)
    for (size_t i = 0; i < macc.nrow(); i++)
      macc(i, j) = mat(i, j);
}

#define REPLACE_MAT(BM_TYPE, MAT) {                                             \
return replace_mat(SubBMAcc<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), MAT);       \
}

// [[Rcpp::export]]
void replaceMat(SEXP xpbm,
                const IntegerVector& rowInd,
                const IntegerVector& colInd,
                SEXP mat) {

  XPtr<FBM> xpBM(xpbm);

  switch(xpBM->matrix_type()) {
  case 1:
    switch(TYPEOF(mat)) {
    case RAWSXP:  REPLACE_MAT(unsigned char, as<RawVector>(mat))
    case LGLSXP:  {
      RawVector mat2 = conv_vec<LGLSXP, RAWSXP, unsigned char>(mat);
      REPLACE_MAT(unsigned char, mat2)
    }
    case INTSXP:  {
      RawVector mat2 = conv_vec<INTSXP, RAWSXP, unsigned char>(mat);
      REPLACE_MAT(unsigned char, mat2)
    }
    case REALSXP: {
      RawVector mat2 = conv_vec<REALSXP, RAWSXP, unsigned char>(mat);
      REPLACE_MAT(unsigned char, mat2)
    }
    default: stop("This R type is not supported.");
    }
  case 2:
    switch(TYPEOF(mat)) {
    case RAWSXP:  REPLACE_MAT(unsigned short, as<RawVector>(mat))
    case LGLSXP:  {
      IntegerVector mat2 = conv_vec<LGLSXP, INTSXP, unsigned short>(mat);
      REPLACE_MAT(unsigned short, mat2)
    }
    case INTSXP:  {
      IntegerVector mat2 = conv_vec<INTSXP, INTSXP, unsigned short>(mat);
      REPLACE_MAT(unsigned short, mat2)
    }
    case REALSXP: {
      IntegerVector mat2 = conv_vec<REALSXP, INTSXP, unsigned short>(mat);
      REPLACE_MAT(unsigned short, mat2)
    }
    default: stop("This R type is not supported.");
    }
  case 4:
    switch(TYPEOF(mat)) {
    case RAWSXP:  REPLACE_MAT(int, as<RawVector>(mat))
    case LGLSXP:  REPLACE_MAT(int, as<LogicalVector>(mat))
    case INTSXP:  REPLACE_MAT(int, as<IntegerVector>(mat))
    case REALSXP: {
      IntegerVector mat2 = conv_vec<REALSXP, INTSXP, int>(mat);
      REPLACE_MAT(int, mat2)
    }
    default: stop("This R type is not supported.");
    }
  case 8:
    switch(TYPEOF(mat)) {
    case RAWSXP:  REPLACE_MAT(double, as<RawVector>(mat))
    case LGLSXP:  REPLACE_MAT(double, as<LogicalVector>(mat))
    case INTSXP:  REPLACE_MAT(double, as<IntegerVector>(mat))
    case REALSXP: REPLACE_MAT(double, as<NumericVector>(mat))
    default: stop("This R type is not supported.");
    }
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
