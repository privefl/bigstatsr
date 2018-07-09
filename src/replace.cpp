/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/utils.h>
#include <bigstatsr/types.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

bool do_warn_downcast() {

  Environment base("package:base");
  Function get_option = base["getOption"];
  SEXP warn = get_option("bigstatsr.downcast.warning");

  if (TYPEOF(warn) == LGLSXP) {
    return as<LogicalVector>(warn)[0];
  } else {
    return true;  // but this shoud not happen
  }
}

/******************************************************************************/

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

/******************************************************************************/

template <int RTYPE_IN, int RTYPE_OUT, typename CTYPE>
Vector<RTYPE_OUT> conv_vec(Vector<RTYPE_IN> nv) {

  int i = 0, n = nv.size();
  Vector<RTYPE_OUT> res(n);
  res.attr("dim") = nv.attr("dim");

  if (do_warn_downcast()) {

    for (; i < n; i++) {
      res[i] = CTYPE(nv[i]);
      if (nv[i] != res[i]) {
        warning("%s (%s -> %s)\n  %s from R type '%s' to C type '%s'.",
                "At least one value changed", nv[i], double(res[i]),
                "while converting", Rf_type2char(RTYPE_IN), type2name<CTYPE>());
        break;
      }
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
void replace_vec(C macc, const Vector<RTYPE>& vec) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = vec[k];
}

#define REPLACE_VEC(BM_TYPE, VEC) {                                            \
return replace_vec(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1), VEC);                  \
}

// [[Rcpp::export]]
void replaceVec(SEXP xpbm,
                const NumericVector& elemInd,
                SEXP vec) {

  XPtr<FBM> xpBM(xpbm);

  switch(xpBM->matrix_type()) {
  case 1:
    switch(TYPEOF(vec)) {
    case RAWSXP:  REPLACE_VEC(unsigned char, as<RawVector>(vec))
    case LGLSXP:  {
      RawVector vec2 = conv_vec<LGLSXP, RAWSXP, unsigned char>(vec);
      REPLACE_VEC(unsigned char, vec2)
    }
    case INTSXP:  {
      RawVector vec2 = conv_vec<INTSXP, RAWSXP, unsigned char>(vec);
      REPLACE_VEC(unsigned char, vec2)
    }
    case REALSXP: {
      RawVector vec2 = conv_vec<REALSXP, RAWSXP, unsigned char>(vec);
      REPLACE_VEC(unsigned char, vec2)
    }
    default: stop("This R type is not supported.");
    }
  case 2:
    switch(TYPEOF(vec)) {
    case RAWSXP:  REPLACE_VEC(unsigned short, as<RawVector>(vec))
    case LGLSXP:  {
      IntegerVector vec2 = conv_vec<LGLSXP, INTSXP, unsigned short>(vec);
      REPLACE_VEC(unsigned short, vec2)
    }
    case INTSXP:  {
      IntegerVector vec2 = conv_vec<INTSXP, INTSXP, unsigned short>(vec);
      REPLACE_VEC(unsigned short, vec2)
    }
    case REALSXP: {
      IntegerVector vec2 = conv_vec<REALSXP, INTSXP, unsigned short>(vec);
      REPLACE_VEC(unsigned short, vec2)
    }
    default: stop("This R type is not supported.");
    }
  case 4:
    switch(TYPEOF(vec)) {
    case RAWSXP:  REPLACE_VEC(int, as<RawVector>(vec))
    case LGLSXP:  REPLACE_VEC(int, as<LogicalVector>(vec))
    case INTSXP:  REPLACE_VEC(int, as<IntegerVector>(vec))
    case REALSXP: {
      IntegerVector vec2 = conv_vec<REALSXP, INTSXP, int>(vec);
      REPLACE_VEC(int, vec2)
    }
    default: stop("This R type is not supported.");
    }
  case 8:
    switch(TYPEOF(vec)) {
    case RAWSXP:  REPLACE_VEC(double, as<RawVector>(vec))
    case LGLSXP:  REPLACE_VEC(double, as<LogicalVector>(vec))
    case INTSXP:  REPLACE_VEC(double, as<IntegerVector>(vec))
    case REALSXP: REPLACE_VEC(double, as<NumericVector>(vec))
    default: stop("This R type is not supported.");
    }
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
