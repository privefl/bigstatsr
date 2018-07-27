/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/utils.h>
#include <bigstatsr/types.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define DISPATCH_REPLACE(REPLACE, VEC) {                                       \
                                                                               \
  XPtr<FBM> xpBM(xpbm);                                                        \
  int fbm_type = xpBM->matrix_type();                                          \
                                                                               \
  switch(fbm_type) {                                                           \
  case 1:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(unsigned char, as<RawVector>(VEC))                   \
    case LGLSXP:  {                                                            \
      RawVector vec2 = conv_vec<LGLSXP, RAWSXP, unsigned char>(VEC);           \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    case INTSXP:  {                                                            \
      RawVector vec2 = conv_vec<INTSXP, RAWSXP, unsigned char>(VEC);           \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    case REALSXP: {                                                            \
      RawVector vec2 = conv_vec<REALSXP, RAWSXP, unsigned char>(VEC);          \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  case 2:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(unsigned short, as<RawVector>(VEC))                  \
    case LGLSXP:  {                                                            \
      IntegerVector vec2 = conv_vec<LGLSXP, INTSXP, unsigned short>(VEC);      \
      REPLACE(unsigned short, vec2)                                            \
    }                                                                          \
    case INTSXP:  {                                                            \
      IntegerVector vec2 = conv_vec<INTSXP, INTSXP, unsigned short>(VEC);      \
      REPLACE(unsigned short, vec2)                                            \
    }                                                                          \
    case REALSXP: {                                                            \
      IntegerVector vec2 = conv_vec<REALSXP, INTSXP, unsigned short>(VEC);     \
      REPLACE(unsigned short, vec2)                                            \
    }                                                                          \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  case 4:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(int, as<RawVector>(VEC))                             \
    case LGLSXP:  REPLACE(int, as<LogicalVector>(VEC))                         \
    case INTSXP:  REPLACE(int, as<IntegerVector>(VEC))                         \
    case REALSXP: {                                                            \
      IntegerVector vec2 = conv_vec_dbl2int(VEC);                              \
      REPLACE(int, vec2)                                                       \
    }                                                                          \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  case 8:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(double, as<RawVector>(VEC))                          \
    case LGLSXP:  REPLACE(double, as<NumericVector>(VEC))                      \
    case INTSXP:  REPLACE(double, as<NumericVector>(VEC))                      \
    case REALSXP: REPLACE(double, as<NumericVector>(VEC))                      \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  default:                                                                     \
    throw Rcpp::exception(ERROR_TYPE);                                         \
  }                                                                            \
}

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
      if (res[i] != nv[i]) {
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

IntegerVector conv_vec_dbl2int(NumericVector nv) {

  int i = 0, n = nv.size();
  IntegerVector res(n);
  res.attr("dim") = nv.attr("dim");

  if (do_warn_downcast()) {

    for (; i < n; i++) {
      res[i] = nv[i];
      if (res[i] != nv[i] && !R_IsNA(nv[i])) {
        warning("%s (%s -> %s)\n  %s",
                "At least one value changed", nv[i], res[i],
                "while converting from R type 'double' to C type 'integer'.");
        break;
      }
    }
  }

  for (; i < n; i++) res[i] = nv[i];

  return res;
}

/******************************************************************************/

template <class C, typename T>
void replaceVecOne(C macc, T val) {

  for (size_t k = 0; k < macc.nelem(); k++)
    macc[k] = val;
}

#define REPLACE_VEC_ONE(BM_TYPE, VEC) {                                        \
return replaceVecOne(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1), VEC[0]);            \
}

// [[Rcpp::export]]
void replaceVecOne(SEXP xpbm,
                   const NumericVector& elemInd,
                   SEXP val) {

  DISPATCH_REPLACE(REPLACE_VEC_ONE, val)
}

/******************************************************************************/

template <class C, int RTYPE>
void replace_vec(C macc, const Vector<RTYPE>& vec) {

  size_t K = macc.nelem();
  for (size_t k = 0; k < K; k++)
    macc[k] = vec[k];
}

#define REPLACE_VEC(BM_TYPE, VEC) {                                            \
return replace_vec(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1), VEC);                 \
}

// [[Rcpp::export]]
void replaceVec(SEXP xpbm,
                const NumericVector& elemInd,
                SEXP vec) {

  DISPATCH_REPLACE(REPLACE_VEC, vec)
}

/******************************************************************************/

template <class C, typename T>
void replaceMatOne(C macc, T val) {

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = val;
}

#define REPLACE_MAT_ONE(BM_TYPE, VEC) {                                        \
return replaceMatOne(SubBMAcc<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), VEC[0]); \
}

// [[Rcpp::export]]
void replaceMatOne(SEXP xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   SEXP val) {

  DISPATCH_REPLACE(REPLACE_MAT_ONE, val)
}

/******************************************************************************/

template <typename T, int RTYPE>
void replace_mat(SubBMAcc<T> macc, const Vector<RTYPE>& vec) {

  Matrix<RTYPE> mat(vec);

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = mat(i, j);
}

#define REPLACE_MAT(BM_TYPE, MAT) {                                            \
return replace_mat(SubBMAcc<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), MAT);      \
}

// [[Rcpp::export]]
void replaceMat(SEXP xpbm,
                const IntegerVector& rowInd,
                const IntegerVector& colInd,
                SEXP mat) {

  DISPATCH_REPLACE(REPLACE_MAT, mat)
}

/******************************************************************************/
