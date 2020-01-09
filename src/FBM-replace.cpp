/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

inline float int2flt(int x) {
  return (x == NA_INTEGER) ? NA_FLOAT : x;
}

inline float dbl2flt(double x) {
  return R_IsNA(x) ? NA_FLOAT : x;
}

inline double int2dbl(int x) {
  return (x == NA_INTEGER) ? NA_REAL : x;
}

template<typename T_IN, typename T_OUT>
inline T_OUT identity(T_IN x) {
  return x;
}

/******************************************************************************/

#define DISPATCH_REPLACE(REPLACE, VEC, REPLACE_CONV) {                         \
                                                                               \
  XPtr<FBM_RW> xpBM(xpbm);                                                     \
  int fbm_type = xpBM->matrix_type();                                          \
                                                                               \
  DISPATCH_REPLACE2(REPLACE, VEC, REPLACE_CONV)                                \
}

#define DISPATCH_REPLACE2(REPLACE, VEC, REPLACE_CONV) {                        \
                                                                               \
int r_type = TYPEOF(VEC);                                                      \
switch(fbm_type) {                                                             \
case 1:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP:  REPLACE(unsigned char, as<RawVector>(VEC))                     \
  case LGLSXP:  {                                                              \
    LogicalVector vec2 = check_conv<LGLSXP,  unsigned char>(VEC);              \
    REPLACE(unsigned char, vec2)                                               \
  }                                                                            \
  case INTSXP:  {                                                              \
    IntegerVector vec2 = check_conv<INTSXP,  unsigned char>(VEC);              \
    REPLACE(unsigned char, vec2)                                               \
  }                                                                            \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv<REALSXP, unsigned char>(VEC);              \
    REPLACE(unsigned char, vec2)                                               \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 2:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP: REPLACE(unsigned short, as<RawVector>(VEC))                     \
  case LGLSXP: {                                                               \
    LogicalVector vec2 = check_conv<LGLSXP,  unsigned short>(VEC);             \
    REPLACE(unsigned short, vec2)                                              \
  }                                                                            \
  case INTSXP: {                                                               \
    IntegerVector vec2 = check_conv<INTSXP,  unsigned short>(VEC);             \
    REPLACE(unsigned short, vec2)                                              \
  }                                                                            \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv<REALSXP, unsigned short>(VEC);             \
    REPLACE(unsigned short, vec2)                                              \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 4:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP: REPLACE(int, as<RawVector>(VEC))                                \
  case LGLSXP: REPLACE(int, as<LogicalVector>(VEC))                            \
  case INTSXP: REPLACE(int, as<IntegerVector>(VEC))                            \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv_dbl2int(VEC);                              \
    REPLACE(int, vec2)                                                         \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 6:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP: REPLACE(float, as<RawVector>(VEC))                              \
  case LGLSXP: REPLACE_CONV(float, as<LogicalVector>(VEC), int2flt)            \
  case INTSXP: {                                                               \
    IntegerVector vec2 = check_conv<INTSXP,  float>(VEC);                      \
    REPLACE_CONV(float, vec2, int2flt)                                         \
  }                                                                            \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv_dbl2flt(VEC);                              \
    REPLACE_CONV(float, vec2, dbl2flt)                                         \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 8:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP: REPLACE(double, as<RawVector>(VEC))                             \
  case LGLSXP: REPLACE_CONV(double, as<LogicalVector>(VEC), int2dbl)           \
  case INTSXP: REPLACE_CONV(double, as<IntegerVector>(VEC), int2dbl)           \
  case REALSXP: REPLACE(double, as<NumericVector>(VEC))                        \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
default:                                                                       \
  throw Rcpp::exception(ERROR_TYPE);                                           \
}                                                                              \
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

  double x = CTYPE(0.5);
  if (x == 0.5) {
    x = CTYPE(2147483647);
    if (x == 2147483647) {
      return "double";
    } else {
      return "float";
    }
  }

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

template <int RTYPE_IN, typename CTYPE>
Vector<RTYPE_IN> check_conv(Vector<RTYPE_IN> nv) {

  if (do_warn_downcast()) {

    size_t n = nv.size();
    CTYPE test;

    for (size_t i = 0; i < n; i++) {
      test = nv[i];
      if (test != nv[i]) {
        warning("%s (%s -> %s)\n  %s from R type '%s' to C type '%s'.",
                "At least one value changed", nv[i], double(test),
                "while converting", Rf_type2char(RTYPE_IN), type2name<CTYPE>());
        break;
      }
    }
  }

  return nv;
}

NumericVector check_conv_dbl2int(NumericVector nv) {

  if (do_warn_downcast()) {

    size_t n = nv.size();
    int test;

    for (size_t i = 0; i < n; i++) {
      test = nv[i];
      if (test != nv[i] && !R_IsNA(nv[i])) {
        warning("%s (%s -> %s)\n  %s",
                "At least one value changed", nv[i], test,
                "while converting from R type 'double' to C type 'integer'.");
        break;
      }
    }
  }

  return nv;
}

NumericVector check_conv_dbl2flt(NumericVector nv) {

  if (do_warn_downcast()) {

    size_t n = nv.size();
    float test;

    for (size_t i = 0; i < n; i++) {
      test = nv[i];
      if (test != nv[i] && !std::isnan(test)) {
        warning("%s (%s -> %s)\n  %s",
                "At least one value changed", nv[i], test,
                "while converting from R type 'double' to C type 'float'.");
        break;
      }
      if (test == NA_FLOAT) {
        warning("%s (%s -> %s)\n  %s",
                "At least one value changed", nv[i], "NA",
                "while converting from R type 'double' to FBM type 'float'.");
        break;
      }
    }
  }

  return nv;
}

/******************************************************************************/

template <typename BM_TYPE, typename CTYPE>
void replace_vec_one(BMAcc_RW<BM_TYPE> macc,
                     const NumericVector& elemInd,
                     CTYPE val,
                     BM_TYPE (*conv)(CTYPE) = identity) {

  BM_TYPE val_conv = conv(val);
  size_t K = elemInd.size();
  for (size_t k = 0; k < K; k++)
    macc[elemInd[k] - 1] = val_conv;
}

#define REPLACE_VEC_ONE(BM_TYPE, VEC) {                                        \
return replace_vec_one(BMAcc_RW<BM_TYPE>(xpBM), elemInd, VEC[0]);              \
}

#define REPLACE_VEC_ONE_CONV(BM_TYPE, VEC, CONV) {                             \
return replace_vec_one(BMAcc_RW<BM_TYPE>(xpBM), elemInd, VEC[0], CONV);        \
}

// [[Rcpp::export]]
void replaceVecOne(SEXP xpbm,
                   const NumericVector& elemInd,
                   SEXP val) {

  myassert(!Rf_isMatrix(val), ERROR_REPORT);

  DISPATCH_REPLACE(REPLACE_VEC_ONE, val, REPLACE_VEC_ONE_CONV)
}

/******************************************************************************/

template <typename BM_TYPE, int RTYPE>
void replace_vec(BMAcc_RW<BM_TYPE> macc,
                 const NumericVector& elemInd,
                 Vector<RTYPE> vec) {

  size_t K = elemInd.size();
  for (size_t k = 0; k < K; k++)
    macc[elemInd[k] - 1] = vec[k];
}

#define REPLACE_VEC(BM_TYPE, VEC) {                                            \
return replace_vec(BMAcc_RW<BM_TYPE>(xpBM), elemInd, VEC);                     \
}

template <typename BM_TYPE, int RTYPE, typename CTYPE>
void replace_vec_conv(BMAcc_RW<BM_TYPE> macc,
                      const NumericVector& elemInd,
                      Vector<RTYPE> vec,
                      BM_TYPE (*conv)(CTYPE)) {

  size_t K = elemInd.size();
  for (size_t k = 0; k < K; k++)
    macc[elemInd[k] - 1] = conv(vec[k]);
}

#define REPLACE_VEC_CONV(BM_TYPE, VEC, CONV) {                                 \
return replace_vec_conv(BMAcc_RW<BM_TYPE>(xpBM), elemInd, VEC, CONV);          \
}

// [[Rcpp::export]]
void replaceVec(SEXP xpbm,
                const NumericVector& elemInd,
                SEXP vec) {

  DISPATCH_REPLACE(REPLACE_VEC, vec, REPLACE_VEC_CONV)
}

/******************************************************************************/

template <typename BM_TYPE, typename CTYPE>
void replace_mat_one(SubBMAcc_RW<BM_TYPE> macc, CTYPE val,
                     BM_TYPE (*conv)(CTYPE) = identity) {

  BM_TYPE val_conv = conv(val);
  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = val_conv;
}

#define REPLACE_MAT_ONE(BM_TYPE, VEC) {                                        \
return replace_mat_one(SubBMAcc_RW<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), VEC[0]); \
}

#define REPLACE_MAT_ONE_CONV(BM_TYPE, VEC, CONV) {                             \
  SubBMAcc_RW<BM_TYPE> macc(xpBM, rowInd - 1, colInd - 1);                     \
  return replace_mat_one(macc, VEC[0], CONV);                                  \
}

// [[Rcpp::export]]
void replaceMatOne(SEXP xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   SEXP val) {

  myassert(!Rf_isMatrix(val), ERROR_REPORT);

  DISPATCH_REPLACE(REPLACE_MAT_ONE, val, REPLACE_MAT_ONE_CONV)
}

/******************************************************************************/

template <typename BM_TYPE, int RTYPE>
void replace_mat(SubBMAcc_RW<BM_TYPE> macc,
                 const Vector<RTYPE>& vec) {

  Matrix<RTYPE> mat(vec);

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = mat(i, j);
}

#define REPLACE_MAT(BM_TYPE, MAT) {                                            \
  return replace_mat(SubBMAcc_RW<BM_TYPE>(xpBM, rowInd - 1, colInd - 1), MAT); \
}

template <typename BM_TYPE, int RTYPE, typename CTYPE>
void replace_mat_conv(SubBMAcc_RW<BM_TYPE> macc,
                      const Vector<RTYPE>& vec,
                      BM_TYPE (*conv)(CTYPE)) {

  Matrix<RTYPE> mat(vec);

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = conv(mat(i, j));
}

#define REPLACE_MAT_CONV(BM_TYPE, MAT, CONV) {                                 \
  SubBMAcc_RW<BM_TYPE> macc(xpBM, rowInd - 1, colInd - 1);                     \
  return replace_mat_conv(macc, MAT, CONV);                                    \
}

// [[Rcpp::export]]
void replaceMat(SEXP xpbm,
                const IntegerVector& rowInd,
                const IntegerVector& colInd,
                SEXP mat) {

  DISPATCH_REPLACE(REPLACE_MAT, mat, REPLACE_MAT_CONV)
}

/******************************************************************************/

template <typename BM_TYPE, int RTYPE>
void replace_col(SubBMAcc_RW<BM_TYPE> macc_j, size_t n, Vector<RTYPE> vec) {

  for (size_t i = 0; i < n; i++)
    macc_j(i, 0) = vec[i];
}

#define REPLACE_COL(BM_TYPE, VEC) {                                            \
replace_col(SubBMAcc_RW<BM_TYPE>(xpBM, row_ind, col_ind), n, VEC);             \
continue;                                                                      \
}

template <typename BM_TYPE, int RTYPE, typename CTYPE>
void replace_col_conv(SubBMAcc_RW<BM_TYPE> macc_j, size_t n, Vector<RTYPE> vec,
                      BM_TYPE (*conv)(CTYPE)) {

  for (size_t i = 0; i < n; i++)
    macc_j(i, 0) = conv(vec[i]);
}

#define REPLACE_COL_CONV(BM_TYPE, VEC, CONV) {                                 \
replace_col_conv(SubBMAcc_RW<BM_TYPE>(xpBM, row_ind, col_ind), n, VEC, CONV);  \
continue;                                                                      \
}

// [[Rcpp::export]]
void replaceDF(SEXP xpbm,
               const IntegerVector& rowInd,
               const IntegerVector& colInd,
               const DataFrame& df) {

  XPtr<FBM_RW> xpBM(xpbm);
  int fbm_type = xpBM->matrix_type();

  size_t n = rowInd.size(), m = colInd.size();
  IntegerVector row_ind = rowInd - 1;
  IntegerVector col_ind(1);

  for (size_t j = 0; j < m; j++) {
    SEXP col = df[j];
    col_ind[0] = colInd[j] - 1;
    DISPATCH_REPLACE2(REPLACE_COL, col, REPLACE_COL_CONV)
  }
}

/******************************************************************************/
