/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/utils.h>
#include <bigstatsr/types.h>
#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// Class for reading integers (and logicals) as doubles
template <int RTYPE>
class Int2NumVector : public Vector<RTYPE> {
public:
  Int2NumVector(SEXP x) : Vector<RTYPE>(x) {}

  inline double operator[](size_t k) {
    double x_k = Vector<RTYPE>::operator[](k);
    if (x_k == NA_INTEGER) x_k = NA_REAL;
    return x_k;
  }
};

template <int RTYPE>
class Int2NumMatrix : public Matrix<RTYPE> {
public:
  Int2NumMatrix(SEXP x) : Matrix<RTYPE>(x) {}

  inline double operator()(size_t i, size_t j) {
    double x_ij = Matrix<RTYPE>::operator()(i, j);
    if (x_ij == NA_INTEGER) x_ij = NA_REAL;
    return x_ij;
  }

  inline double operator[](size_t k) {
    double x_k = Matrix<RTYPE>::operator[](k);
    if (x_k == NA_INTEGER) x_k = NA_REAL;
    return x_k;
  }
};

/******************************************************************************/

#define DISPATCH_REPLACE(REPLACE, VEC) {                                       \
                                                                               \
XPtr<FBM> xpBM(xpbm);                                                          \
int fbm_type = xpBM->matrix_type();                                            \
                                                                               \
DISPATCH_REPLACE2(REPLACE, VEC)                                                \
}

#define DISPATCH_REPLACE2(REPLACE, VEC) {                                      \
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
  case RAWSXP:  REPLACE(unsigned short, as<RawVector>(VEC))                    \
  case LGLSXP:  {                                                              \
    LogicalVector vec2 = check_conv<LGLSXP,  unsigned short>(VEC);             \
    REPLACE(unsigned short, vec2)                                              \
  }                                                                            \
  case INTSXP:  {                                                              \
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
  case RAWSXP:  REPLACE(int, as<RawVector>(VEC))                               \
  case LGLSXP:  REPLACE(int, as<LogicalVector>(VEC))                           \
  case INTSXP:  REPLACE(int, as<IntegerVector>(VEC))                           \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv_dbl2int(VEC);                              \
    REPLACE(int, vec2)                                                         \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 6:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP:  REPLACE(float, as<RawVector>(VEC))                             \
  case LGLSXP:  {                                                              \
    LogicalVector vec2 = check_conv<LGLSXP,  float>(VEC);                      \
    REPLACE(float, vec2)                                                       \
  }                                                                            \
  case INTSXP:  {                                                              \
    IntegerVector vec2 = check_conv<INTSXP,  float>(VEC);                      \
    REPLACE(float, vec2)                                                       \
  }                                                                            \
  case REALSXP: {                                                              \
    NumericVector vec2 = check_conv<REALSXP, float>(VEC);                      \
    REPLACE(float, vec2)                                                       \
  }                                                                            \
  default: stop("R type '%s' is not supported.", Rf_type2char(r_type));        \
  }                                                                            \
case 8:                                                                        \
  switch(r_type) {                                                             \
  case RAWSXP:  REPLACE(double, as<RawVector>(VEC))                            \
  case LGLSXP: {                                                               \
    if (Rf_isMatrix(VEC)) {                                                    \
      Int2NumMatrix<LGLSXP> vec2(VEC);                                         \
      REPLACE(double, vec2)                                                    \
    } else {                                                                   \
      Int2NumVector<LGLSXP> vec2(VEC);                                         \
      REPLACE(double, vec2)                                                    \
    }                                                                          \
  }                                                                            \
  case INTSXP: {                                                               \
    if (Rf_isMatrix(VEC)) {                                                    \
      Int2NumMatrix<INTSXP> vec2(VEC);                                         \
      REPLACE(double, vec2)                                                    \
    } else {                                                                   \
      Int2NumVector<INTSXP> vec2(VEC);                                         \
      REPLACE(double, vec2)                                                    \
    }                                                                          \
  }                                                                            \
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

/******************************************************************************/

template <typename BM_TYPE, typename T>
void replace_vec_one(VecBMAcc<BM_TYPE> macc, T val) {

  size_t K = macc.size();
  for (size_t k = 0; k < K; k++)
    macc[k] = val;
}

// https://stackoverflow.com/a/1726777/6103040
#define REPLACE_VEC_ONE(BM_TYPE, VEC) {                                        \
return replace_vec_one(VecBMAcc<BM_TYPE>(xpBM, elemInd - 1),                   \
                       VEC[static_cast<size_t>(0)]);                           \
}

// [[Rcpp::export]]
void replaceVecOne(SEXP xpbm,
                   const NumericVector& elemInd,
                   SEXP val) {

  myassert(!Rf_isMatrix(val), ERROR_REPORT);

  DISPATCH_REPLACE(REPLACE_VEC_ONE, val)
}

/******************************************************************************/

// C can be Vector<RTYPE>, Int2NumVector<RTYPE> or Int2NumMatrix<RTYPE>
template <typename BM_TYPE, class C>
void replace_vec(VecBMAcc<BM_TYPE> macc, C vec) {

  size_t K = macc.size();
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

template <typename BM_TYPE, typename T>
void replace_mat_one(SubBMAcc<BM_TYPE> macc, T val) {

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = val;
}

// https://stackoverflow.com/a/1726777/6103040
#define REPLACE_MAT_ONE(BM_TYPE, VEC) {                                        \
return replace_mat_one(SubBMAcc<BM_TYPE>(xpBM, rowInd - 1, colInd - 1),        \
                       VEC[static_cast<size_t>(0)]);                           \
}

// [[Rcpp::export]]
void replaceMatOne(SEXP xpbm,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd,
                   SEXP val) {

  myassert(!Rf_isMatrix(val), ERROR_REPORT);

  DISPATCH_REPLACE(REPLACE_MAT_ONE, val)
}

/******************************************************************************/

template <int RTYPE>
void replace_mat(SubBMAcc<double> macc, Int2NumMatrix<RTYPE>& mat) {

  size_t i, j, n = macc.nrow(), m = macc.ncol();
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      macc(i, j) = mat(i, j);
}

template <typename BM_TYPE, int RTYPE>
void replace_mat(SubBMAcc<BM_TYPE> macc, const Vector<RTYPE>& vec) {

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

// C can be Vector<RTYPE> or Int2NumVector<RTYPE>
template <typename BM_TYPE, class C>
void replace_col(SubBMAcc<BM_TYPE> macc_j, size_t n, C vec) {

  for (size_t i = 0; i < n; i++)
    macc_j(i, 0) = vec[i];
}

#define REPLACE_COL(BM_TYPE, VEC) {                                            \
replace_col(SubBMAcc<BM_TYPE>(xpBM, row_ind, col_ind), n, VEC);                \
continue;                                                                      \
}

// [[Rcpp::export]]
void replaceDF(SEXP xpbm,
               const IntegerVector& rowInd,
               const IntegerVector& colInd,
               const DataFrame& df) {

  XPtr<FBM> xpBM(xpbm);
  int fbm_type = xpBM->matrix_type();

  size_t n = rowInd.size(), m = colInd.size();
  IntegerVector row_ind = rowInd - 1;
  IntegerVector col_ind(1);

  for (size_t j = 0; j < m; j++) {
    SEXP col = df[j];
    col_ind[0] = colInd[j] - 1;
    DISPATCH_REPLACE2(REPLACE_COL, col)
  }
}

/******************************************************************************/
