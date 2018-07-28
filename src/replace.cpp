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

  size_t size() const { return Vector<RTYPE>::size(); }

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

  // inline double operator[](size_t k) {
  //   double x_k = Matrix<RTYPE>::operator[](k);
  //   if (x_k == NA_INTEGER) x_k = NA_REAL;
  //   return x_k;
  // }

  size_t nrow() const { return Matrix<RTYPE>::nrow(); }
  size_t ncol() const { return Matrix<RTYPE>::ncol(); }
  // size_t size() const { return Matrix<RTYPE>::size(); }

};

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
      LogicalVector vec2 = check_conv<LGLSXP,  unsigned char>(VEC);            \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    case INTSXP:  {                                                            \
      IntegerVector vec2 = check_conv<INTSXP,  unsigned char>(VEC);            \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    case REALSXP: {                                                            \
      NumericVector vec2 = check_conv<REALSXP, unsigned char>(VEC);            \
      REPLACE(unsigned char, vec2)                                             \
    }                                                                          \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  case 2:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(unsigned short, as<RawVector>(VEC))                  \
    case LGLSXP:  {                                                            \
      LogicalVector vec2 = check_conv<LGLSXP,  unsigned short>(VEC);           \
      REPLACE(unsigned short, vec2)                                            \
    }                                                                          \
    case INTSXP:  {                                                            \
      IntegerVector vec2 = check_conv<INTSXP,  unsigned short>(VEC);           \
      REPLACE(unsigned short, vec2)                                            \
    }                                                                          \
    case REALSXP: {                                                            \
      NumericVector vec2 = check_conv<REALSXP, unsigned short>(VEC);           \
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
      NumericVector vec2 = check_conv_dbl2int(VEC);                            \
      REPLACE(int, vec2)                                                       \
    }                                                                          \
    default: stop("This R type is not supported.");                            \
    }                                                                          \
  case 8:                                                                      \
    switch(TYPEOF(VEC)) {                                                      \
    case RAWSXP:  REPLACE(double, as<RawVector>(VEC))                          \
    case LGLSXP: {                                                             \
      if (Rf_isMatrix(VEC)) {                                                  \
        Int2NumMatrix<LGLSXP> vec2(VEC);                                       \
        REPLACE(double, vec2)                                                  \
      } else {                                                                 \
        Int2NumVector<LGLSXP> vec2(VEC);                                       \
        REPLACE(double, vec2)                                                  \
      }                                                                        \
    }                                                                          \
    case INTSXP: {                                                             \
      if (Rf_isMatrix(VEC)) {                                                  \
        Int2NumMatrix<INTSXP> vec2(VEC);                                       \
        REPLACE(double, vec2)                                                  \
      } else {                                                                 \
        Int2NumVector<INTSXP> vec2(VEC);                                       \
        REPLACE(double, vec2)                                                  \
      }                                                                        \
    }                                                                          \
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
void replaceVecOne(VecBMAcc<BM_TYPE> macc, T val) {

  size_t K = macc.nelem();
  for (size_t k = 0; k < K; k++)
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

template <int RTYPE>
void replace_vec(VecBMAcc<double> macc, Int2NumVector<RTYPE>& vec) {

  size_t K = macc.nelem();
  for (size_t k = 0; k < K; k++)
    macc[k] = vec[k];
}

template <typename BM_TYPE, int RTYPE>
void replace_vec(VecBMAcc<BM_TYPE> macc, const Vector<RTYPE>& vec) {

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

template <typename BM_TYPE, typename T>
void replaceMatOne(SubBMAcc<BM_TYPE> macc, T val) {

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
