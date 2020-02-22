/******************************************************************************/

#include <bigstatsr/BMCodeAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// [[Rcpp::export]]
NumericVector& conv_NA_float(NumericVector& source) {

  size_t n = source.size();
  for (size_t i = 0; i < n; i++) {
    if (source[i] == NA_FLOAT) source[i] = NA_REAL;
  }

  return source;
}

/******************************************************************************/

template <int RTYPE, class C>
Vector<RTYPE> _extract_vec(C macc, const NumericVector& elemInd) {

  size_t K = elemInd.size();

  Vector<RTYPE> res(K);

  for (size_t k = 0; k < K; k++)
    res[k] = macc[elemInd[k] - 1];

  return res;
}

#define EXTRACT_VEC(BM_TYPE, RTYPE) {                                          \
  return _extract_vec<RTYPE>(BMAcc<BM_TYPE>(xpBM), elemInd);                   \
}

// [[Rcpp::export]]
RObject extractVec(Environment BM,
                   const NumericVector& elemInd) {

  XPtr<FBM> xpBM = BM["address"];

  if (BM.exists("code256")) {
    return _extract_vec<REALSXP>(BMCode256Acc(xpBM, BM["code256"]), elemInd);
  } else {
    switch(xpBM->matrix_type()) {
    case 1:
      EXTRACT_VEC(unsigned char,  RAWSXP)
    case 2:
      EXTRACT_VEC(unsigned short, INTSXP)
    case 4:
      EXTRACT_VEC(int,            INTSXP)
    case 8:
      EXTRACT_VEC(double,         REALSXP)
    case 6: {
        NumericVector res = _extract_vec<REALSXP>(BMAcc<float>(xpBM), elemInd);
        return conv_NA_float(res);
      }
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/

template <int RTYPE, class C>
Matrix<RTYPE> _extract_mat(C macc) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();

  Matrix<RTYPE> res(n, m);

  for (size_t j = 0; j < m; j++)
    for (size_t i = 0; i < n; i++)
      res(i, j) = macc(i, j);

  return res;
}

#define EXTRACT_MAT(BM_TYPE, RTYPE) {                                          \
  return _extract_mat<RTYPE>(SubBMAcc<BM_TYPE>(xpBM, rowInd, colInd, 1));      \
}

// [[Rcpp::export]]
RObject extractMat(Environment BM,
                   const IntegerVector& rowInd,
                   const IntegerVector& colInd) {

  XPtr<FBM> xpBM = BM["address"];

  if (BM.exists("code256")) {
    return _extract_mat<REALSXP>(
      SubBMCode256Acc(xpBM, rowInd, colInd, BM["code256"], 1));
  } else {
    switch(xpBM->matrix_type()) {
    case 1:
      EXTRACT_MAT(unsigned char,  RAWSXP)
    case 2:
      EXTRACT_MAT(unsigned short, INTSXP)
    case 4:
      EXTRACT_MAT(int,            INTSXP)
    case 8:
      EXTRACT_MAT(double,         REALSXP)
    case 6: {
        NumericMatrix res = _extract_mat<REALSXP>(
          SubBMAcc<float>(xpBM, rowInd, colInd, 1));
        return conv_NA_float(res);
      }
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
