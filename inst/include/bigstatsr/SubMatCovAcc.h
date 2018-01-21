#ifndef SUBMATCOVACC_H
#define SUBMATCOVACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define SUBMATCOVACC(T)     SubMatCovAcc<T>(xpBM, rows,     cols, covar)
#define SUBMATCOVACC_VAL(T) SubMatCovAcc<T>(xpBM, rows_val, cols, covar_val)

#define RAWSUBMATCOVACC     RawSubMatCovAcc(xpBM, rows,     cols, covar,     BM["code256"])
#define RAWSUBMATCOVACC_VAL RawSubMatCovAcc(xpBM, rows_val, cols, covar_val, BM["code256"])


#define DISPATCH_SUBMATCOVACC_VAL(CALL) {                                      \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                              \
  IntegerVector rows = row_idx - 1;                                            \
  IntegerVector cols = col_idx - 1;                                            \
  IntegerVector rows_val = row_idx_val - 1;                                    \
                                                                               \
  if (BM.exists("code256")) {                                                  \
    CALL(RAWSUBMATCOVACC, RAWSUBMATCOVACC_VAL);                                \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(SUBMATCOVACC(double), SUBMATCOVACC_VAL(double))                     \
    case 4:                                                                    \
      CALL(SUBMATCOVACC(int), SUBMATCOVACC_VAL(int))                           \
    case 1:                                                                    \
      CALL(SUBMATCOVACC(unsigned char), SUBMATCOVACC_VAL(unsigned char))       \
    case 2:                                                                    \
      CALL(SUBMATCOVACC(unsigned short), SUBMATCOVACC_VAL(unsigned short))     \
    default:                                                                   \
      throw Rcpp::exception(ERROR_TYPE);                                       \
    }                                                                          \
  }                                                                            \
}

#define DISPATCH_SUBMATCOVACC(CALL) {                                          \
                                                                               \
XPtr<FBM> xpBM = BM["address"];                                                \
IntegerVector rows = row_idx - 1;                                              \
IntegerVector cols = col_idx - 1;                                              \
                                                                               \
if (BM.exists("code256")) {                                                    \
  CALL(RAWSUBMATCOVACC);                                                       \
} else {                                                                       \
  switch(xpBM->matrix_type()) {                                                \
  case 8:                                                                      \
    CALL(SUBMATCOVACC(double))                                                 \
  case 4:                                                                      \
    CALL(SUBMATCOVACC(int))                                                    \
  case 1:                                                                      \
    CALL(SUBMATCOVACC(unsigned char))                                          \
  case 2:                                                                      \
    CALL(SUBMATCOVACC(unsigned short))                                         \
  default:                                                                     \
    throw Rcpp::exception(ERROR_TYPE);                                         \
  }                                                                            \
}                                                                              \
}

/******************************************************************************/

// For biglasso
template<typename T>
class SubMatCovAcc : public SubBMAcc<T> {
public:
  SubMatCovAcc(const FBM * xpBM,
               const IntegerVector& row_ind,
               const IntegerVector& col_ind,
               const NumericMatrix& covar)
    : SubBMAcc<T>(xpBM, row_ind, col_ind) {

    _ncolsub = col_ind.size();

    if (covar.nrow() != 0) {
      myassert(row_ind.size() == covar.nrow(), ERROR_DIM);
      _ncoladd = covar.ncol();
      _covar = covar;
    }  else {
      _ncoladd = 0;
    }
  }

  inline double operator() (size_t i, size_t j) {
    int j2 = j - _ncolsub;
    if (j2 < 0) {
      // https://stackoverflow.com/a/32087373/6103040
      return SubBMAcc<T>::operator()(i, j);
      // https://stackoverflow.com/a/7076312/6103040
      // return this->_pMat[_row_ind[i] + _col_ind[j] * this->_nrow];
    } else {
      return _covar(i, j2);
    }
  }

  size_t nrow() const { return this->_row_ind.size(); }
  size_t ncol() const { return _ncolsub + _ncoladd; }

protected:
  size_t _ncolsub;
  size_t _ncoladd;
  NumericMatrix _covar;
};

/******************************************************************************/

class RawSubMatCovAcc : public SubMatCovAcc<unsigned char> {
public:
  RawSubMatCovAcc(const FBM * xpBM,
                  const IntegerVector& row_ind,
                  const IntegerVector& col_ind,
                  const NumericMatrix& covar,
                  const NumericVector& code256)
    : SubMatCovAcc<unsigned char>(xpBM, row_ind, col_ind, covar) {
      _code256 = code256;
    }

  inline double operator() (size_t i, size_t j) {
    int j2 = j - this->_ncolsub;
    if (j2 < 0) {
      // https://stackoverflow.com/a/32087373/6103040
      return _code256[SubMatCovAcc<unsigned char>::operator()(i, j)];;
    } else {
      return _covar(i, j2);
    }
  }

protected:
  NumericVector _code256;
};

/******************************************************************************/

#endif // SUBMATCOVACC_H
