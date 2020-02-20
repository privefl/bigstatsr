#ifndef SUBMATCOVACC_H
#define SUBMATCOVACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define SUBMATCOVACC(T)     SubMatCovAcc<T>(xpBM, row_idx,     col_idx, covar,     1)
#define SUBMATCOVACC_VAL(T) SubMatCovAcc<T>(xpBM, row_idx_val, col_idx, covar_val, 1)

#define RAWSUBMATCOVACC     RawSubMatCovAcc(xpBM, row_idx,     col_idx, covar,     BM["code256"], 1)
#define RAWSUBMATCOVACC_VAL RawSubMatCovAcc(xpBM, row_idx_val, col_idx, covar_val, BM["code256"], 1)


#define DISPATCH_SUBMATCOVACC_VAL(CALL) {                                      \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                              \
                                                                               \
  if (BM.exists("code256")) {                                                  \
    CALL(RAWSUBMATCOVACC, RAWSUBMATCOVACC_VAL);                                \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(SUBMATCOVACC(double),         SUBMATCOVACC_VAL(double))             \
    case 4:                                                                    \
      CALL(SUBMATCOVACC(int),            SUBMATCOVACC_VAL(int))                \
    case 6:                                                                    \
      CALL(SUBMATCOVACC(float),          SUBMATCOVACC_VAL(float))              \
    case 1:                                                                    \
      CALL(SUBMATCOVACC(unsigned char),  SUBMATCOVACC_VAL(unsigned char))      \
    case 2:                                                                    \
      CALL(SUBMATCOVACC(unsigned short), SUBMATCOVACC_VAL(unsigned short))     \
    default:                                                                   \
      throw Rcpp::exception(ERROR_TYPE);                                       \
    }                                                                          \
  }                                                                            \
}

#define DISPATCH_SUBMATCOVACC(CALL) {                                          \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                              \
                                                                               \
  if (BM.exists("code256")) {                                                  \
    CALL(RAWSUBMATCOVACC);                                                     \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(SUBMATCOVACC(double))                                               \
    case 4:                                                                    \
      CALL(SUBMATCOVACC(int))                                                  \
    case 6:                                                                    \
      CALL(SUBMATCOVACC(float))                                                \
    case 1:                                                                    \
      CALL(SUBMATCOVACC(unsigned char))                                        \
    case 2:                                                                    \
      CALL(SUBMATCOVACC(unsigned short))                                       \
    default:                                                                   \
      throw Rcpp::exception(ERROR_TYPE);                                       \
    }                                                                          \
  }                                                                            \
}

/******************************************************************************/

// For biglasso
template<typename T>
class SubMatCovAcc : public SubBMAcc<T> {
public:
  SubMatCovAcc(FBM * xpBM,
               const IntegerVector& row_ind,
               const IntegerVector& col_ind,
               const NumericMatrix& covar,
               int sub = 0)
    : SubBMAcc<T>(xpBM, row_ind, col_ind, sub) {

    _ncolsub = col_ind.size();

    if (covar.nrow() != 0) {
      myassert_size(row_ind.size(), covar.nrow());
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
  RawSubMatCovAcc(FBM * xpBM,
                  const IntegerVector& row_ind,
                  const IntegerVector& col_ind,
                  const NumericMatrix& covar,
                  const NumericVector& code256,
                  int sub = 0)
    : SubMatCovAcc<unsigned char>(xpBM, row_ind, col_ind, covar, sub) {
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
