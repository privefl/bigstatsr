#ifndef SUBMATCOVACC_H
#define SUBMATCOVACC_H

/******************************************************************************/

#include <bigstatsr/FBM.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

#define SUBMATCOVACC(T) SubMatCovAcc<T>(xpBM, rows, covar)
#define RAWSUBMATCOVACC RawSubMatCovAcc(xpBM, rows, covar, BM["code256"])

#define DISPATCH_SUBMATCOVACC(CALL) {                                          \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                             \
  IntegerVector rows = row_idx - 1;                                            \
                                                                               \
  if (BM.exists("code256")) {                                                 \
    CALL(RAWSUBMATCOVACC);                                                     \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(SUBMATCOVACC(double))                                               \
    case 4:                                                                    \
      CALL(SUBMATCOVACC(int))                                                  \
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
class SubMatCovAcc {
public:
  SubMatCovAcc(const FBM * xpBM,
               const IntegerVector& row_ind,
               const NumericMatrix& covar) {

    if (covar.nrow() != 0) {
      myassert(row_ind.length() == covar.nrow(), ERROR_DIM);
      _ncoladd = covar.ncol();
      _covar = covar;
    }  else {
      _ncoladd = 0;
    }

    size_t n = row_ind.size();
    std::vector<size_t> row_ind2(n);
    for (size_t i = 0; i < n; i++)
      row_ind2[i] = static_cast<size_t>(row_ind[i]);
    _row_ind = row_ind2;

    _pMat = static_cast<T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline double operator() (size_t i, size_t j) {
    if (j < _ncol) {
      return _pMat[_row_ind[i] + j * _nrow];
    } else {
      return _covar(i, j - _ncol);
    }
  }

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _ncol + _ncoladd; }

protected:
  T *_pMat;
  size_t _nrow;
  size_t _ncol;
  std::vector<size_t> _row_ind;
  size_t _ncoladd;
  NumericMatrix _covar;
};

/******************************************************************************/

class RawSubMatCovAcc : public SubMatCovAcc<unsigned char> {
public:
  RawSubMatCovAcc(const FBM * xpBM,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar,
                  const NumericVector& code256)
    : SubMatCovAcc<unsigned char>(xpBM, row_ind, covar) {
      _code256 = code256;
    }

  inline double operator() (size_t i, size_t j) {
    if (j < _ncol) {
      // https://stackoverflow.com/a/32087373/6103040
      return _code256[SubMatCovAcc<unsigned char>::operator()(i, j)];;
    } else {
      return _covar(i, j - _ncol);
    }
  }

protected:
  NumericVector _code256;
};

/******************************************************************************/

#endif // SUBMATCOVACC_H
