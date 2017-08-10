#ifndef SUBMATCOVACC_H
#define SUBMATCOVACC_H

/******************************************************************************/

#include "SubMatAcc.h"

/******************************************************************************/

#define SUBMATCOVACC(T) SubMatCovAcc<T>(*xpMat, rows, covar)
#define RAWSUBMATCOVACC RawSubMatCovAcc(*xpMat, rows, covar, BM.slot("code"))

#define DISPATCH_SUBMATCOVACC(CALL) {                                             \
                                                                               \
  XPtr<BigMatrix> xpMat = BM.slot("address");                                  \
  IntegerVector rows = row_idx - 1;                                            \
                                                                               \
  if (Rf_inherits(BM, "BM.code")) {                                            \
    CALL(RAWSUBMATCOVACC);                                                     \
  } else {                                                                     \
    switch(xpMat->matrix_type()) {                                             \
    case 1:                                                                    \
      CALL(SUBMATCOVACC(char))                                                 \
    case 2:                                                                    \
      CALL(SUBMATCOVACC(short))                                                \
    case 4:                                                                    \
      CALL(SUBMATCOVACC(int))                                                  \
    case 6:                                                                    \
      CALL(SUBMATCOVACC(float))                                                \
    case 8:                                                                    \
      CALL(SUBMATCOVACC(double))                                               \
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
  SubMatCovAcc(BigMatrix& bm,
               const IntegerVector& row_ind,
               const NumericMatrix& covar) {
    if (bm.is_submatrix()) throw Rcpp::exception(ERROR_SUB);

    if (covar.nrow() != 0) {
      myassert(row_ind.length() == covar.nrow(), ERROR_DIM);
      _ncoladd = covar.ncol();
      _covar = covar;
    }  else {
      _ncoladd = 0;
    }

    int n = row_ind.size();
    std::vector<index_type> row_ind2(n);
    for (int i = 0; i < n; i++)
      row_ind2[i] = static_cast<index_type>(row_ind[i]);

    _pMat = reinterpret_cast<T*>(bm.matrix());
    _totalRows = bm.total_rows();
    _row_ind = row_ind2;
    _nrow = row_ind.size();
    _ncolBM = bm.ncol();
  }

  inline double operator() (int i, int j) {
    if (j < _ncolBM) {
      return *(_pMat + _totalRows * j + _row_ind[i]);
    } else {
      return _covar(i, j - _ncolBM);
    }
  }

  int nrow() const {
    return _nrow;
  }

  int ncol() const {
    return _ncolBM + _ncoladd;
  }

protected:
  T *_pMat;
  index_type _totalRows;
  int _nrow;
  int _ncolBM;
  int _ncoladd;
  std::vector<index_type> _row_ind;
  NumericMatrix _covar;
};

/******************************************************************************/

class RawSubMatCovAcc : public SubMatCovAcc<unsigned char> {
public:
  RawSubMatCovAcc(BigMatrix& bm,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar,
                  const NumericVector& lookup)
    : SubMatCovAcc<unsigned char>(bm, row_ind, covar) {
      _lookup = lookup;
    }

  inline double operator() (int i, int j) {
    if (j < _ncolBM) {
      return _lookup[*(_pMat + _totalRows * j + _row_ind[i])];
    } else {
      return _covar(i, j - _ncolBM);
    }
  }

protected:
  NumericVector _lookup;
};

/******************************************************************************/

#endif // SUBMATCOVACC_H
