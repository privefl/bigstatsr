/******************************************************************************/

#ifndef SUBINTMATCOVACC_H
#define SUBINTMATCOVACC_H

#include "SubMatCovAcc.h"

/******************************************************************************/

// For sparseSVM
template<typename T>
class SubIntMatCovAcc : public SubMatCovAcc<T> {
public:
  SubIntMatCovAcc(BigMatrix& bm,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar)
    : SubMatCovAcc<T>(bm, row_ind, covar) {}

  inline double operator() (int i, int j) {
    return j == 0 ? 1.0 : SubMatCovAcc<T>::operator()(i, j-1);
  }

  int ncol() const {
    return 1 + SubMatCovAcc<T>::ncol();
  }
};

/******************************************************************************/

class RawSubIntMatCovAcc : public RawSubMatCovAcc {
public:
  RawSubIntMatCovAcc(BigMatrix& bm,
                     const IntegerVector& row_ind,
                     const NumericMatrix& covar,
                     const NumericVector& lookup)
    : RawSubMatCovAcc(bm, row_ind, covar, lookup) {}

  inline double operator() (int i, int j) {
    return j == 0 ? 1.0 : RawSubMatCovAcc::operator()(i, j-1);
  }

  int ncol() const {
    return 1 + RawSubMatCovAcc::ncol();
  }
};

/******************************************************************************/

#endif // SUBINTMATCOVACC_H

/******************************************************************************/
