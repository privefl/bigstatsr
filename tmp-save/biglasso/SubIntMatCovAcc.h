#ifndef SUBINTMATCOVACC_H
#define SUBINTMATCOVACC_H

/******************************************************************************/

#include <bigstatsr/SubMatCovAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

// For sparseSVM
template<typename T>
class SubIntMatCovAcc : public SubMatCovAcc<T> {
public:
  SubIntMatCovAcc(const FBM * xpBM,
                  const IntegerVector& row_ind,
                  const NumericMatrix& covar)
    : SubMatCovAcc<T>(xpBM, row_ind, covar) {}

  inline double operator() (size_t i, size_t j) {
    return j == 0 ? 1.0 : SubMatCovAcc<T>::operator()(i, j - 1);
  }

  size_t ncol() const {
    return 1 + SubMatCovAcc<T>::ncol();
  }
};

/******************************************************************************/

class RawSubIntMatCovAcc : public RawSubMatCovAcc {
public:
  RawSubIntMatCovAcc(const FBM * xpBM,
                     const IntegerVector& row_ind,
                     const NumericMatrix& covar,
                     const NumericVector& code256)
    : RawSubMatCovAcc(xpBM, row_ind, covar, code256) {}

  inline double operator() (size_t i, size_t j) {
    return j == 0 ? 1.0 : RawSubMatCovAcc::operator()(i, j - 1);
  }

  size_t ncol() const {
    return 1 + RawSubMatCovAcc::ncol();
  }
};

/******************************************************************************/

#endif // SUBINTMATCOVACC_H
