#ifndef BM_CODE_ACC_H
#define BM_CODE_ACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

class BMCode256Acc : public BMAcc<unsigned char> {
public:
  BMCode256Acc(FBM * xpBM, const NumericVector& code256)
    : BMAcc<unsigned char>(xpBM) {
      _code256 = code256;
    }

  inline double operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[BMAcc<unsigned char>::operator()(i, j)];
  }

  inline double operator[](size_t k) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[BMAcc<unsigned char>::operator[](k)];
  }

protected:
  NumericVector _code256;
};

/******************************************************************************/

class SubBMCode256Acc : public SubBMAcc<unsigned char> {
public:
  SubBMCode256Acc(FBM * xpBM,
                  const IntegerVector& row_ind,
                  const IntegerVector& col_ind,
                  const NumericVector& code256,
                  int sub = 0)
    : SubBMAcc<unsigned char>(xpBM, row_ind, col_ind, sub) {
      _code256 = code256;
    }

  inline double operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[SubBMAcc<unsigned char>::operator()(i, j)];
  }

  // WARNING: operator[] is not redefined

protected:
  NumericVector _code256;
};

/******************************************************************************/

// For biglasso
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

#endif // BM_CODE_ACC_H
