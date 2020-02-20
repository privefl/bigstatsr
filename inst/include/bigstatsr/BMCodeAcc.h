#ifndef BM_CODE_ACC_H
#define BM_CODE_ACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>

using namespace Rcpp;

/******************************************************************************/

class BMCode256Acc : public BMAcc<unsigned char> {
public:
  BMCode256Acc(FBM * xpBM, const NumericVector& code256)
    : BMAcc<unsigned char>(xpBM) {
      _code256 = code256;
    }

  inline double operator()(std::size_t i, std::size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[BMAcc<unsigned char>::operator()(i, j)];
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

  inline double operator()(std::size_t i, std::size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return _code256[SubBMAcc<unsigned char>::operator()(i, j)];
  }

protected:
  NumericVector _code256;
};

/******************************************************************************/

#define DISPATCH_ACC(CALL, ACC, RAWACC) {                                      \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                              \
                                                                               \
  if (BM.exists("code256")) {                                                  \
    CALL(RAWACC);                                                              \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(ACC(double))                                                        \
    case 4:                                                                    \
      CALL(ACC(int))                                                           \
    case 6:                                                                    \
      CALL(ACC(float))                                                         \
    case 1:                                                                    \
      CALL(ACC(unsigned char))                                                 \
    case 2:                                                                    \
      CALL(ACC(unsigned short))                                                \
    default:                                                                   \
      throw Rcpp::exception(ERROR_TYPE);                                       \
    }                                                                          \
  }                                                                            \
}

/******************************************************************************/

#define MATACC(T) BMAcc<T>(xpBM)
#define RAWMATACC BMCode256Acc(xpBM, BM["code256"])

#define DISPATCH_MATACC(CALL) DISPATCH_ACC(CALL, MATACC, RAWMATACC)

#define SUBMATACC(T) SubBMAcc<T>(xpBM, rowInd, colInd, 1)
#define RAWSUBMATACC SubBMCode256Acc(xpBM, rowInd, colInd, BM["code256"], 1)

#define DISPATCH_SUBMATACC(CALL) DISPATCH_ACC(CALL, SUBMATACC, RAWSUBMATACC)

/******************************************************************************/

#endif // BM_CODE_ACC_H
