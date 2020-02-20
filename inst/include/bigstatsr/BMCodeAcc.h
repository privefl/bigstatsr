#ifndef BM_CODE_ACC_H
#define BM_CODE_ACC_H

/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/utils.h>

using namespace Rcpp;

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

#define SUBMATACC(T) SubBMAcc<T>(xpBM, rowInd, colInd, 1)
#define RAWSUBMATACC SubBMCode256Acc(xpBM, rowInd, colInd, BM["code256"], 1)

#define DISPATCH_SUBMATACC(CALL) {                                             \
                                                                               \
  XPtr<FBM> xpBM = BM["address"];                                              \
                                                                               \
  if (BM.exists("code256")) {                                                  \
    CALL(RAWSUBMATACC);                                                        \
  } else {                                                                     \
    switch(xpBM->matrix_type()) {                                              \
    case 8:                                                                    \
      CALL(SUBMATACC(double))                                                  \
    case 4:                                                                    \
      CALL(SUBMATACC(int))                                                     \
    case 6:                                                                    \
      CALL(SUBMATACC(float))                                                   \
    case 1:                                                                    \
      CALL(SUBMATACC(unsigned char))                                           \
    case 2:                                                                    \
      CALL(SUBMATACC(unsigned short))                                          \
    default:                                                                   \
      throw Rcpp::exception(ERROR_TYPE);                                       \
    }                                                                          \
  }                                                                            \
}

/******************************************************************************/

#endif // BM_CODE_ACC_H
