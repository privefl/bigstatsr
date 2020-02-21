#ifndef BM_ACC_DISPATCH_H
#define BM_ACC_DISPATCH_H

/******************************************************************************/

#include <bigstatsr/BMCodeAcc.h>

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

#define SUBMATCOVACC(T)     SubMatCovAcc<T>(xpBM, row_idx,     col_idx, covar,     1)
#define SUBMATCOVACC_VAL(T) SubMatCovAcc<T>(xpBM, row_idx_val, col_idx, covar_val, 1)

#define RAWSUBMATCOVACC     RawSubMatCovAcc(xpBM, row_idx,     col_idx, covar,     BM["code256"], 1)
#define RAWSUBMATCOVACC_VAL RawSubMatCovAcc(xpBM, row_idx_val, col_idx, covar_val, BM["code256"], 1)

#define DISPATCH_SUBMATCOVACC(CALL) DISPATCH_ACC(CALL, SUBMATCOVACC, RAWSUBMATCOVACC)


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

/******************************************************************************/

#endif // BM_ACC_DISPATCH_H
