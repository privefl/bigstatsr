/******************************************************************************/

#include <bigstatsr/BMAcc.h>
#include <bigstatsr/types.h>

using std::size_t;

/******************************************************************************/

#define TRANSPOSE_STOP_SIZE 64

// recursive divide and conquer function
template <typename T>
void transpose3_rec(BMAcc_RW<T> macc,
                    BMAcc<T> macc2,
                    size_t i_min, size_t i_max,
                    size_t j_min, size_t j_max) {

  if ((j_max - j_min) > TRANSPOSE_STOP_SIZE) {
    size_t j_mid = (j_min + j_max) / 2;
    transpose3_rec(macc, macc2, i_min, i_max, j_min, j_mid);
    transpose3_rec(macc, macc2, i_min, i_max, j_mid, j_max);
  } else {
    if ((i_max - i_min) > TRANSPOSE_STOP_SIZE) {
      size_t i_mid = (i_min + i_max) / 2;
      transpose3_rec(macc, macc2, i_min, i_mid, j_min, j_max);
      transpose3_rec(macc, macc2, i_mid, i_max, j_min, j_max);
    } else {
      size_t i, j;
      for (j = j_min; j < j_max; j++) {
        for (i = i_min; i < i_max; i++) {
          macc(i, j) = macc2(j, i);
        }
      }
    }
  }
}

template <typename T>
void transpose3(BMAcc_RW<T> macc, BMAcc<T> macc2) {
  transpose3_rec(macc, macc2, 0, macc.nrow(), 0, macc.ncol());
}

/******************************************************************************/

#define TRANSPOSE(T) return transpose3(BMAcc_RW<T>(xpBM), BMAcc<T>(xpBM2));

// Dispatch function for transpose3
// [[Rcpp::export]]
void transpose3(Environment BM, Environment BM2) {

  XPtr<FBM_RW> xpBM = BM["address_rw"];
  XPtr<FBM> xpBM2 = BM2["address"];

  int type = xpBM->matrix_type();
  DISPATCH_TYPE(TRANSPOSE)
}

/******************************************************************************/
