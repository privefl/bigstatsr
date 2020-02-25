#ifndef UTILS_H
#define UTILS_H

/******************************************************************************/

#include <Rcpp.h>

/******************************************************************************/

#define DISPATCH_TYPE(CALL) {                                                  \
  switch(type) {                                                               \
  case 8:                                                                      \
    CALL(double)                                                               \
  case 4:                                                                      \
    CALL(int)                                                                  \
  case 6:                                                                      \
    CALL(float)                                                                \
  case 1:                                                                      \
    CALL(unsigned char)                                                        \
  case 2:                                                                      \
    CALL(unsigned short)                                                       \
  default:                                                                     \
    throw Rcpp::exception(ERROR_TYPE);                                         \
  }                                                                            \
}

/******************************************************************************/

const char* const ERROR_TYPE =
  "Unknown type detected for Filebacked Big Matrix.";
const char* const ERROR_DIM =
  "Incompatibility between dimensions.";
const char* const ERROR_BOUNDS =
  "Subscript out of bounds.";
// const char* const ERROR_USHORT =
//   "Try to fill an 'unsigned short' with a value outside [0:65535].";
const char* const ERROR_REPORT =
  "Please report this issue that shoud not have happened.";
const char* const ERROR_BUG =
  "This is a bug; please report it.";

/******************************************************************************/

inline void myassert(bool cond, const char *msg) {
  if (!cond) Rcpp::stop(msg);
}

inline void myassert_bounds(std::size_t ind, std::size_t lim) {
  if (!(ind < lim)) Rcpp::stop("Tested %s < %s. %s", ind, lim, ERROR_BOUNDS);
}

inline void myassert_size(std::size_t n1, std::size_t n2) {
  if (n1 != n2) Rcpp::stop("Tested %s == %s. %s", n1, n2, ERROR_DIM);
}

/******************************************************************************/

#endif // UTILS_H
