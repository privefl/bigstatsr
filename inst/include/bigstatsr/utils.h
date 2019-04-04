#ifndef UTILS_H
#define UTILS_H

/******************************************************************************/

#include <mio/mmap.hpp>
#include <RcppArmadillo.h>
#include <Rcpp.h>

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

/******************************************************************************/

inline void myassert(bool cond, const char *msg) {
  if (!cond) Rcpp::stop(msg);
}

inline void myassert_bounds(std::size_t ind, std::size_t lim) {
  if (!(ind < lim)) Rcpp::stop("Tested %s < %s. %s", ind, lim, ERROR_BOUNDS);
}

/******************************************************************************/

#endif // UTILS_H
