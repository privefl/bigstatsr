#ifndef UTILS_H
#define UTILS_H

// also defined in R/utils.R
const char* const ERROR_TYPE = "unknown type detected for big.matrix object!";
const char* const ERROR_DIM = "incompatibility between dimensions";
const char* const ERROR_SUB =
  "you can't use this function on a sub.big.matrix!";

inline void myassert(bool cond, const char *msg) {
  if (!cond) throw Rcpp::exception(msg);
}

#endif // UTILS_H
