#ifndef TYPES_H
#define TYPES_H

#include <bigstatsr/utils.h>

#define DISPATCH_TYPE(CALL) {                                                  \
  switch(type) {                                                               \
  case 1:                                                                      \
    CALL(unsigned char)                                                        \
  case 2:                                                                      \
    CALL(unsigned short)                                                       \
  case 4:                                                                      \
    CALL(int)                                                                  \
  case 8:                                                                      \
    CALL(double)                                                               \
  default:                                                                     \
    throw Rcpp::exception(ERROR_TYPE);                                         \
  }                                                                            \
}

#endif // TYPES_H
