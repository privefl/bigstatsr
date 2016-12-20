#ifndef UTILS_H
#define UTILS_H

// [[Rcpp::depends(bigmemory, BH, RcppArmadillo, RcppEigen)]]
#include <RcppArmadillo.h> // Sys.setenv("PKG_LIBS" = "-llapack")
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;

// also defined in R/utils.R
const char* const ERROR_TYPE =
  "unknown type detected for big.matrix object!";

#endif // UTILS_H
