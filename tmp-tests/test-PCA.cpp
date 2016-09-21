// [[Rcpp::depends(RcppEigen, BH, bigmemory)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix crossprodEigen(const Eigen::Map<Eigen::MatrixXd> X,
                    const Eigen::Map<Eigen::MatrixXd> Y) {
  return(X.transpose() * Y);
}
