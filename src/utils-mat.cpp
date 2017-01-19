// [[Rcpp::depends(RcppEigen, bigmemory, BH)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
Eigen::MatrixXd multEigen(const Eigen::Map<Eigen::MatrixXd> X,
                          const Eigen::Map<Eigen::MatrixXd> Y) {
  return X * Y;
}

/******************************************************************************/

// [[Rcpp::export]]
Eigen::MatrixXd crossprodEigen5(const Eigen::Map<Eigen::MatrixXd> X,
                                const Eigen::Map<Eigen::MatrixXd> Y) {
  return X.transpose() * Y;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& scaling(NumericMatrix& source,
                       const NumericVector& mean,
                       const NumericVector& sd) {
  int n = source.rows();
  int m = source.cols();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      source(i,j) -= mean[j];
      source(i,j) /= sd[j];
    }
  }

  return(source);
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& complete2(NumericMatrix& mat) {
  for (int j = 0; j < mat.ncol()-1; j++) {
    for (int i = j+1; i < mat.nrow(); i++) {
      mat(i, j) = mat(j, i);
    }
  }

  return mat;
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& incrSup2(NumericMatrix& mat, const NumericMatrix& source) {
  for (int j = 0; j < mat.ncol(); j++) {
    for (int i = 0; i <= j; i++) {
      mat(i, j) += source(i,j);
    }
  }

  return mat;
}

/******************************************************************************/

// [[Rcpp::export]]
void tcrossprodEigen3(Eigen::Map<Eigen::MatrixXd> res,
                      const Eigen::Map<Eigen::MatrixXd> bM) {
  res.selfadjointView<Eigen::Upper>().rankUpdate(bM);
}

/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& incrMat(NumericMatrix& dest, const NumericMatrix& source) {
  dest += source;

  return dest;
}

/******************************************************************************/
