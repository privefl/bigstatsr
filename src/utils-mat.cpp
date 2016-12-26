// [[Rcpp::depends(RcppEigen, bigmemory, BH)]]
#include <RcppEigen.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
NumericMatrix& scaling2(NumericMatrix& source,
                       const NumericVector& sd) {
  int n = source.rows();
  int m = source.cols();

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      source(i, j) /= sd[i];
    }
  }

  return(source);
}

// [[Rcpp::export]]
NumericMatrix& scaling3(NumericMatrix& source,
                        const NumericVector& mean) {
  int n = source.rows();
  int m = source.cols();

  double mj;

  for (int j = 0; j < m; j++) {
    mj = mean[j];
    for (int i = 0; i < n; i++) {
      source(i, j) -= mj;
    }
  }

  return(source);
}

/******************************************************************************/

// typedef Eigen::Matrix<char, Eigen::Dynamic, Eigen::Dynamic> MatrixXchar;
//
// // [[Rcpp::export]]
// Eigen::VectorXd produ(SEXP xpMat, const Eigen::Map<Eigen::VectorXd> x) {
//
//   XPtr<BigMatrix> bMPtr(xpMat);
//
//   // won't work with a sub.big.matrix
//   Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(),
//                              bMPtr->nrow(), bMPtr->ncol());
//
//   return bM.cast<double>() * x;
// }

// [[Rcpp::export]]
NumericVector produ2(SEXP xpMat, const NumericVector& x) {

  XPtr<BigMatrix> bMPtr(xpMat);
  MatrixAccessor<char> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(n);
  double xj;
  int i, j;

  for (j = 0; j < m; j++) {
    xj = x[j];
    for (i = 0; i < n; i++) {
      res[i] += macc[j][i] * xj;
    }
  }

  return res;
}

/******************************************************************************/

// // [[Rcpp::export]]
// Eigen::VectorXd crossprodu(SEXP xpMat, const Eigen::Map<Eigen::VectorXd> x) {
//
//   XPtr<BigMatrix> bMPtr(xpMat);
//
//   // won't work with a sub.big.matrix
//   Eigen::Map<MatrixXchar> bM((char *)bMPtr->matrix(),
//                              bMPtr->nrow(), bMPtr->ncol());
//
//   return bM.transpose().cast<double>() * x;
// }

// [[Rcpp::export]]
NumericVector crossprodu2(SEXP xpMat, const NumericVector& x) {

  XPtr<BigMatrix> bMPtr(xpMat);
  MatrixAccessor<char> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(m);
  double tmp;
  int i, j;

  for (j = 0; j < m; j++) {
    tmp = 0;
    for (i = 0; i < n; i++) {
      tmp += macc[j][i] * x[i];
    }
    res[j] = tmp;
  }

  return res;
}

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

  return(dest);
}

/******************************************************************************/

// // [[Rcpp::export]]
// void incrG(SEXP pBigMat, const NumericMatrix& source,
//            double n, double offset, double L, double m) {
//   XPtr<BigMatrix> xpMat(pBigMat);
//   MatrixAccessor<double> macc(*xpMat);
//
//   for (int j = 0; j < L; j++) {
//     for (int i = 0; i < n; i++) {
//       macc[j + offset][i] += source(i, j) / m;
//     }
//   }
//
//   return;
// }

/******************************************************************************/
