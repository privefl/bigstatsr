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
  int i, j;
  double xj;

  for (j = 0; j < m; j++) {
    xj = x[j];
    for (i = 0; i < n; i++) {
      res[i] += xj * macc[j][i];
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericVector produ3(SEXP xpMat, const NumericVector& x) {

  XPtr<BigMatrix> bMPtr(xpMat);
  MatrixAccessor<char> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(n);
  int i, j;

  for (j = 0; j <= m - 16; j += 16) {
    for (i = 0; i < n; i++) { // unrolling optimization
      res[i] += (((x[j] * macc[j][i] + x[j+1] * macc[j+1][i]) +
        (x[j+2] * macc[j+2][i] + x[j+3] * macc[j+3][i])) +
        ((x[j+4] * macc[j+4][i] + x[j+5] * macc[j+5][i]) +
        (x[j+6] * macc[j+6][i] + x[j+7] * macc[j+7][i]))) +
        (((x[j+8] * macc[j+8][i] + x[j+9] * macc[j+9][i]) +
        (x[j+10] * macc[j+10][i] + x[j+11] * macc[j+11][i])) +
        ((x[j+12] * macc[j+12][i] + x[j+13] * macc[j+13][i]) +
        (x[j+14] * macc[j+14][i] + x[j+15] * macc[j+15][i])));
    }
  }
  for (; j < m; j++) {
    for (i = 0; i < n; i++) {
      res[i] += x[j] * macc[j][i];
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

// [[Rcpp::export]]
NumericVector crossprodu3(SEXP xpMat, const NumericVector& x) {

  XPtr<BigMatrix> bMPtr(xpMat);
  MatrixAccessor<char> macc(*bMPtr);

  int n = bMPtr->nrow();
  int m = bMPtr->ncol();

  NumericVector res(m);
  double tmp;
  int i, j;

  for (j = 0; j < m; j++) {
    tmp = 0;
    for (i = 0; i <= n - 16; i += 16) { // unrolling optimization
      tmp += (((macc[j][i] * x[i] + macc[j][i+1] * x[i+1]) +
        (macc[j][i+2] * x[i+2] + macc[j][i+3] * x[i+3])) +
        ((macc[j][i+4] * x[i+4] + macc[j][i+5] * x[i+5]) +
        (macc[j][i+6] * x[i+6] + macc[j][i+7] * x[i+7]))) +
        (((macc[j][i+8] * x[i+8] + macc[j][i+9] * x[i+9]) +
        (macc[j][i+10] * x[i+10] + macc[j][i+11] * x[i+11])) +
        ((macc[j][i+12] * x[i+12] + macc[j][i+13] * x[i+13]) +
        (macc[j][i+14] * x[i+14] + macc[j][i+15] * x[i+15])));
    }
    for (; i < n; i++) {
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
